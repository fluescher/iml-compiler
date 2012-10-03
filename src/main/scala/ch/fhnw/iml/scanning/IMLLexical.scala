package ch.fhnw.iml.scanning

import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharArrayReader.EofCh

class IMLLexical extends Lexical with IMLTokens {

    def identChar = letter    
    
    override def whitespace : Parser[Any] = rep(whitespaceChar)
	
	override def token: Parser[Token] =
	    (   identChar ~ rep(identChar | digit) 	^^ { case first ~ last => chooseIdentToken(first :: last mkString "")}
	    |	'>' ~ '='							^^^ GreaterEqualsThan
	    |	'>'									^^^ GreaterThan
	    |	'<' ~ '='							^^^ LessEqualsThan
	    |	'<'									^^^ LessThan
	    |	':' ~ '='							^^^ Becomes 
	    |	':' 								^^^ TypeSeparator
	    |	'='									^^^	Equals
	    |	'/' ~ '='							^^^ NotEquals
	    |	'?'									^^^ Read
	    |	'!'									^^^ Print
	    |	'-'									^^^ Minus
	    |	'+'									^^^ Plus
	    |   ';'									^^ {s => Delimiter(s.toString)}
	    |	EofCh								^^^ EOF
	    )
	
	    
	def chooseIdentToken(name: String) : Token = name match {
        case "program" => Program
        case "endprogram" => EndProgram
        case "command" => Command
        case "var" => Var
        case "declaration" => Declaration
        case "while" => While
        case "do" => Do
        case "endwhile" => EndWhile
        case "if" => If
        case "then" => Then
        case "else" => Else
        case "endiif" => EndIf
        case other => Ident(other)
    }
}