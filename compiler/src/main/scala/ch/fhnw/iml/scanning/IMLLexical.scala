package ch.fhnw.iml.scanning

import scala.annotation.migration
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.matching.Regex

class IMLLexical extends Lexical with IMLTokens {
    private def identChar = letter;  
    
    override def whitespace : Parser[Any] = rep(whitespaceChar)

	override def token: Parser[Token] =
	    (	digit ~ rep(digit)					^^ { case first ~ last => IntLiteral((first::last mkString).toInt) }   
	    |   identChar ~ rep(identChar | digit) 	^^ { case first ~ last => chooseIdentToken(first::last mkString) }
	    | 	'('									^^^	LParen
	    |	')'									^^^ RParen
	    |	','									^^^	Comma
	    |   ';'									^^^ SemiColon
	    |	':' ~ '='							^^^ Becomes
	    |	':' 								^^^ Colon
	    |	'?'									^^^ QuestionMark
	    |	'!'									^^^ ExclamationMark
	    | 	'{'									^^^ LBrace
	    |	'}'									^^^ RBrace
	    
	    |	'*'									^^^ Times
	    |   '+'									^^^ Plus
	    |   '-'									^^^ Minus
	    
	    |	'='									^^^	Equals
	    |	'/' ~ '='							^^^ NotEquals
	    |	'>' ~ '='							^^^ GreaterEqualsThan
	    |	'>'									^^^ GreaterThan
	    |	'<' ~ '='							^^^ LessEqualsThan
	    |	'<'									^^^ LessThan
	    
	    |	EOI									^^^ EOF			
	    )
	
	/**
	 * Parse for the end of the input.
	 */
	private def EOI = new Parser[Token] {
        def apply(in: Input) = {
            if(in.atEnd) Success(EOF, in)
            else Failure("End of input expected", in)
        }
    }
    
	private def chooseIdentToken(name: String) : Token = name match {
        case "bool" => Bool
        case "call" => Call
        case "cand" => And
        case "const"=> Const
        case "copy" => Copy
        case "cor"	=> Or
        case "div"  => Div
        case "else" => Else
        case "false"=> False
        case "fun"	=> Fun
        case "global"=>Global
        case "if" 	=> If
        case "in"	=> In
        case "init" => Init
        case "inout"=> InOut
        case "int32"=> Int32
        case "local"=>Local
        case "mod" 	=> Mod
        case "not" 	=> Not
        case "out" 	=> Out
        case "proc" => Proc
        case "program" => Program
        case "ref" 	=> Ref
        case "returns" => Returns
        case "skip"	=> Skip
        case "true" => True
        case "var" 	=> Var
        case "while"=> While
        case other  => Ident(other)
    }
}