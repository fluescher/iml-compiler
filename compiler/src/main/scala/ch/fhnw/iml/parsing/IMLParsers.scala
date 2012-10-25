package ch.fhnw.iml.parsing

import scala.util.parsing.combinator.Parsers
import ch.fhnw.iml.scanning.IMLLexical
import ch.fhnw.iml.scanning.IMLTokens
import scala.util.parsing.combinator.syntactical.TokenParsers
import ch.fhnw.iml.scanning.IMLTokens
import scala.util.parsing.input.Reader

class IMLParsers extends TokenParsers {
    type Tokens = IMLTokens
    
    val lexical = new IMLLexical
    
    def parse(source : Reader[Char]) : AnyRef = {
        val scanner = new lexical.Scanner(source)
        
        return null;
    }
    
}