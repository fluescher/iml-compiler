package ch.fhnw.iml.scanning

import scala.io.Source
import scala.util.parsing.input.Reader
import org.junit.Test
import org.junit.Assert
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.input.CharArrayReader
import org.junit.Ignore
import ch.fhnw.iml.parsing.IMLParsers
import ch.fhnw.iml.parsing.IMLParsers
import scala.util.parsing.combinator.lexical.Lexical

class ParserTest {

    val lexer = new IMLLexical
    import lexer._
    
    def printTokens(tokens: Reader[Token]) {
        if (tokens.atEnd) return
        println(tokens.first)
        printTokens(tokens.rest)
    }
    
    @Test
    def testExprList() {
      val parser = new IMLParsers
      val scanner = new parser.lexical.Scanner("(1,2)")
      
      println(parser.phrase(parser.program)(scanner))
    }

}