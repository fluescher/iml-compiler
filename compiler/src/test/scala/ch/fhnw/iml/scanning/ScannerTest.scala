package ch.fhnw.iml.scanning

import scala.io.Source
import scala.util.parsing.input.Reader
import org.junit.Test
import org.junit.Assert

class ScannerTest {

    val lexer = new IMLLexical
    
    def main(args: Array[String]) {
    	val scanner = new lexer.Scanner(Source.fromFile("src/test/resources/first.iml").mkString)
    	printTokens(scanner.asInstanceOf[Reader[lexer.Token]])
    }

    def printTokens(tokens: Reader[lexer.Token]) {
        if (tokens.atEnd) return
        println(tokens.first)
        printTokens(tokens.rest)
    }

    @Test
    def testReadIntLiteral() {
        val scanner = new lexer.Scanner("123")
        
        Assert.assertEquals(lexer.IntLiteral(123), scanner.first);
    }
    
    @Test
    def testReadIdent() {
        val scanner = new lexer.Scanner("d1mD1")
        
        Assert.assertEquals(lexer.Ident("d1mD1"), scanner.first)
    }
    
    @Test
    def testReadBecomes() {
        val scanner = new lexer.Scanner(":=")
        
        Assert.assertEquals(lexer.Becomes, scanner.first)
    }
    
    @Test
    def testReadOrdinals() {
        val scanner = new lexer.Scanner("<")
        val both = new lexer.Scanner("<= <")
        
        Assert.assertEquals(lexer.LessEqualsThan, both.first)
        Assert.assertEquals(lexer.LessThan, both.rest.first)
        Assert.assertEquals(lexer.LessThan, scanner.first)
    }
}