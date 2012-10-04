package ch.fhnw.iml.scanning

import scala.io.Source
import scala.util.parsing.input.Reader
import org.junit.Test
import org.junit.Assert

class ScannerTest {

    val lexer = new IMLLexical
    import lexer._
    
    def main(args: Array[String]) {
    	val scanner = new Scanner(Source.fromFile("src/test/resources/first.iml").mkString)
    	printTokens(scanner.asInstanceOf[Reader[Token]])
    }

    def printTokens(tokens: Reader[Token]) {
        if (tokens.atEnd) return
        println(tokens.first)
        printTokens(tokens.rest)
    }

    @Test
    def testReadIntLiteral() {
        val scanner = new Scanner("123")
        
        Assert.assertEquals(IntLiteral(123), scanner.first);
    }
    
    @Test
    def testReadIdent() {
        val scanner = new Scanner("d1mD1")
        
        Assert.assertEquals(Ident("d1mD1"), scanner.first)
    }
    
    @Test
    def testReadBecomes() {
        val scanner = new Scanner(":=")
        
        Assert.assertEquals(Becomes, scanner.first)
    }
    
    @Test
    def testReadOrdinals() {
        val scanner = new Scanner("<")
        val both = new Scanner("<= <")
        
        Assert.assertEquals(LessEqualsThan, both.first)
        Assert.assertEquals(LessThan, both.rest.first)
        Assert.assertEquals(LessThan, scanner.first)
    }
}