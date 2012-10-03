package ch.fhnw.iml.scanning

import scala.io.Source
import scala.util.parsing.input.Reader
import org.junit.Test

class ScannerTest extends IMLTokens {

    def main(args: Array[String]) {
    	val lexer = new IMLLexical
    	val scanner = new lexer.Scanner(Source.fromFile("src/test/resources/first.iml").mkString)
    	printTokens(scanner.asInstanceOf[Reader[Token]])
    }

    def printTokens(tokens: Reader[Token]) {
        if (tokens.atEnd) return
        println(tokens.first)
        printTokens(tokens.rest)
    }
    
}