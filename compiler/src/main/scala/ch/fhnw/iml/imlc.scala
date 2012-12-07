package ch.fhnw.iml

import ch.fhnw.iml.parsing.IMLParsers
import ch.fhnw.iml.generation.JVMWriter
import ch.fhnw.iml.generation.JVMWriter
import java.io.File
import ch.fhnw.iml.ast.AST.apply
import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.checker.SymbolChecker
import ch.fhnw.iml.checker.CheckSuccess
import ch.fhnw.iml.checker.TypeChecker

object imlc extends App {
    
    val code = scala.io.Source.fromFile(args(0)).mkString
    
    compile(code)
    
    def compile(code: String) = {
		val parser = new IMLParsers
		
		parser.parse(code) match {
	        case parser.Success(prog, _) => SymbolChecker(AST(prog)) match {
	            case CheckSuccess(a) => TypeChecker(a) match {
	                case CheckSuccess(_) => JVMWriter(a,new File(args(0)).getName())
	                case e => println(e)
	            }
	            case e => println(e)
	        } 
	        case e => println ("BLUBBER BLUBBER: " + e)
	    }
    }
	
}