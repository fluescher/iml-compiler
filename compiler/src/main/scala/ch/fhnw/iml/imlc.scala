package ch.fhnw.iml

import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.AST.apply
import ch.fhnw.iml.checker.CheckError
import ch.fhnw.iml.checker.CheckResult
import ch.fhnw.iml.checker.CheckSuccess
import ch.fhnw.iml.checker.Checker
import ch.fhnw.iml.checker.FlowChecker
import ch.fhnw.iml.checker.InitializationChecker
import ch.fhnw.iml.checker.SymbolChecker
import ch.fhnw.iml.checker.TypeChecker
import ch.fhnw.iml.generation.JVMWriter
import ch.fhnw.iml.generation.JVMWriter
import ch.fhnw.iml.parsing.IMLParsers

object imlc extends App {
    
    val checkers = List(SymbolChecker, TypeChecker, InitializationChecker, FlowChecker)
    
    val code = scala.io.Source.fromFile(args(0)).mkString
    
    compile(code)
    
    def compile(code: String) = {
		val parser = new IMLParsers
		
		parser.parse(code) match {
	        case parser.Success(prog, _) => runCheckers(checkers, CheckSuccess(AST(prog))) match {
	            case CheckSuccess(ast) 		=> JVMWriter(ast, "/target/"+prog.i.chars+".class")
	            case e:	CheckError[AST]		=> printCheckError(e)
	        }
	        case e => 
	    }
    }
    
    def printCheckError(e: CheckError[AST]) = e match {
        case CheckError(msg, n) => {
            println("Error on line " + n.pos.line + "." + n.pos.column +": " + msg)
            println(n.pos.longString)
        }
    }
    
    def runCheckers(checkers: List[Checker], res: CheckResult[AST]):CheckResult[AST] = res match {
        case CheckSuccess(ast) => checkers match {
	        case c::checkers 	=> runCheckers(checkers, c(ast))
	        case Nil 			=> res
	    }
        case e => e
    } 
	
}