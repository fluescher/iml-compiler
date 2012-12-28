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
import java.io.File
import ch.fhnw.iml.checker.GlobalImportChecker
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import ch.fhnw.iml.checker.FunctionProgramNameChecker
import ch.fhnw.iml.checker.ConditonLabelUniquenessChecker

object imlc extends App {
    
    if(args.length != 1) {
        printUsage()
        System.exit(1)
    }
    
    val checkers = List(SymbolChecker, 
            			TypeChecker,
            			FlowChecker,
            			InitializationChecker,
            			GlobalImportChecker,
            			ConditonLabelUniquenessChecker,
            			FunctionProgramNameChecker
            			)
    val code = if(canReadFile(args(0))) scala.io.Source.fromFile(args(0)).mkString else null
    			
    
    if(code == null) {
    	println("Error: Could not read file: " + args(0))
    }
    else {
    	compile(code, new File(args(0)).getName)   
    }
    
    def compile(code: String, file: String = "dynamic") = {
		val parser = new IMLParsers
		
		parser.parse(code) match {
	        case parser.Success(prog, _) => runCheckers(checkers, CheckSuccess(AST(prog))) match {
	            case CheckSuccess(ast) 		=> JVMWriter(ast, file)
	            case e:	CheckError[AST]		=> printCheckError(e)
	        }
	        case parser.Failure(e, n) => printParseError(e, n.pos)
	        case e => println("unknown error: " + e)
	    }
    }
    
    def printParseError(msg: String, next: Position) {
        println("Parse error on line " + next.line + "." + next.column +": " + msg)
    	println(next.longString)
    }
    
    def printCheckError(e: CheckError[AST]) =  e match {
        case CheckError(msg, n) => {
            println("Checking error on line " + n.pos.line + "." + n.pos.column +": " + msg)
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
    
    def printUsage() {
        println("IML Compiler v0.8")
        println("Usage: imlc <filename>")
    }
    
    def canReadFile(f: String) = {
        val file = new File(f)
        file.exists && file.canRead
    }
	
}