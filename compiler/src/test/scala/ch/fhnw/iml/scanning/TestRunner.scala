package ch.fhnw.iml.scanning

import java.io.File

import scala.collection.immutable.List.apply

import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.checker.CheckError
import ch.fhnw.iml.checker.CheckResult
import ch.fhnw.iml.checker.CheckSuccess
import ch.fhnw.iml.checker.Checker
import ch.fhnw.iml.checker.ConditonLabelUniquenessChecker
import ch.fhnw.iml.checker.FlowChecker
import ch.fhnw.iml.checker.FunctionProgramNameChecker
import ch.fhnw.iml.checker.GlobalImportChecker
import ch.fhnw.iml.checker.InitializationChecker
import ch.fhnw.iml.checker.RecursionInConditionChecker
import ch.fhnw.iml.checker.SymbolChecker
import ch.fhnw.iml.checker.TypeChecker
import ch.fhnw.iml.generation.JVMWriter
import ch.fhnw.iml.generation.JVMWriter.apply
import ch.fhnw.iml.parsing.IMLParsers

object TestRunner extends App {
	
    val directory = "src/test/resources/"
        
    val checkers = List(SymbolChecker, 
            			TypeChecker,
            			FlowChecker,
            			RecursionInConditionChecker,
            			InitializationChecker,
            			GlobalImportChecker,
            			ConditonLabelUniquenessChecker,
            			FunctionProgramNameChecker)
    val files = new java.io.File(directory).listFiles.filter(_.getName.endsWith(".iml"));
    
    for(file <- files){
        readFile(file)
    }
    
    def readFile(file: File) {
	    val code = if(canReadFile(file)) scala.io.Source.fromFile(file).mkString else null			
	    
	    if(code == null) {
	    	println("Error: Could not read file: " + file)
	    }
	    else {
	    	compile(code, file)   
	    }
    }
    
    def compile(code: String, file: File) = {
		val parser = new IMLParsers
		
		println("Filename: " + file.getName())
		parser.parse(code) match {
	        case parser.Success(prog, _) => runCheckers(checkers, CheckSuccess(AST(prog))) match {
	            case CheckSuccess(ast) 		=> JVMWriter(ast, "/target/"+prog.i.chars+".class")
	            case e:	CheckError[AST]		=> printCheckError(e)
	        }
	        case e => println("Error while parsing: " + e)
	    }
		println("----------------------------");
    }
    
    def printCheckError(e: CheckError[AST]) =  e match {
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
    
    def canReadFile(file: File) = {
        file.exists && file.canRead
    }
}