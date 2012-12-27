package ch.fhnw.iml.checker

import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.ProcDecl
import ch.fhnw.iml.ast.FunDecl
import ch.fhnw.iml.ast.Decl
import ch.fhnw.iml.ast.ProgramNode
import ch.fhnw.iml.ast.Node

object FunctionProgramNameChecker extends Checker {
	def apply(ast: AST) ={
	    nameCheck(ast.root) match {
	        case CheckSuccess(_) 	=> CheckSuccess(ast)
	        case CheckError(m,n)	=> CheckError(m,n)
	    }
	}
	
	private def nameCheck(implicit p: ProgramNode) =  checkProcDecls(p.cps.decls) and checkFunDecls(p.cps.decls)
	
	private def checkProcDecls(decls: List[Decl])(implicit p: ProgramNode) = {
		toResult (decls	.filter(_.isInstanceOf[ProcDecl])
						.map(_.asInstanceOf[ProcDecl])
						.map(a => checkName(a.head.i.chars, a.head)))
	}
    														
    private def checkFunDecls(decls: List[Decl])(implicit p: ProgramNode) = { 
        toResult (decls	.filter(_.isInstanceOf[FunDecl])
				    	.map(_.asInstanceOf[FunDecl])
						.map(a => checkName(a.head.i.chars, a.head)))
	}
    
    private def checkName(n: String, node: Node)(implicit p: ProgramNode): CheckResult[Any] = {
        if(n == p.i.chars) {
            CheckError("No routine may have the name of the program when compiling to the JVM backend.", node)
        } else {
            CheckSuccess(n)
        }
    }
}