package ch.fhnw.iml.checker

import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.ProgramNode
import ch.fhnw.iml.ast.ProcDecl
import ch.fhnw.iml.ast.FunDecl
import ch.fhnw.iml.ast.Node
import ch.fhnw.iml.ast.Decl
import ch.fhnw.iml.ast.Condition
import ch.fhnw.iml.ast.ConditionList

object ConditonLabelUniquenessChecker extends Checker {
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
						.map(a => checkConditions(a.pre)(Nil).map(checkConditions(a.post)))
						.map(_.asInstanceOf[CheckResult[Any]]))
	}
    														
    private def checkFunDecls(decls: List[Decl])(implicit p: ProgramNode) = { 
        toResult (decls	.filter(_.isInstanceOf[FunDecl])
				    	.map(_.asInstanceOf[FunDecl])
						.map(a => checkConditions(a.pre)(Nil).map(checkConditions(a.post)))
						.map(_.asInstanceOf[CheckResult[Any]]))
	}
    
    private def checkConditions(conds: Option[ConditionList])(names: List[String]): CheckResult[List[String]] = conds match {
        case None 			=> CheckSuccess(names)
        case Some(conds)	=> checkConditions(conds.conditions)(names)   
    }
    
    private def checkConditions(conds: List[Condition])(names: List[String] = Nil): CheckResult[List[String]] = {
		conds.foldLeft(CheckSuccess(names): CheckResult[List[String]])(checkConditions)   
    }
    
    private def checkConditions(res: CheckResult[List[String]], c: Condition): CheckResult[List[String]] = res.map(checkLabelName(c))
    
	private def checkLabelName(c: Condition)(names: List[String]): CheckResult[List[String]] = c.name match {
	    case Some(n) 	if(names.contains(n.chars))	=> CheckError("A condition label has to be unique", n) 
	    case Some(n)								=> CheckSuccess(n.chars::names)
	    case None									=> CheckSuccess(names)
	}
}