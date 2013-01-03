package ch.fhnw.iml.checker

import ch.fhnw.iml.ast.ProgramNode
import ch.fhnw.iml.ast.ConditionList
import ch.fhnw.iml.ast.FunDecl
import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.Condition
import ch.fhnw.iml.ast.Decl
import ch.fhnw.iml.ast.StoreExpr
import ch.fhnw.iml.ast.FunCallExpr
import ch.fhnw.iml.ast.StorageSymbol
import ch.fhnw.iml.ast.IntLiteralExpression
import ch.fhnw.iml.ast.MonadicExpr
import ch.fhnw.iml.ast.DyadicExpr
import ch.fhnw.iml.ast.BoolLiteralExpression
import ch.fhnw.iml.ast.PlusOpr
import ch.fhnw.iml.ast.MinusOpr
import ch.fhnw.iml.ast.NotEqualsOpr
import ch.fhnw.iml.ast.ArithOpr
import ch.fhnw.iml.ast.EqualsOpr
import ch.fhnw.iml.ast.NotOpr
import ch.fhnw.iml.ast.RelOpr
import ch.fhnw.iml.ast.BoolOpr
import ch.fhnw.iml.ast.Expr
import ch.fhnw.iml.ast.SymbolTable

object RecursionInConditionChecker extends Checker {
	def apply(ast: AST) ={
	    recursionCheck(ast.root) match {
	        case CheckSuccess(_) 	=> CheckSuccess(ast)
	        case CheckError(m,n)	=> CheckError(m,n)
	    }
	}
	
	private def recursionCheck(implicit p: ProgramNode) =  checkFunDecls(p.cps.decls)
    														
    private def checkFunDecls(decls: List[Decl])(implicit p: ProgramNode) = { 
        toResult (decls	.filter(_.isInstanceOf[FunDecl])
				    	.map(_.asInstanceOf[FunDecl])
						.map(a => checkConditions(a.pre)(Scope(p.symbols, a)).map(checkConditions(a.post)))
						.map(_.asInstanceOf[CheckResult[Any]]))
	}
    
    private def checkConditions(conds: Option[ConditionList])(cur: Scope): CheckResult[Scope] = conds match {
        case None 			=> CheckSuccess(cur)
        case Some(conds)	=> checkConditions(conds.conditions)(cur)   
    }
    
    private def checkConditions(conds: List[Condition])(implicit cur: Scope): CheckResult[Scope] = {
		conds.foldLeft(CheckSuccess(cur): CheckResult[Scope])(checkConditions)   
    }
    
    private def checkConditions(res: CheckResult[Scope], c: Condition)(implicit cur: Scope): CheckResult[Scope] = res and checkExpr(c.expr)
    
    private def checkExprs(res: CheckResult[Scope], e: Expr)(implicit cur: Scope): CheckResult[Scope] = res and checkExpr(e)
    
    private def checkExpr(e: Expr)(implicit cur: Scope): CheckResult[Scope] = e match{
	    case m: MonadicExpr	 															=> checkMonadicExpr(m)
	    case d: DyadicExpr																=> checkDyadicExpr(d)
	    case f: FunCallExpr		if cur.symbols.getFunctionDeclaration(f.i) == cur.f 	=> CheckError("No recursion allowed in conditions", e)
	    case f: FunCallExpr																=> f.exprs.foldLeft(CheckSuccess(cur): CheckResult[Scope])(checkExprs)   
	    case _ 																			=> CheckSuccess(cur)
    }
    
    private def checkDyadicExpr(d: DyadicExpr)(implicit cur: Scope): CheckResult[Scope] = d match {
	    case DyadicExpr(opr, expr1, expr2) => checkExpr(expr1) and checkExpr(expr2)
	}
	
	private def checkMonadicExpr(m: MonadicExpr)(implicit cur: Scope): CheckResult[Scope] = m match {
	    case MonadicExpr(opr, expr) => checkExpr(expr)
	}
	
	private case class Scope(symbols: SymbolTable, f: FunDecl)
}