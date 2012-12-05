package ch.fhnw.iml.checker

import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.Decl
import ch.fhnw.iml.ast.EmptyTable
import ch.fhnw.iml.ast.FunDecl
import ch.fhnw.iml.ast.Parameter
import ch.fhnw.iml.ast.ParameterList
import ch.fhnw.iml.ast.ProgramNode
import ch.fhnw.iml.ast.StorageSymbol
import ch.fhnw.iml.ast.StoreDecl
import ch.fhnw.iml.ast.SymbolTable
import ch.fhnw.iml.ast.CpsDecl
import ch.fhnw.iml.ast.ProcDecl
import ch.fhnw.iml.ast.StorageSymbol
import ch.fhnw.iml.ast.StorageSymbol

object SymbolChecker extends Checker {
    type CombinationResult[A,B] = (A, Option[CheckError[B]])
    
	override def apply(ast: AST) = {
        combine(combine(visitProgramNode, visitFunDecls), visitProcDecls)(ast)
	}
	
	private def visitProgramNode(ast: AST): CheckResult[AST] = {
	    val prg = ast.root
	    val storeDecls: List[StoreDecl] = prg.cps.decls	.filter(onlyVars).map(_.asInstanceOf[StoreDecl])
	    storeDecls.foldLeft((EmptyTable,None): CombinationResult[SymbolTable,AST])(reduceGlobals) match {
	        case (_, Some(e)) => e
	        case (l, None) => CheckSuccess(AST(ProgramNode(prg.i, prg.cps, prg.cmd, l)))
	    }
	}
	
	private def reduceGlobals(left: CombinationResult[SymbolTable,AST], right: StoreDecl) : CombinationResult[SymbolTable,AST] = left match {
	    case (l, None) if l.stores.contains(right.i) => (l, Some(CheckError("Variable " + right.i + " already declared.", right))) 
	    case (l, None) => (SymbolTable(l.functions, l.procs, (l.stores + (right.i -> StorageSymbol(right.i, right.t, false, true, false, -1, false)))), None)
		case (l, Some(e)) => (l, Some(e))
	}
	
	private def visitFunDecls(ast: AST): CheckResult[AST] = {
	    val prg = ast.root
	    val funDecls: List[FunDecl] = prg.cps.decls	.filter(onlyFunDecls).map(_.asInstanceOf[FunDecl])
	    val otherDecls: List[Decl] = prg.cps.decls.filter(not(onlyFunDecls))
	    
	    funDecls.foldLeft((Nil,None): CombinationResult[List[FunDecl],FunDecl])(reduceFunDecls) match {
	        case (_, Some(e)) => CheckError(e.msg, e.node)
	        case (l, None) => CheckSuccess(AST(ProgramNode(prg.i, CpsDecl(otherDecls ++ l), prg.cmd, prg.symbols)))
	    }
	}
	
	private def reduceFunDecls(left: CombinationResult[List[FunDecl],FunDecl], right: FunDecl) : CombinationResult[List[FunDecl],FunDecl] = left match {
	    case (l, None) => {
	    	visitFunDecl(right) match {
	    	    case CheckSuccess(f) 	=> ((f :: l), None)
	    	    case CheckError(m,n)	=> (l, Some(CheckError(m,n)))
	    	}
	    }
		case (l, Some(e)) => (l, Some(e))
	}
	
	private def visitFunDecl(f: FunDecl): CheckResult[FunDecl] = {
	    combine(combine(visitFunParams, visitReturn),visitGlobalFunImport)(f)
	}
	
	private def visitGlobalFunImport(f: FunDecl) = {
	    CheckSuccess(f)
	}
	
	private def visitReturn(f: FunDecl): CheckResult[FunDecl] = {
	    val ret = f.head.store
	    if(f.symbols.stores.contains(ret.i)) CheckError("Variable for return value is already defined.", ret)
	    else CheckSuccess(FunDecl(f.head, f.global, f.cps, f.pre, f.post, f.cmd, addStorageSymbol(f.symbols, StorageSymbol(ret.i, ret.t, true, false, false, -1, false))))
	}
	
	private def visitFunParams(f: FunDecl):CheckResult[FunDecl] = visitParams(f.head.params) match {
	    case CheckSuccess(s) 	=> CheckSuccess(FunDecl(f.head, f.global, f.cps, f.pre, f.post, f.cmd, s))
	    case CheckError(m,n)	=> CheckError(m,n)
	}
	
	private def visitParams(p: ParameterList) = {
	    p.params.foldLeft((EmptyTable,None): CombinationResult[SymbolTable,Parameter])(reduceParams) match {
	        case (_, Some(e)) 	=> CheckError(e.msg, e.node)
	        case (l, None) 		=> CheckSuccess(l)
	    }
	}
	
	private def reduceParams(left: CombinationResult[SymbolTable,Parameter], right: Parameter) : CombinationResult[SymbolTable,Parameter] = left match {
	   	case (l, None) if l.stores.contains(right.store.i) => (l, Some(CheckError("Variable " + right.store.i + " already declared.", right))) 
	    case (l, None) => (addStorageSymbol(l, StorageSymbol(right.store.i, right.store.t, false, false, true, l.stores.size+1, false)), None)
		case (l, Some(e)) => (l, Some(e))
	}

	private def visitProcDecls(ast: AST): CheckResult[AST] = {
	    val prg = ast.root
	    val funDecls: List[ProcDecl] = prg.cps.decls.filter(onlyProcDecls).map(_.asInstanceOf[ProcDecl])
	    val otherDecls: List[Decl] = prg.cps.decls.filter(not(onlyProcDecls))
	    
	    funDecls.foldLeft((Nil,None): CombinationResult[List[ProcDecl],ProcDecl])(reduceProcDecls) match {
	        case (_, Some(e)) => CheckError(e.msg, e.node)
	        case (l, None) => CheckSuccess(AST(ProgramNode(prg.i, CpsDecl(otherDecls ++ l), prg.cmd, prg.symbols)))
	    }
	}
	
	private def reduceProcDecls(left: CombinationResult[List[ProcDecl],ProcDecl], right: ProcDecl) : CombinationResult[List[ProcDecl],ProcDecl] = left match {
	    case (l, None) => {
	    	visitProcDecl(right) match {
	    	    case CheckSuccess(f) 	=> ((f :: l), None)
	    	    case CheckError(m,n)	=> (l, Some(CheckError(m,n)))
	    	}
	    }
		case (l, Some(e)) => (l, Some(e))
	}
	
	private def visitProcDecl(p: ProcDecl): CheckResult[ProcDecl] = {
	    combine(visitProcParams, visitGlobalProcImport)(p)
	}
	
	private def visitProcParams(p: ProcDecl):CheckResult[ProcDecl] = visitParams(p.head.params) match {
	    case CheckSuccess(s) 	=> CheckSuccess(ProcDecl(p.head, p.global, p.cps, p.pre, p.post, p.cmd, s))
	    case CheckError(m,n)	=> CheckError(m,n)
	}
	
	private def visitGlobalProcImport(p: ProcDecl) = {
	    CheckSuccess(p)
	}
	
	private def addStorageSymbol(table: SymbolTable, sym: StorageSymbol) = {
	    SymbolTable(table.functions, table.procs, table.stores + (sym.id -> sym))
	}
	
	private def onlyVars(a: Decl) = a.isInstanceOf[StoreDecl]
	
	private def onlyFunDecls(a: Decl) = a.isInstanceOf[FunDecl]
	
	private def onlyProcDecls(a: Decl) = a.isInstanceOf[ProcDecl]
	
	private def not[A](f: A => Boolean)(a: A) = !f(a) 
}