package ch.fhnw.iml.checker

import ch.fhnw.iml.ast._


/**
 * Calling conventions:
 * 
 * fun (a, b, c)
 * ret d
 * local
 * 	e
 * 
 * maps to the following local variable indized:
 * 
 * a: 0
 * b: 1
 * c: 2
 * e: 3
 * d: 4 (return value is always the last local variable)
 * 
 */
object SymbolChecker extends Checker {
    type CombinationResult[A,B] = (A, Option[CheckError[B]])
    
	override def apply(ast: AST) = {
        combine(combine(visitProgramNode, visitFunDecls), visitProcDecls)(ast)
    }
    
	private def visitProgramNode(ast: AST): CheckResult[AST] = {
	    val prg = ast.root
	    val storeDecls: List[StoreDecl] = prg.cps.decls	.filter(onlyVars).map(_.asInstanceOf[StoreDecl]) /* TODO fix bug if no cpsdecl is there */
	    
	    storeDecls.foldLeft((EmptyTable,None): CombinationResult[SymbolTable,AST])(reduceGlobals) match {
	        case (_, Some(e)) => e
	        case (l, None) => CheckSuccess(AST(ProgramNode(prg.i, prg.cps, prg.cmd, l)))
	    }
	}
	
	private def generateOldDecl(): (Ident, FunctionSymbol) = {
	    (Ident("old"), FunctionSymbol(Ident("old"), Void, null))
	}
	
	private def reduceGlobals(left: CombinationResult[SymbolTable,AST], right: StoreDecl) : CombinationResult[SymbolTable,AST] = left match {
	    case (l, None) if l.stores.contains(right.i) => (l, Some(CheckError("Variable " + right.i + " already declared.", right))) 
	    case (l, None) => (SymbolTable(l.functions, l.procs, (l.stores + (right.i -> StorageSymbol(right.i, right.t, right, false, true, false, false, 1, -1, false)))), None)
		case (l, Some(e)) => (l, Some(e))
	}
	
	private def visitFunDecls(ast: AST): CheckResult[AST] = {
	    val prg = ast.root
	    val funDecls: List[FunDecl] = prg.cps.decls	.filter(onlyFunDecls).map(_.asInstanceOf[FunDecl])
	    val otherDecls: List[Decl] = prg.cps.decls.filter(not(onlyFunDecls))
	    
	    funDecls.foldLeft((Nil,None): CombinationResult[List[FunDecl],FunDecl])(reduceFunDecls(prg.symbols)) match {
	        case (_, Some(e)) => CheckError(e.msg, e.node)
	        case (l, None) => {
	        	addFunDecls(prg.symbols, l) match {
	        	    case CheckSuccess(syms) => CheckSuccess(AST(ProgramNode(prg.i, CpsDecl(otherDecls ++ l), prg.cmd, syms))) 
	        	    case CheckError(m,n) 	=> CheckError(m,n)
	        	}
	        } 
	    }
	}
	
	private def addFunDecls(global: SymbolTable, l: List[FunDecl]) : CheckResult[SymbolTable] = l match {
	    case Nil 	 => CheckSuccess(global)
	    case x::xs  if(global.functions.contains(x.head.i)) => CheckError("Already defined this function", x)
	    case x::xs	 => addFunDecls(SymbolTable(global.functions +(x.head.i -> FunctionSymbol(x.head.i, x.head.retVal.t, x)), global.procs, global.stores), xs)
	}

	private def addProcDecls(global: SymbolTable, l: List[ProcDecl]) : CheckResult[SymbolTable] = l match {
	    case Nil 	 => CheckSuccess(global)
	    case x::xs  if(global.procs.contains(x.head.i)) => CheckError("Already defined this function", x)
	    case x::xs	 => addProcDecls(SymbolTable(global.functions, global.procs +(x.head.i -> ProcedureSymbol(x.head.i, x)), global.stores), xs)
	}
	
	private def reduceFunDecls(global: SymbolTable)(left: CombinationResult[List[FunDecl],FunDecl], right: FunDecl) : CombinationResult[List[FunDecl],FunDecl] = left match {
	    case (l, None) => {
	    	visitFunDecl(global)(right) match {
	    	    case CheckSuccess(f) 	=> ((f :: l), None)
	    	    case CheckError(m,n)	=> (l, Some(CheckError(m,n)))
	    	}
	    }
		case (l, Some(e)) => (l, Some(e))
	}
	
	private def visitFunDecl(global: SymbolTable)(f: FunDecl): CheckResult[FunDecl] = {
		combine(combine(combine(visitFunParams, visitReturn),visitGlobalFunImport(global)),visitFunLocals(global))(f) match {
		    case CheckSuccess(f) => CheckSuccess(updateReturnValueIndex(f))
		    case e => e
		}
	}
	
	private def visitGlobalFunImport(global: SymbolTable)(f: FunDecl) : CheckResult[FunDecl] = {
	     f.global match {
	        case Some(l) => {
	        	visitGlobals(global)(f.symbols)(l.globals.map(_.asInstanceOf[GlobalImport])) match { 
	        	    case CheckSuccess(s) => CheckSuccess(FunDecl(f.head, f.global, f.cps, f.pre, f.post, f.cmd, s))
	        	    case CheckError(m,n) => CheckError(m, n)
	        	}  
	        }
	        case None => CheckSuccess(f)
	    }
	}
	
	private def visitGlobals(global: SymbolTable)(local: SymbolTable)(globals: List[GlobalImport]) = {
	    globals.foldLeft((local, None):CombinationResult[SymbolTable,GlobalImport])(reduceGlobals(global)) match {
	        case (l, None) 		=> CheckSuccess(l)
	        case (_, Some(e))	=> CheckError(e.msg, e.node)
	    }
	}
	
	private def reduceGlobals(global: SymbolTable)(left: CombinationResult[SymbolTable,GlobalImport], right: GlobalImport) : CombinationResult[SymbolTable,GlobalImport] = left match {
	   	case (l, None) if l.stores.contains(right.i) 		=> (l, Some(CheckError("Variable " + right.i + " already declared.", right))) 
	   	case (l, None) if !global.stores.contains(right.i) 	=> (l, Some(CheckError("Variable " + right.i + " is not declared as global.", right))) 
	    case (l, None) 										=> (addStorageSymbol(l, StorageSymbol(right.i, global.stores.get(right.i).get.t, global.stores.get(right.i).get.decl, false, true, false, false, -1, -1, false)), None)
		case (l, Some(e)) 									=> (l, Some(e))
	}
	
	private def visitFunLocals(global: SymbolTable)(f: FunDecl): CheckResult[FunDecl] = {
	    f.cps match {
	        case Some(l) => {
	        	visitLocals(f.symbols)(l.decls.map(_.asInstanceOf[StoreDecl])) match { // TODO nested function definition? (cast)
	        	    case CheckSuccess(s) => CheckSuccess(FunDecl(f.head, f.global, f.cps, f.pre, f.post, f.cmd, s))
	        	    case CheckError(m,n) => CheckError(m, n)
	        	}  
	        }
	        case None => CheckSuccess(f)
	    }
	}
	
	private def visitReturn(f: FunDecl): CheckResult[FunDecl] = {
	    val ret = f.head.retVal
	    if(f.symbols.stores.contains(ret.i)) CheckError("Variable for return value is already defined.", ret)
	    else CheckSuccess(FunDecl(f.head, f.global, f.cps, f.pre, f.post, f.cmd, addStorageSymbol(f.symbols, StorageSymbol(ret.i, ret.t, ret, true, false, false, false, -1, -1, false))))
	}
	
	private def visitFunParams(f: FunDecl):CheckResult[FunDecl] = visitParams(f.head.params) match {
	    case CheckSuccess(s) 	=> CheckSuccess(FunDecl(f.head, f.global, f.cps, f.pre, f.post, f.cmd, s))
	    case CheckError(m,n)	=> CheckError(m,n)
	}
	
	private def visitLocals(local: SymbolTable)(locals: List[StoreDecl]) = {
	    locals.foldLeft(((local, None):CombinationResult[SymbolTable,StoreDecl]), argumentCount(local)-1)(reduceLocals) match { 
	        case  ((l, None), i) => CheckSuccess(l)
	        case ((_, Some(e)),_)=> CheckError(e.msg, e.node)
	    }
	}
	
	private def reduceLocals(left: (CombinationResult[SymbolTable,StoreDecl], Int), right: StoreDecl) : (CombinationResult[SymbolTable,StoreDecl], Int) = left match {
	   	case ((l, None), i) if l.stores.contains(right.i) => ((l, Some(CheckError("Variable " + right.i + " already declared.", right))) ,i)
	    case ((l, None), i) => ((addStorageSymbol(l, StorageSymbol(right.i, right.t, right, false, false, false, false, -1, i+1, false)), None), i+1)
		case ((l, Some(e)), i) => ((l, Some(e)),i)
	}
	
	private def visitParams(p: ParameterList) = {
	    p.params.foldLeft((EmptyTable,None): CombinationResult[SymbolTable,Parameter])(reduceParams) match {
	        case (_, Some(e)) 	=> CheckError(e.msg, e.node)
	        case (l, None) 		=> CheckSuccess(l)
	    }
	}
	
	private def reduceParams(left: CombinationResult[SymbolTable,Parameter], right: Parameter) : CombinationResult[SymbolTable,Parameter] = left match {
	   	case (l, None) if l.stores.contains(right.store.i) => (l, Some(CheckError("Variable " + right.store.i + " already declared.", right))) 
	    case (l, None) => (addStorageSymbol(l, StorageSymbol(right.store.i, right.store.t, right.store, false, false, true, right.flow == OutFlow || right.flow == InOutFlow, l.stores.size, -1, false)), None)
		case (l, Some(e)) => (l, Some(e))
	}

	private def visitProcDecls(ast: AST): CheckResult[AST] = {
	    val prg = ast.root
	    val funDecls: List[ProcDecl] = prg.cps.decls.filter(onlyProcDecls).map(_.asInstanceOf[ProcDecl])
	    val otherDecls: List[Decl] = prg.cps.decls.filter(not(onlyProcDecls))
	    
	    funDecls.foldLeft((Nil,None): CombinationResult[List[ProcDecl],ProcDecl])(reduceProcDecls(prg.symbols)) match {
	        case (_, Some(e)) => CheckError(e.msg, e.node)
	        case (l, None) => {
	        	addProcDecls(prg.symbols, l) match {
	        	    case CheckSuccess(syms) => CheckSuccess(AST(ProgramNode(prg.i, CpsDecl(otherDecls ++ l), prg.cmd, syms))) 
	        	    case CheckError(m,n) 	=> CheckError(m,n)
	        	}
	        }
	    }
	}
	
	private def reduceProcDecls(global: SymbolTable)(left: CombinationResult[List[ProcDecl],ProcDecl], right: ProcDecl) : CombinationResult[List[ProcDecl],ProcDecl] = left match {
	    case (l, None) => {
	    	visitProcDecl(global)(right) match {
	    	    case CheckSuccess(f) 	=> ((f :: l), None)
	    	    case CheckError(m,n)	=> (l, Some(CheckError(m,n)))
	    	}
	    }
		case (l, Some(e)) => (l, Some(e))
	}
	
	private def visitProcDecl(global: SymbolTable)(p: ProcDecl): CheckResult[ProcDecl] = {
	    combine(combine(visitProcParams, visitGlobalProcImport(global)),visitProcLocals)(p)
	}
	
	private def visitProcParams(p: ProcDecl):CheckResult[ProcDecl] = visitParams(p.head.params) match {
	    case CheckSuccess(s) 	=> CheckSuccess(ProcDecl(p.head, p.global, p.cps, p.pre, p.post, p.cmd, s))
	    case CheckError(m,n)	=> CheckError(m,n)
	}
	
	private def visitGlobalProcImport(global: SymbolTable)(p: ProcDecl): CheckResult[ProcDecl] = {
	    p.global match {
	        case Some(l) => {
	        	visitGlobals(global)(p.symbols)(l.globals.map(_.asInstanceOf[GlobalImport])) match { 
	        	    case CheckSuccess(s) => CheckSuccess(ProcDecl(p.head, p.global, p.cps, p.pre, p.post, p.cmd, s))
	        	    case CheckError(m,n) => CheckError(m, n)
	        	}  
	        }
	        case None => CheckSuccess(p)
	    }
	}
	
	private def visitProcLocals(p: ProcDecl): CheckResult[ProcDecl] = {
	    p.cps match {
	        case Some(l) => {
	        	visitLocals(p.symbols)(l.decls.map(_.asInstanceOf[StoreDecl])) match { // TODO nested procedure definition? (cast)
	        	    case CheckSuccess(s) => CheckSuccess(ProcDecl(p.head, p.global, p.cps, p.pre, p.post,p.cmd, s))
	        	    case CheckError(m,n) => CheckError(m, n)
	        	}  
	        }
	        case None => CheckSuccess(p)
	    }
	}
	
	private def updateReturnValueIndex(f: FunDecl) = {
	    val ret = getReturnValue(f.symbols)
	    val nret = StorageSymbol(ret.id, ret.t, ret.decl, true, false, false, false, -1, localCount(f.symbols), false)
	    FunDecl(f.head, f.global, f.cps, f.pre, f.post, f.cmd, SymbolTable(f.symbols.functions, f.symbols.procs, f.symbols.stores.updated(ret.id, nret)))
	}
	
	private def getReturnValue(syms: SymbolTable) = {
	    var r: StorageSymbol = null
		for((k,v) <- syms.stores if v.isRet) {
			r = v   
		}
		r
	}
	
	private def addStorageSymbol(table: SymbolTable, sym: StorageSymbol) = {
	    SymbolTable(table.functions, table.procs, table.stores + (sym.id -> sym))
	}
	
	private def localCount(symbols: SymbolTable) = {
	    var i = 0
	    for((k,v) <- symbols.stores if v.isArgument || !(v.isGlobal || v.isRet)) i += 1
	    i
	}
	
	private def argumentCount(symbols: SymbolTable) = {
	    var i = 0
	    for((k,v) <- symbols.stores if v.isArgument) i += 1
	    i
	}
	
	private def onlyVars(a: Decl) = a.isInstanceOf[StoreDecl]
	
	private def onlyFunDecls(a: Decl) = a.isInstanceOf[FunDecl]
	
	private def onlyProcDecls(a: Decl) = a.isInstanceOf[ProcDecl]
	
	private def not[A](f: A => Boolean)(a: A) = !f(a) 
}