package ch.fhnw.iml.checker

import ch.fhnw.iml.ast._

object InitializationChecker extends Checker {
    case class Scope(global: SymbolTable, pre : SymbolTable, current: SymbolTable)
    
    override def apply(ast: AST) = {
        initCheck(ast.root) match {
            case CheckSuccess(_) => CheckSuccess(ast)
            case CheckError(m, n) => CheckError(m, n)
        }
    }

    private def initCheck(n: ProgramNode): CheckResult[SymbolTable] = {
        checkFunDecls(n) and checkProcDecls(n) and checkBlock(true)(n.cmd)(Scope(n.symbols, n.symbols, n.symbols))
    }

    private def checkFunDecls(n: ProgramNode): CheckResult[SymbolTable] = {
        n.cps.decls.filter(_.isInstanceOf[FunDecl])
            .map(_.asInstanceOf[FunDecl])
            .map(checkFunDecl(n))
            .foldLeft(CheckSuccess[SymbolTable](n.symbols): CheckResult[SymbolTable])(ignoreFirst)
    }

    private def checkFunDecl(n: ProgramNode)(f: FunDecl): CheckResult[SymbolTable] = {
        val funSymbols = markGlobalImport(f.global)(markInParameters(f.head.params.params)(f.symbols))
        checkConditions(f.pre)(Scope(n.symbols, funSymbols, funSymbols)) match {
            case CheckSuccess(symbls) => checkBlock(true)(f.cmd)(Scope(n.symbols, funSymbols, funSymbols)) match {
                case CheckSuccess(sym) => checkConditions(f.post)(Scope(n.symbols, funSymbols, sym)) match {
                    case CheckSuccess(s) => checkIsInitalized(f.head.retVal.i)(s)
                    case e => e
                }
                case e => e
            }
            case e => e
        }
    }

    private def checkConditions(conditions: Option[ConditionList])(symbols: Scope): CheckResult[SymbolTable] = conditions match {
        case None => CheckSuccess(symbols.current)
        case Some(l) => l.conditions.map({ case Condition(_, expr) => expr })
            .map(a => (checkValueExpr(a)(symbols)))
            .foldLeft(CheckSuccess[SymbolTable](symbols.current): CheckResult[SymbolTable])(combineToResult)
    }

    private def checkOutRef(params: List[Parameter])(symbols: SymbolTable): CheckResult[SymbolTable] = {
        params match {
            case Nil => CheckSuccess(symbols)
            case Parameter(OutFlow, _, storeDecl) :: xs => checkIsInitalized(storeDecl.i)(symbols) match {
                case CheckSuccess(_) => checkOutRef(xs)(symbols)
                case e => e
            }
            case Parameter(_, _, _) :: xs => checkOutRef(xs)(symbols)
            case other => CheckSuccess(symbols)
        }
    }

    private def markGlobalImport(global: Option[GlobalImportList])(symbols: SymbolTable): SymbolTable = global match {
        case None => symbols
        case Some(glob) => markInGlobals(glob.globals)(symbols)
    }

    private def checkGlobalImportOut(global: Option[GlobalImportList])(symbols: SymbolTable): CheckResult[SymbolTable] = global match {
        case None => CheckSuccess(symbols)
        case Some(glob) => glob.globals.filter(_.flow == OutFlow)
            .map(a => (checkIsInitalized(a.i)(symbols)))
            .foldLeft(CheckSuccess[SymbolTable](symbols): CheckResult[SymbolTable])(combineToResult)
    }

    private def checkIsInitalized(id: Ident)(symbols: SymbolTable): CheckResult[SymbolTable] = {
        if (symbols.isInitialized(id))
            CheckSuccess(symbols)
        else
            CheckError("Store must be initialized.", id)
    }

    private def checkProcDecls(n: ProgramNode): CheckResult[SymbolTable] = {
        n.cps.decls.filter(_.isInstanceOf[ProcDecl])
            .map(_.asInstanceOf[ProcDecl])
            .map(checkProcDecl(n))
            .foldLeft(CheckSuccess[SymbolTable](n.symbols): CheckResult[SymbolTable])(ignoreFirst)
    }

    private def checkProcDecl(n: ProgramNode)(p: ProcDecl): CheckResult[SymbolTable] = {
        val procSymbols = markGlobalImport(p.global)( markInParameters(p.head.params.params)(p.symbols));

        checkConditions(p.pre)(Scope(n.symbols, procSymbols, procSymbols)) match {
            case CheckSuccess(symbls) => checkBlock(true)(p.cmd)(Scope(n.symbols, procSymbols, procSymbols)) match {
                case CheckSuccess(sym) => checkConditions(p.post)(Scope(n.symbols, procSymbols, sym)) match {
                    case CheckSuccess(s) => checkOutRef(p.head.params.params)(s) match {
                        case CheckSuccess(s) => checkGlobalImportOut(p.global)(s)
                        case e => e
                    }
                    case e => e
                }
                case e => e
            }
            case e => e
        }
    }

    private def markInParameters(params: List[Parameter])(symbols: SymbolTable): SymbolTable = params match {
        case Nil => symbols
        case Parameter(InFlow, _, storeDecl) :: xs => markInParameters(xs)(symbols.markStorageAsInitialized(storeDecl.i))
        case Parameter(InOutFlow, _, storeDecl) :: xs => markInParameters(xs)(symbols.markStorageAsInitialized(storeDecl.i))
        case Parameter(_, _, storeDecl) :: xs => markInParameters(xs)(symbols)
    }

    private def markInGlobals(globals: List[GlobalImport])(symbols: SymbolTable): SymbolTable = globals match {
        case Nil => symbols
        case GlobalImport(InFlow, _, i) :: xs => markInGlobals(xs)(symbols.markStorageAsInitialized(i))
        case GlobalImport(InOutFlow, _, i) :: xs => markInGlobals(xs)(symbols.markStorageAsInitialized(i))
        case GlobalImport(_, _, _) :: xs => markInGlobals(xs)(symbols)
    }

    private def checkBlock(initAllowed: Boolean)(block: BlockCommand)(symbols: Scope): CheckResult[SymbolTable] = {
        checkCommandList(initAllowed)(block.cmds)(symbols)
    }

    private def checkCommandList(initAllowed: Boolean)(cmds: List[Command])(symbols: Scope): CheckResult[SymbolTable] = cmds match {
        case Nil => CheckSuccess(symbols.current)
        case x :: xs => checkCommand(initAllowed)(x)(symbols) match {
            case CheckSuccess(symbls) => checkCommandList(initAllowed)(xs)(Scope(symbols.global, symbols.pre, symbls))
            case e => e
        }
    }

    private def checkCommand(initAllowed: Boolean)(cmd: Command)(symbols: Scope): CheckResult[SymbolTable] = cmd match {
        case block: BlockCommand 			=> checkBlock(initAllowed)(block)(symbols)
        case SkipCommand 					=> CheckSuccess(symbols.current)
        case AssiCommand(left, right) 		=> combineToResult(checkLeftExpr(initAllowed)(left)(symbols.current), checkValueExpr(right)(symbols))
        case CondCommand(expr, cmd1, cmd2) 	=> ignoreSecond(mergeResults(symbols.current, checkCommand(true)(cmd1)(symbols), checkCommand(true)(cmd2)(symbols)), checkValueExpr(expr)(symbols))
        case WhileCommand(expr, cmd) 		=> ignoreSecond(checkCommand(false)(cmd)(symbols), checkValueExpr(expr)(symbols))
        case p: ProcCallCommand 			=> checkFunAndProcCall(p)(symbols) and checkProcCall(initAllowed)(p)(symbols)
        case InputCommand(expr) 			=> checkLeftExpr(initAllowed)(expr)(symbols.current)
        case OutputCommand(expr) 			=> checkValueExpr(expr)(symbols)
    }
    
    private def checkFunAndProcCall(n : Node)(symbols: Scope) : CheckResult[SymbolTable] = n match {
        case f: FunCallExpr 	if f.i.chars == "old"				=> checkValueExpr(f.exprs.head)(Scope(symbols.global, symbols.pre, symbols.pre))
        case f: FunCallExpr											=> val decl = symbols.global.getFunctionDeclaration(f.i)
            														   checkParamsAreInit(f.exprs)(decl.head.params)(symbols) and checkGlobalsAreInit(decl.global)(symbols.current)
        case p: ProcCallCommand										=> val decl = symbols.global.getProcedureDeclaration(p.f)
            														   checkParamsAreInit(p.exprs)(decl.head.params)(symbols) and checkGlobalsAreInit(decl.global)(symbols.current)
        case other													=> CheckSuccess(symbols.current)
    }
    
    private def checkParamsAreInit(exprs :List[Expr])(params: ParameterList)(symbols: Scope) = {
    	val paramToExpr = params.params.zip(exprs)
    	paramToExpr.map(a => checkParamToExpr(a)(symbols))
    			   .foldLeft(CheckSuccess[SymbolTable](symbols.current): CheckResult[SymbolTable])(combineToResult)
    }
    
    private def checkParamToExpr(paramToExpr : (Param, Expr))(symbols: Scope) : CheckResult[SymbolTable] = paramToExpr match {
        case (Parameter(OutFlow,_,_), e) 	=>	e match {
            case s: StoreExpr if !symbols.current.isInitialized(s.i)	=> CheckSuccess(symbols.current.markStorageAsInitialized(s.i))
            case s: StoreExpr if symbols.current.isInitialized(s.i)		=> CheckError("Out parameters must not be initialzied." , e)
            case other													=> CheckError("Only store expression allowed", e)
        }
        case (p , e)			=> checkValueExpr(e)(symbols)
    }
    
    private def checkGlobalsAreInit(globals : Option[GlobalImportList])(symbols: SymbolTable) : CheckResult[SymbolTable] = globals match {
        case Some(global) 	=> global.globals.filter(a => a.flow == InFlow || a.flow == InOutFlow)
        					   				 .map(a => checkIsInitalized(a.i)(symbols))
        					   				 .foldLeft(CheckSuccess[SymbolTable](symbols): CheckResult[SymbolTable])(combineToResult)
        case None			=> CheckSuccess(symbols)
    }
    
    private def checkProcCall(initAllowed: Boolean)(p: ProcCallCommand)(symbols: Scope): CheckResult[SymbolTable] = {
    	val paramsToExprs = if(symbols.global.containsProcedure(p.f)) symbols.global.getProcedureDeclaration(p.f).head.params.params.zip(p.exprs) else List()
        checkProcCallParameters(paramsToExprs)(initAllowed)(p.exprs)(symbols) match {
    	    case CheckSuccess(sym) if sym.containsProcedure(p.f) => sym.getProcedureDeclaration(p.f).global match {
    	        case None if(p.idents == Nil) => CheckSuccess(sym)
    	        case None 					  => CheckError("No global imports. ", p)
    	        case Some(glob)				  => if (p.idents.foldLeft(true)((a,b) => glob.globals.filter(_.flow == OutFlow).map(g => g.i).contains(b)))
    	            								checkProcCallGlobals(initAllowed)(p.idents)(sym)
    	            							 else
    	            							    CheckError("Global inits and global imports does not match. ", p)
    	    }
    	    case CheckSuccess(sym) => CheckSuccess(sym)
    	    case e => e
    	}
    }

    private def checkProcCallParameters(paramsToExprs : List[(Param, Expr)])(initAllowed: Boolean)(exprs: List[Expr])(symbols: Scope): CheckResult[SymbolTable] = paramsToExprs match{
    	case Nil 	=> CheckSuccess(symbols.current)
    	case x::xs 	=> checkProcCallParameter(x)(initAllowed)(symbols) match {
    	    case CheckSuccess(sym) => checkProcCallParameters(xs)(initAllowed)(exprs)(Scope(symbols.global, symbols.pre, sym))
    	    case e => e
    	}
    }
    
    private def checkProcCallParameter(paramToExpr : (Param, Expr))(initAllowed: Boolean)(symbols: Scope) =  paramToExpr match  {
        case (Parameter(InFlow,_,_), e) 	=>	checkValueExpr(e)(symbols)
        case (p , e) 						=>	e match {
            case  StoreExpr(_, _)	=> checkLeftExpr(initAllowed)(e)(symbols.current)
            case  other 			=> checkValueExpr(e)(symbols)
        }
    }

    private def checkProcCallGlobals(initAllowed: Boolean)(idents: List[Ident])(symbols: SymbolTable): CheckResult[SymbolTable] = {
        idents match {
            case Nil 		=> CheckSuccess(symbols)
            case x :: xs 	=> checkProcCallGlobals(initAllowed)(xs)(symbols.markStorageAsInitialized(x))
        }
    }

    private def mergeResults(symbols: SymbolTable, r1: CheckResult[SymbolTable], r2: CheckResult[SymbolTable]): CheckResult[SymbolTable] = r1 match {
        case e: CheckError[SymbolTable] => e
        case CheckSuccess(symbolsR1) => r2 match {
            case e: CheckError[SymbolTable] => e
            case CheckSuccess(symbolsR2) => CheckSuccess(mergeSymbols(symbols, symbolsR1, symbolsR2))
        }
    }

    private def mergeSymbols(orig: SymbolTable, one: SymbolTable, other: SymbolTable): SymbolTable = {
        var table = orig
        for ((_, sym) <- one.stores) {
            if (sym.isInitialized && other.isInitialized(sym.id))
                table = table.markStorageAsInitialized(sym.id)
        }
        table
    }

    private def checkValueExpr(expr: Expr)(symbols: Scope): CheckResult[SymbolTable] = expr match {
        case BoolLiteralExpression(_) 									=> CheckSuccess(symbols.current)
        case IntLiteralExpression(_) 									=> CheckSuccess(symbols.current)
        case StoreExpr(id, false) if symbols.current.isInitialized(id)	=> CheckSuccess(symbols.current)
        case va: StoreExpr 												=> CheckError("Use of uninitialized var", va)
        case m: MonadicExpr 											=> checkValueExpr(m.expr)(symbols)
        case d: DyadicExpr 												=> checkValueExpr(d.expr1)(symbols) and checkValueExpr(d.expr2)(symbols)
        case f: FunCallExpr 											=> checkFunAndProcCall(f)(symbols) and f.exprs.map(a => (checkValueExpr(a)(symbols)))
            																	.foldLeft(CheckSuccess[SymbolTable](symbols.current): CheckResult[SymbolTable])(combineToResult)
    }

    private def checkLeftExpr(initAllowed: Boolean)(expr: Expr)(symbols: SymbolTable): CheckResult[SymbolTable] = expr match {
        case StoreExpr(id, true) if !initAllowed 				=> CheckError("Its not allowed to initalized a store in this command.", expr)
        case StoreExpr(id, true) if !symbols.isInitialized(id) 	=> CheckSuccess(symbols.markStorageAsInitialized(id))
        case StoreExpr(id, true) if symbols.isInitialized(id) 	=> CheckError("You can only initialize a store once.", expr)
        case StoreExpr(id, false) if symbols.isConst(id) 		=> CheckError("Its only allowed to write once to a const.", expr)
        case StoreExpr(id, false) if symbols.isInitialized(id) 	=> CheckSuccess(symbols)
        case other 												=> CheckError("Use of not initalized store", expr)
    }

    private def combineToResult(r1: CheckResult[SymbolTable], r2: CheckResult[SymbolTable]): CheckResult[SymbolTable] = r2 match {
        case e: CheckError[SymbolTable] => e
        case r => r1
    }

    private def ignoreFirst(r1: CheckResult[SymbolTable], r2: CheckResult[SymbolTable]): CheckResult[SymbolTable] = r2 match {
        case e: CheckError[SymbolTable] => e
        case r => r2
    }

    private def ignoreSecond(r1: CheckResult[SymbolTable], r2: CheckResult[SymbolTable]): CheckResult[SymbolTable] = r1 match {
        case e: CheckError[SymbolTable] => e
        case r => r1
    }

}