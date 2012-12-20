package ch.fhnw.iml.checker

import ch.fhnw.iml.ast._

object InitializationChecker extends Checker {
    override def apply(ast: AST) = {
        initCheck(ast.root) match {
            case CheckSuccess(_) => CheckSuccess(ast)
            case CheckError(m, n) => CheckError(m, n)
        }
    }

    private def initCheck(n: ProgramNode): CheckResult[SymbolTable] = {
        checkFunDecls(n) and checkProcDecls(n) and checkBlock(true)(n.cmd)(n.symbols)
    }

    private def checkFunDecls(n: ProgramNode): CheckResult[SymbolTable] = {
        n.cps.decls.filter(_.isInstanceOf[FunDecl])
            .map(_.asInstanceOf[FunDecl])
            .map(checkFunDecl)
            .foldLeft(CheckSuccess[SymbolTable](n.symbols): CheckResult[SymbolTable])(ignoreFirst)
    }

    private def checkFunDecl(f: FunDecl): CheckResult[SymbolTable] = {
        var funSymbols = markInParameters(f.head.params.params)(f.symbols);
        funSymbols = markGlobalImport(f.global)(funSymbols)
        checkConditions(f.pre)(funSymbols) match {
            case CheckSuccess(symbls) => checkBlock(true)(f.cmd)(symbls) match {
                case CheckSuccess(sym) => checkConditions(f.post)(sym) match {
                    case CheckSuccess(s) => checkIsInitalized(f.head.retVal.i)(s)
                    case e => e
                }
                case e => e
            }
            case e => e
        }
    }

    private def checkConditions(conditions: Option[ConditionList])(symbols: SymbolTable): CheckResult[SymbolTable] = conditions match {
        case None => CheckSuccess(symbols)
        case Some(l) => l.conditions.map({ case Condition(_, expr) => expr })
            .map(a => (checkValueExpr(a)(symbols)))
            .foldLeft(CheckSuccess[SymbolTable](symbols): CheckResult[SymbolTable])(combineToResult)
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
            CheckError("Store must be initialized.", symbols.stores.get(id).getOrElse(null).id)
    }

    private def checkProcDecls(n: ProgramNode): CheckResult[SymbolTable] = {
        n.cps.decls.filter(_.isInstanceOf[ProcDecl])
            .map(_.asInstanceOf[ProcDecl])
            .map(checkProcDecl)
            .foldLeft(CheckSuccess[SymbolTable](n.symbols): CheckResult[SymbolTable])(ignoreFirst)
    }

    private def checkProcDecl(p: ProcDecl): CheckResult[SymbolTable] = {
        var procSymbols = markInParameters(p.head.params.params)(p.symbols);
        procSymbols = markGlobalImport(p.global)(procSymbols)

        checkConditions(p.pre)(procSymbols) match {
            case CheckSuccess(symbls) => checkBlock(true)(p.cmd)(symbls) match {
                case CheckSuccess(sym) => checkConditions(p.post)(sym) match {
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

    private def checkBlock(initAllowed: Boolean)(block: BlockCommand)(symbols: SymbolTable): CheckResult[SymbolTable] = {
        checkCommandList(initAllowed)(block.cmds)(symbols)
    }

    private def checkCommandList(initAllowed: Boolean)(cmds: List[Command])(symbols: SymbolTable): CheckResult[SymbolTable] = cmds match {
        case Nil => CheckSuccess(symbols)
        case x :: xs => checkCommand(initAllowed)(x)(symbols) match {
            case CheckSuccess(symbls) => checkCommandList(initAllowed)(xs)(symbls)
            case e => e
        }
    }

    private def checkCommand(initAllowed: Boolean)(cmd: Command)(symbols: SymbolTable): CheckResult[SymbolTable] = cmd match {
        case block: BlockCommand 			=> checkBlock(initAllowed)(block)(symbols)
        case SkipCommand 					=> CheckSuccess(symbols)
        case AssiCommand(left, right) 		=> ignoreSecond(checkLeftExpr(initAllowed)(left)(symbols), checkValueExpr(right)(symbols))
        case CondCommand(expr, cmd1, cmd2) 	=> ignoreSecond(mergeResults(symbols, checkCommand(true)(cmd1)(symbols), checkCommand(true)(cmd2)(symbols)), checkValueExpr(expr)(symbols))
        case WhileCommand(expr, cmd) 		=> ignoreSecond(checkCommand(false)(cmd)(symbols), checkValueExpr(expr)(symbols))
        case p: ProcCallCommand 			=> checkFunAndProcCall(p)(symbols) and checkProcCall(initAllowed)(p)(symbols)
        case InputCommand(expr) 			=> checkLeftExpr(initAllowed)(expr)(symbols)
        case OutputCommand(expr) 			=> checkValueExpr(expr)(symbols)
    }
    
    private def checkFunAndProcCall(n : Node)(symbols: SymbolTable) : CheckResult[SymbolTable] = n match {
        case f: FunCallExpr 		=> checkGlobalsAreInit(symbols.getFunctionDeclaration(f.i).global)(symbols)
        case p: ProcCallCommand 	=> checkGlobalsAreInit(symbols.getProcedureDeclaration(p.f).global)(symbols)
        case other					=> CheckSuccess(symbols)    
    }
    
    private def checkGlobalsAreInit(globals : Option[GlobalImportList])(symbols: SymbolTable) : CheckResult[SymbolTable] = globals match {
        case Some(global) 	=> global.globals.filter(a => a.flow == InFlow || a.flow == InOutFlow)
        					   				 .map(a => checkIsInitalized(a.i)(symbols))
        					   				 .foldLeft(CheckSuccess[SymbolTable](symbols): CheckResult[SymbolTable])(combineToResult)
        case None			=> CheckSuccess(symbols)
    }
    
    private def checkProcCall(initAllowed: Boolean)(p: ProcCallCommand)(symbols: SymbolTable): CheckResult[SymbolTable] = {
    	checkProcCallParameters(initAllowed)(p.exprs)(symbols) match {
    	    case CheckSuccess(sym) => sym.getProcedureDeclaration(p.f).global match {
    	        case None if(p.idents == Nil) => CheckSuccess(sym)
    	        case None 					  => CheckError("No global imports. ", p)
    	        case Some(glob)				  => if (p.idents.foldLeft(true)((a,b) => glob.globals.filter(_.flow == OutFlow).map(g => g.i).contains(b)))
    	            								checkProcCallGlobals(initAllowed)(p.idents)(sym)
    	            							 else
    	            							    CheckError("Global inits and global imports does not match. ", p)
    	    }
    	    case e => e
    	}
    }

    private def checkProcCallParameters(initAllowed: Boolean)(exprs: List[Expr])(symbols: SymbolTable): CheckResult[SymbolTable] = {
        exprs match {
            case Nil => CheckSuccess(symbols)
            case x :: xs => x match {
                case StoreExpr(_, _) 		=> checkLeftExpr(initAllowed)(x)(symbols) match {
                    case CheckSuccess(sym) 	=> checkProcCallParameters(initAllowed)(xs)(sym)
                    case e => e
                }
                case other => checkValueExpr(x)(symbols) match {
                    case CheckSuccess(sym) 	=> checkProcCallParameters(initAllowed)(xs)(sym)
                    case e 					=> e
                }
            }
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

    private def checkValueExpr(expr: Expr)(symbols: SymbolTable): CheckResult[SymbolTable] = expr match {
        case BoolLiteralExpression(_) 							=> CheckSuccess(symbols)
        case IntLiteralExpression(_) 							=> CheckSuccess(symbols)
        case StoreExpr(id, false) if symbols.isInitialized(id)	=> CheckSuccess(symbols)
        case va: StoreExpr 										=> CheckError("Use of not initialized var", va)
        case m: MonadicExpr 									=> checkValueExpr(m.expr)(symbols)
        case d: DyadicExpr 										=> ignoreSecond(checkValueExpr(d.expr1)(symbols), checkValueExpr(d.expr1)(symbols))
        case f: FunCallExpr 									=> checkFunAndProcCall(f)(symbols) and f.exprs.map(a => (checkValueExpr(a)(symbols)))
            															.foldLeft(CheckSuccess[SymbolTable](symbols): CheckResult[SymbolTable])(combineToResult)
    }

    private def checkLeftExpr(initAllowed: Boolean)(expr: Expr)(symbols: SymbolTable): CheckResult[SymbolTable] = expr match {
        case StoreExpr(id, true) if !initAllowed 				=> CheckError("Its not allowed to initalized a store in this command.", expr)
        case StoreExpr(id, true) if !symbols.isInitialized(id) 	=> CheckSuccess(symbols.markStorageAsInitialized(id))
        case StoreExpr(id, true) if symbols.isInitialized(id) 	=> CheckError("You can only once initialized a store.", expr)
        case StoreExpr(id, false) if symbols.isConst(id) 		=> CheckError("Its only allowed to write once to a const.", expr)
        case StoreExpr(id, false) if symbols.isInitialized(id) 	=> CheckSuccess(symbols)
        case other 												=> CheckError("Use of not initalized store", expr)
    }

    private def combineToResult(r1: CheckResult[SymbolTable], r2: CheckResult[SymbolTable]): CheckResult[SymbolTable] = r1 match {
        case e: CheckError[SymbolTable] => e
        case r => r2
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