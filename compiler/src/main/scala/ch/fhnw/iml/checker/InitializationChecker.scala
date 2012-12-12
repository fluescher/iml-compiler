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
        checkConditions(f.pre)(markInParameters(f.head.params.params)(f.symbols))  match {
            case CheckSuccess(symbls) 		=> checkBlock(true)(f.cmd)(symbls) match {
                case CheckSuccess(sym) 		=> checkConditions(f.post)(sym) match {
                    case CheckSuccess(s)	=> checkReturnValue(f.head.retVal)(s)
                    case e => e
                }
                case e => e
            }
            case e => e
        }
    }
    
   private def checkConditions(conditions: Option[ConditionList])(symbols: SymbolTable): CheckResult[SymbolTable] = conditions match {
	    case None 		=> CheckSuccess(symbols)
	    case Some(l)	=> l.conditions.map({case Condition(_, expr) => expr})
	    							   .map(a => (checkValueExpr(a)(symbols)))
	    							   .foldLeft(CheckSuccess[SymbolTable](symbols):CheckResult[SymbolTable])(combineToResult)
	}

    private def checkOutRef(params: List[Parameter])(symbols: SymbolTable): CheckResult[SymbolTable] = {
        params match {
            case Nil => CheckSuccess(symbols)
            case Parameter(OutFlow,_,storeDecl) :: xs => checkReturnValue(storeDecl)(symbols) match {
                case CheckSuccess(_) => checkOutRef(xs)(symbols)
                case e => e
            }
            case Parameter(_,_,_) :: xs => checkOutRef(xs)(symbols)
            case other	 => CheckSuccess(symbols)
        }
    }

    private def checkReturnValue(retVal: StoreDecl)(symbols: SymbolTable): CheckResult[SymbolTable] = retVal match {
        case StoreDecl(_, id, _) if symbols.isInitialized(id) => CheckSuccess(symbols)
        case other => CheckError("Return value must be initialized at the end of a function.", retVal)
    }

    private def checkProcDecls(n: ProgramNode): CheckResult[SymbolTable] = {
        n.cps.decls.filter(_.isInstanceOf[ProcDecl])
            .map(_.asInstanceOf[ProcDecl])
            .map(checkProcDecl)
            .foldLeft(CheckSuccess[SymbolTable](n.symbols): CheckResult[SymbolTable])(ignoreFirst)
    }

    private def checkProcDecl(p: ProcDecl): CheckResult[SymbolTable] = {
        checkConditions(p.pre)(markInParameters(p.head.params.params)(p.symbols))  match {
            case CheckSuccess(symbls) 		=> checkBlock(true)(p.cmd)(symbls) match {
                case CheckSuccess(sym) 		=> checkConditions(p.post)(sym) match {
                    case CheckSuccess(s)	=> checkOutRef(p.head.params.params)(s)
                    case e => e
                }
                case e => e
            }
            case e => e
        }
    }
    
    private def markInParameters(params: List[Parameter])(symbols: SymbolTable) : SymbolTable = params match {
        case Nil => symbols
        case Parameter(InFlow,_,storeDecl) :: xs 	=> markInParameters(xs)(symbols.markStorageAsInitialized(storeDecl.i))
        case Parameter(InOutFlow,_,storeDecl) :: xs => markInParameters(xs)(symbols.markStorageAsInitialized(storeDecl.i))
        case Parameter(_,_,storeDecl) :: xs			=> markInParameters(xs)(symbols)
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
        case p: ProcCallCommand 			=> checkProcCall(initAllowed)(p.exprs)(symbols)
        case InputCommand(expr) 			=> checkLeftExpr(initAllowed)(expr)(symbols)
        case OutputCommand(expr) 			=> checkValueExpr(expr)(symbols)
    }
    
    private def checkProcCall(initAllowed: Boolean)(exprs: List[Expr])(symbols: SymbolTable) : CheckResult[SymbolTable] = {
        exprs match {
            case Nil => CheckSuccess(symbols)
            case x :: xs => x match {
                case StoreExpr(_, _) => checkLeftExpr(initAllowed)(x)(symbols)  match {
                     case CheckSuccess(sym) => checkProcCall(initAllowed)(xs)(sym)
                     case e => e
                }
                case other => checkValueExpr(x)(symbols) match {
                     case CheckSuccess(sym) => checkProcCall(initAllowed)(xs)(sym)
                     case e => e
                }
            }
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
        case BoolLiteralExpression(_) 					=> CheckSuccess(symbols)
        case IntLiteralExpression(_) 					=> CheckSuccess(symbols)
        case VarAccess(id) if symbols.isInitialized(id) => CheckSuccess(symbols)
        case va: VarAccess 								=> CheckError("Use of not initialized var", va)
        case m: MonadicExpr 							=> checkValueExpr(m.expr)(symbols)
        case d: DyadicExpr 								=> ignoreSecond(checkValueExpr(d.expr1)(symbols), checkValueExpr(d.expr1)(symbols))
        case StoreExpr(_, _) 							=> CheckError("No initalization allowed", expr)
    }

    private def checkLeftExpr(initAllowed: Boolean)(expr: Expr)(symbols: SymbolTable): CheckResult[SymbolTable] = expr match {
        case StoreExpr(id, true) 	if !initAllowed 					=> CheckError("Its not allowed to initalized a store in this command.", expr)
        case StoreExpr(id, true) 	if !symbols.isInitialized(id) 		=> CheckSuccess(symbols.markStorageAsInitialized(id))													   
        case StoreExpr(id, true) 	if symbols.isInitialized(id) 		=> CheckError("You can only once initialized a store.", expr)
        case StoreExpr(id, _) 		if symbols.isInitialized(id) 		=> CheckSuccess(symbols)
        case VarAccess(id) 			if symbols.isInitialized(id) 		=> CheckSuccess(symbols)
        case other 														=> CheckError("Use of not initalized store", expr)
    }
    
    private def combineToResult(r1: CheckResult[SymbolTable], r2: CheckResult[SymbolTable]): CheckResult[SymbolTable] = r1 match {
	    case e: CheckError[SymbolTable]		=> e
	    case r							=> r2
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