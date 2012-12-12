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
        println(n.symbols)
        println(n.cmd)
        println(n.cps)
        checkBlock(true)(n.cmd)(n.symbols)
        // checkDeclList(true)(n.cps.decls)(n.symbols)
    }

    private def checkBlock(initAllowed: Boolean)(block: BlockCommand)(implicit symbols: SymbolTable): CheckResult[SymbolTable] = {
       checkCommandList(initAllowed)(block.cmds)(symbols)
    }
    
    private def checkCommandList(initAllowed: Boolean) (cmds: List[Command]) (symbols: SymbolTable) : CheckResult[SymbolTable] = cmds match {
        case Nil 	=> CheckSuccess(symbols)
        case x::xs	=> checkCommand(initAllowed)(x)(symbols) match {
            case CheckSuccess(symbls) => checkCommandList(initAllowed)(xs)(symbls)
            case e => e
        }
    }
    
    private def checkCommand(initAllowed: Boolean)(cmd: Command)(symbols: SymbolTable): CheckResult[SymbolTable] = cmd match {
        case block: BlockCommand 			=> checkBlock(initAllowed)(block)(symbols)
        case SkipCommand 					=> CheckSuccess(symbols)
        case AssiCommand(left, right)		=> ignoreSecond(checkLeftExpr(left)(symbols), checkValueExpr(right)(symbols))
        case CondCommand(expr, cmd1, cmd2) 	=> ignoreSecond(mergeResults(checkCommand(true)(cmd1)(symbols), checkCommand(true)(cmd2)(symbols)), checkValueExpr(expr)(symbols))  
        case WhileCommand(expr, cmd) 		=> ignoreSecond(checkCommand(true)(cmd)(symbols), checkValueExpr(expr)(symbols))
        // case p: ProcCallCommand 			=> checkProcCall(p)
        case InputCommand(expr) 			=> checkLeftExpr(expr)(symbols)
        case OutputCommand(expr) 			=> checkValueExpr(expr)(symbols)
    }

    private def mergeResults(r1: CheckResult[SymbolTable], r2: CheckResult[SymbolTable]): CheckResult[SymbolTable] = r1 match{
        case e: CheckError[SymbolTable] => e
        case CheckSuccess(symbolsR1) => r2 match {
        	case e: CheckError[SymbolTable] => e
        	case CheckSuccess(symbolsR2) => CheckSuccess(mergeSymbols(symbolsR1, symbolsR2))
        }
    }

    private def mergeSymbols(one: SymbolTable, other: SymbolTable) : SymbolTable = {
    	one
    }
    
    private def checkValueExpr(expr: Expr)(symbols: SymbolTable): CheckResult[SymbolTable] = expr match {
        case BoolLiteralExpression(_) 								=> CheckSuccess(symbols)
        case IntLiteralExpression(_) 								=> CheckSuccess(symbols)
        case VarAccess(id) 			if symbols.isInitialized(id) 	=> CheckSuccess(symbols)
        case va: VarAccess 											=> CheckError("Use of not initialized var", va)
        case m: MonadicExpr				 							=> checkValueExpr(m.expr)(symbols)
        case d: DyadicExpr 											=> ignoreSecond(checkValueExpr(d.expr1)(symbols), checkValueExpr(d.expr1)(symbols))
        case StoreExpr(_,_) 										=> CheckError("No initalization allowed", expr)
        // case f: FunCallExpr										=> checkFunCall(f)
    }

    private def checkLeftExpr(expr: Expr)(symbols: SymbolTable): CheckResult[SymbolTable] = expr match {
    	case StoreExpr(id, true) 	if !symbols.isInitialized(id) 		=> 	println("left init")
        																	CheckSuccess(symbols.markStorageAsInitialized(id))
        case StoreExpr(id, true) 	if symbols.isInitialized(id) 		=> 	CheckError("You can only once initialized a store.", expr)
        case StoreExpr(id, _) 		if symbols.isInitialized(id) 		=> 	CheckSuccess(symbols)
        case se: StoreExpr 												=> 	CheckError("Use of not initalized store", se)
        case other 														=> 	CheckSuccess(symbols)
    }
    
    private def checkDeclList(initAllowed: Boolean) (cps: List[Decl]) (symbols: SymbolTable) : CheckResult[SymbolTable] = cps match {
        case Nil	=> CheckSuccess(symbols)
        case x::xs	=> checkDecl(initAllowed)(x)(symbols) match {
            case CheckSuccess(symbls) => checkDeclList(initAllowed)(xs)(symbls)
            case e => e
        }
    }
    
    private def checkDecl(initAllowed: Boolean)(decl: Decl)(symbols: SymbolTable): CheckResult[SymbolTable] = decl match {
        case other => CheckSuccess(symbols)
    }
    
    /*
	private def checkFunCall(f: FunCallExpr)(implicit symbols: SymbolTable): CheckResult[Type] = {
	        checkFunctionAttributes(f)(symbols.functions.get(f.i).get.decl.head.params.params, f.exprs) match {
	                case CheckSuccess(_) => CheckSuccess(symbols.getFunctionType(f.i)) 
	                case e => e
	        }
	    }
	}*/

    private def ignoreSecond(r1: CheckResult[SymbolTable], r2: CheckResult[SymbolTable]): CheckResult[SymbolTable] = r1 match {
        case e: CheckError[SymbolTable] => e
        case r => r1
    }

}