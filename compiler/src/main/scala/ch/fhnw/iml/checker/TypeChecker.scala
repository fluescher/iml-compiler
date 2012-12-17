package ch.fhnw.iml.checker

import CheckError.apply
import CheckSuccess.apply
import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.ArithOpr
import ch.fhnw.iml.ast.AssiCommand
import ch.fhnw.iml.ast.BlockCommand
import ch.fhnw.iml.ast.Bool
import ch.fhnw.iml.ast.BoolLiteralExpression
import ch.fhnw.iml.ast.BoolOpr
import ch.fhnw.iml.ast.Command
import ch.fhnw.iml.ast.CondCommand
import ch.fhnw.iml.ast.Condition
import ch.fhnw.iml.ast.ConditionList
import ch.fhnw.iml.ast.Copy
import ch.fhnw.iml.ast.DyadicExpr
import ch.fhnw.iml.ast.EqualsOpr
import ch.fhnw.iml.ast.Expr
import ch.fhnw.iml.ast.FunCallExpr
import ch.fhnw.iml.ast.FunDecl
import ch.fhnw.iml.ast.InputCommand
import ch.fhnw.iml.ast.Int32
import ch.fhnw.iml.ast.IntLiteralExpression
import ch.fhnw.iml.ast.MinusOpr
import ch.fhnw.iml.ast.MonadicExpr
import ch.fhnw.iml.ast.Node
import ch.fhnw.iml.ast.NotEqualsOpr
import ch.fhnw.iml.ast.NotOpr
import ch.fhnw.iml.ast.OutputCommand
import ch.fhnw.iml.ast.Parameter
import ch.fhnw.iml.ast.PlusOpr
import ch.fhnw.iml.ast.ProcCallCommand
import ch.fhnw.iml.ast.ProcDecl
import ch.fhnw.iml.ast.ProgramNode
import ch.fhnw.iml.ast.Ref
import ch.fhnw.iml.ast.RelOpr
import ch.fhnw.iml.ast.SkipCommand
import ch.fhnw.iml.ast.StoreDecl
import ch.fhnw.iml.ast.StoreExpr
import ch.fhnw.iml.ast.SymbolTable
import ch.fhnw.iml.ast.Type
import ch.fhnw.iml.ast.Void
import ch.fhnw.iml.ast.WhileCommand
import ch.fhnw.iml.ast.InFlow
import ch.fhnw.iml.ast.OutFlow
import ch.fhnw.iml.ast.InOutFlow

object TypeChecker extends Checker {
	override def apply(ast: AST) = {
	    typeCheck(ast.root) match {
	        case CheckSuccess(_) 	=> CheckSuccess(ast)
	        case CheckError(m,n)	=> CheckError(m,n)
	    }
	}
	
	/**
	 * Determines the resulting type of an expression.
	 * 
	 * This function is only safe, if the expression was 
	 * previously validated using this type checker.
	 */
	def getType(expr: Expr)(implicit symbols: SymbolTable): Type = expr match {
	    case BoolLiteralExpression(_) 		=> Bool
	    case IntLiteralExpression(_) 		=> Int32
	    case StoreExpr(i,_)					=> symbols.getStoreType(i)
	    case FunCallExpr(i, _, _)			=> symbols.getFunctionType(i)
	    case MonadicExpr(o: ArithOpr, _)	=> Int32
	    case MonadicExpr(o: BoolOpr, _)		=> Bool
	    case MonadicExpr(o: RelOpr,_)		=> throw new UnsupportedOperationException("This function can only be called if typecheck was successful") /* This should never happen. Statement only inserted because of scala compiler warning */
	    case DyadicExpr(o: BoolOpr, _, _)	=> Bool
	    case DyadicExpr(o: RelOpr, _, _)	=> Bool
	    case DyadicExpr(o: ArithOpr,_ ,_)	=> Int32
	}
	
	private def typeCheck(n: ProgramNode): CheckResult[Type] = checkBlock(true)(n)(n.cmd)(n.symbols) and checkFunDecls(n) and checkProcDecls(n)
	
	private def checkFunDecls(implicit n: ProgramNode): CheckResult[Type] = 
	    n.cps.decls	.filter(_.isInstanceOf[FunDecl])
	    			.map(_.asInstanceOf[FunDecl])
	    			.map(checkFunDecl)
	    			.foldLeft(CheckSuccess[Type](Void):CheckResult[Type])(combineToResult)
	
	private def checkFunDecl(f: FunDecl)(implicit p: ProgramNode): CheckResult[Type] = 
	    checkBlock(false)(p)(f.cmd)(f.symbols) and checkConditions(f.pre)(TypeCheckScope(p, false, f.symbols)) and checkConditions(f.post)(TypeCheckScope(p, true, f.symbols))
	
	private def checkProcDecls(implicit n: ProgramNode): CheckResult[Type] =  
	    n.cps.decls	.filter(_.isInstanceOf[ProcDecl])
	    			.map(_.asInstanceOf[ProcDecl])
	    			.map(checkProcDecl(n))
	    			.foldLeft(CheckSuccess[Type](Void):CheckResult[Type])(combineToResult)
	
	private def checkProcDecl(prog: ProgramNode)(p: ProcDecl): CheckResult[Type] = 
	    checkBlock(false)(prog)(p.cmd)(p.symbols) 	and checkConditions(p.pre)(TypeCheckScope(prog, false, p.symbols)) and checkConditions(p.post)(TypeCheckScope(prog, true, p.symbols))
	
	private def checkBlock(inMain: Boolean)(p: ProgramNode)(block: BlockCommand)(implicit symbols: SymbolTable): CheckResult[Type] = {
	    block.cmds.map(checkCommand(inMain)(p)).foldLeft(CheckSuccess[Type](Bool):CheckResult[Type])(combineToResult)
	}
	
	private def checkConditions(conditions: Option[ConditionList])(implicit scope: TypeCheckScope): CheckResult[Type] = conditions match {
	    case None 		=> CheckSuccess(Void)
	    case Some(l)	=> l.conditions.map({case Condition(_, expr) => expr})
	    							   .map(a => checkType(Bool)(a)(checkValueExpr(a)))
	    							   .foldLeft(CheckSuccess[Type](Bool):CheckResult[Type])(combineToResult)
	}
	
	private def checkCommand(inMain: Boolean = false)(prog: ProgramNode)(cmd: Command)(implicit symbols: SymbolTable):CheckResult[Type] = cmd match {
	    case block: BlockCommand 			=> checkBlock(inMain)(prog)(block)
	    case SkipCommand 					=> CheckSuccess(Void)
	    case AssiCommand(left, right) 		=> checkEqualTypes(left)(checkLeftExpr(left), checkValueExpr(right)(TypeCheckScope(prog, false, symbols)))
	    case CondCommand(expr, cmd1, cmd2)	=> checkType(Bool)(expr)(checkValueExpr(expr)(TypeCheckScope(prog, false, symbols))) and checkCommand(inMain)(prog)(cmd1) and checkCommand(inMain)(prog)(cmd2)
	    case WhileCommand(expr, cmd)		=> checkType(Bool)(expr)(checkValueExpr(expr)(TypeCheckScope(prog, false, symbols))) and checkCommand(inMain)(prog)(cmd)
	    case p: ProcCallCommand				=> checkProcCall(p)(TypeCheckScope(prog, false, symbols))
	    case i: InputCommand	if !inMain  => CheckError("IO only allowed in main block", i)
	    case i: OutputCommand	if !inMain  => CheckError("IO only allowed in main block", i)
	    case InputCommand(expr)				=> checkLeftExpr(expr)
	    case OutputCommand(expr)			=> checkValueExpr(expr)(TypeCheckScope(prog, false, symbols))
	}
	
	private def checkProcCall(p: ProcCallCommand)(implicit scope: TypeCheckScope): CheckResult[Type] = {
	    if(!scope.p.symbols.containsProcedure(p.f)) CheckError("Call to undefined procedure: " + p.f.chars, p)
	    else {
            checkFunctionAttributes(p)(scope.p.symbols.procs.get(p.f).get.decl.head.params.params, p.exprs) match {
                case CheckSuccess(_) => CheckSuccess(Void) 
                case e => e
            }
	    }
	}
	
	private def checkLeftExpr(expr: Expr)(implicit symbols: SymbolTable) :CheckResult[Type] = expr match {
	    case StoreExpr(id,_) 	if symbols.containsStore(id) => CheckSuccess(symbols.getStoreType(id))
	    case se: StoreExpr									 => CheckError("Use of undeclared store", se)
	    case other 											 => CheckError("Invalid store reference", other)
	}
	
	private def checkValueExpr(expr: Expr)(implicit scope: TypeCheckScope) :CheckResult[Type] = expr match {
	    case BoolLiteralExpression(_) 									=> CheckSuccess(Bool)
	    case IntLiteralExpression(_) 									=> CheckSuccess(Int32)
	    case StoreExpr(id, true)	 									=> CheckError("No init on right side allowed", expr)
	    case StoreExpr(id, false) 	if scope.symbols.containsStore(id)	=> CheckSuccess(scope.symbols.getStoreType(id))
	    case StoreExpr(id, _)											=> CheckError("Use of undeclared store.", expr)
	    case m: MonadicExpr	 											=> checkMonadicExpr(m)
	    case d: DyadicExpr												=> checkDyadicExpr(d)
	    case f: FunCallExpr												=> checkFunCall(f)
	}
	
	private def checkDyadicExpr(d: DyadicExpr)(implicit symbols: TypeCheckScope): CheckResult[Type] = d match {
	    case DyadicExpr(opr, expr1, expr2) => opr match {
	        case o: ArithOpr	=> checkType(Int32)(d)(checkEqualTypes(d)(checkValueExpr(expr1), checkValueExpr(expr2)))
	        case o: BoolOpr		=> checkType(Bool)(d)(checkEqualTypes(d)(checkValueExpr(expr1), checkValueExpr(expr2)))
	        case EqualsOpr		=> toOtherTypeResult(Bool)(checkEqualTypes(d)(checkValueExpr(expr1), checkValueExpr(expr2)))
	        case NotEqualsOpr	=> toOtherTypeResult(Bool)(checkEqualTypes(d)(checkValueExpr(expr1), checkValueExpr(expr2)))
	        case o: RelOpr		=> toOtherTypeResult(Bool)(checkType(Int32)(d)(checkEqualTypes(d)(checkValueExpr(expr1), checkValueExpr(expr2))))
	    }
	}
	
	private def checkMonadicExpr(m: MonadicExpr)(implicit symbols: TypeCheckScope): CheckResult[Type] = m match {
	    case MonadicExpr(opr, expr) => opr match {
		    case PlusOpr	=> checkType(Int32)(m)(checkValueExpr(expr))
		    case MinusOpr	=> checkType(Int32)(m)(checkValueExpr(expr))
		    case NotOpr		=> checkType(Bool)(m)(checkValueExpr(expr))
		    case other		=> CheckError("Monadic operator expected.", m)
		}
	}
	
	private def checkFunCall(f: FunCallExpr)(implicit scope: TypeCheckScope): CheckResult[Type] = {
	    if(scope.inPost && f.i.chars.equals("old") && f.exprs.size == 1) { /* check for old state fun */
	        checkValueExpr(f.exprs.head)(TypeCheckScope(scope.p, false, scope.symbols))
	    }
	    else if(!scope.p.symbols.containsFunction(f.i)) CheckError("Call to undefined function: " + f.i.chars, f)
	    else {
	        checkFunctionAttributes(f)(scope.p.symbols.functions.get(f.i).get.decl.head.params.params, f.exprs) match {
	                case CheckSuccess(_) => CheckSuccess(scope.p.symbols.getFunctionType(f.i)) 
	                case e => e
	        }
	    }
	}
	
	private def checkFunctionAttributes(n: Node)(decl: List[Parameter], callExprs: List[Expr])(implicit scope: TypeCheckScope): CheckResult[Type] = (decl, callExprs) match {
	    case (Nil, Nil)						=> CheckSuccess(Void)
	    case (x::xs, Nil)					=> CheckError("Not enough arguments for call", n)
	    case (Nil, y::xs)					=> CheckError("To many arguments for call", n)
	    case (x::xs, y::ys)					=> checkParameter(n)(x,y) match {
	        case CheckSuccess(_) 			=> checkFunctionAttributes(n)(xs, ys)
	        case e => e
	    }
	}
	
	private def checkParameter(n: Node)(p: Parameter, expr: Expr)(implicit scope: TypeCheckScope): CheckResult[Type] = p match {
	    case Parameter(InFlow, Copy, StoreDecl(c, i, t)) 	=> checkType(t)(n)(checkValueExpr(expr))
	    case Parameter(InFlow, Ref, StoreDecl(c, i, t)) 		=> checkType(t)(n)(checkLeftExpr(expr)(scope.symbols))
	    case Parameter(OutFlow, Copy, StoreDecl(c, i, t)) 	=> checkType(t)(n)(checkValueExpr(expr))
	    case Parameter(OutFlow, Ref, StoreDecl(c, i, t)) 		=> checkType(t)(n)(checkLeftExpr(expr)(scope.symbols))
	    case Parameter(InOutFlow, Copy, StoreDecl(c, i, t)) 	=> checkType(t)(n)(checkValueExpr(expr))
	    case Parameter(InOutFlow, Ref, StoreDecl(c, i, t)) 		=> checkType(t)(n)(checkLeftExpr(expr)(scope.symbols))
	}
	
	private def checkType(expected: Type)(n: Node)(res: CheckResult[Type]): CheckResult[Type] = res match {
	    case CheckSuccess(t)	if t == expected	=> CheckSuccess(t)
	    case CheckSuccess(t)						=> CheckError(expected + " type expected", n)
	    case e 										=> e
	}
	
	private def checkEqualTypes(n: Node)(res1: CheckResult[Type], res2: CheckResult[Type]): CheckResult[Type] = (res1, res2) match {
        case (CheckSuccess(t1), CheckSuccess(t2))	 if t1 == t2 							=> CheckSuccess(t1)
        case (CheckSuccess(_), CheckSuccess(_))												=> CheckError("Incompatible types", n)
        case (a, _)									 if a.isInstanceOf[CheckError[Type]]	=> a
        case (_, b)									 if b.isInstanceOf[CheckError[Type]]	=> b
    }
	
	private def toOtherTypeResult(t: Type)(res: CheckResult[Type]): CheckResult[Type] = res match {
	    case CheckSuccess(_) => CheckSuccess(t)
	    case e => e
	}
	
	private def combineToResult(r1: CheckResult[Type], r2: CheckResult[Type]): CheckResult[Type] = r1 match {
	    case e: CheckError[Type]		=> e
	    case r							=> r2
	}
	
	case class TypeCheckScope(p: ProgramNode, inPost: Boolean, symbols: SymbolTable)
}