package ch.fhnw.iml.checker

import ch.fhnw.iml.ast.PlusOpr
import ch.fhnw.iml.ast.SkipCommand
import ch.fhnw.iml.ast.VarAccess
import ch.fhnw.iml.ast.MinusOpr
import ch.fhnw.iml.ast.StoreDecl
import ch.fhnw.iml.ast.StoreExpr
import ch.fhnw.iml.ast.ProgramNode
import ch.fhnw.iml.ast.Command
import ch.fhnw.iml.ast.NotOpr
import ch.fhnw.iml.ast.IntLiteralExpression
import ch.fhnw.iml.ast.MonadicExpr
import ch.fhnw.iml.ast.AssiCommand
import ch.fhnw.iml.ast.BlockCommand
import ch.fhnw.iml.ast.BoolLiteralExpression
import ch.fhnw.iml.ast.Bool
import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.Int32
import ch.fhnw.iml.ast.Expr
import ch.fhnw.iml.ast.Node
import ch.fhnw.iml.ast.SymbolTable
import ch.fhnw.iml.ast.Type
import ch.fhnw.iml.ast.MinusOpr
import ch.fhnw.iml.ast.PlusOpr
import ch.fhnw.iml.ast.Void
import ch.fhnw.iml.ast.DyadicExpr
import ch.fhnw.iml.ast.ArithOpr
import ch.fhnw.iml.ast.BoolOpr
import ch.fhnw.iml.ast.RelOpr
import ch.fhnw.iml.ast.CondCommand
import ch.fhnw.iml.ast.WhileCommand
import ch.fhnw.iml.ast.FunCallExpr
import ch.fhnw.iml.ast.ProcCallCommand
import ch.fhnw.iml.ast.InputCommand
import ch.fhnw.iml.ast.OutputCommand
import ch.fhnw.iml.ast.NotEqualsOpr
import ch.fhnw.iml.ast.EqualsOpr

object TypeChecker extends Checker {
	override def apply(ast: AST) = {
	    typeCheck(ast.root) match {
	        case CheckSuccess(_) 	=> CheckSuccess(null)
	        case CheckError(m,n)	=> CheckError(m,n)
	    }
	}
	
	def typeCheck(n: ProgramNode): CheckResult[Type] = checkBlock(true)(n.cmd)(n.symbols)
	
	def checkBlock(inMain: Boolean)(block: BlockCommand)(implicit symbols: SymbolTable): CheckResult[Type] = {
	    block.cmds.map(checkCommand(inMain)).foldLeft(CheckSuccess[Type](Bool):CheckResult[Type])(combineToResult)
	}
	
	def checkCommand(inMain: Boolean = false)(cmd: Command)(implicit symbols: SymbolTable):CheckResult[Type] = cmd match {
	    case block: BlockCommand 			=> checkBlock(inMain)(block)
	    case SkipCommand 					=> CheckSuccess(Void)
	    case AssiCommand(left, right) 		=> checkEqualTypes(left)(checkLeftExpr(left), checkValueExpr(right))
	    case CondCommand(expr, cmd1, cmd2)	=> checkType(Bool)(expr)(checkValueExpr(expr)) and checkCommand(inMain)(cmd1) and checkCommand(inMain)(cmd2)
	    case WhileCommand(expr, cmd)		=> checkType(Bool)(expr)(checkValueExpr(expr)) and checkCommand(inMain)(cmd)
	    case ProcCallCommand(_,_,_)			=> CheckSuccess(Void) // TODO check proc call
	    case i: InputCommand	if !inMain  => CheckError("IO only allowed in main block", i)
	    case i: OutputCommand	if !inMain  => CheckError("IO only allowed in main block", i)
	    case InputCommand(expr)				=> checkLeftExpr(expr)
	    case OutputCommand(expr)			=> checkValueExpr(expr)
	}
	
	def checkLeftExpr(expr: Expr)(implicit symbols: SymbolTable) :CheckResult[Type] = expr match {
	    case VarAccess(id) 		if symbols.containsStore(id) => CheckSuccess(symbols.getStoreType(id))
	    case va: VarAccess									 => CheckError("Use of undeclared store", va) 
	    case StoreExpr(id,_) 	if symbols.containsStore(id) => CheckSuccess(symbols.getStoreType(id))
	    case se: StoreExpr									 => CheckError("Use of undeclared store", se)
	    case other 											 => CheckError("Invalid left side of assignment", other)
	}
	
	def checkValueExpr(expr: Expr)(implicit symbols: SymbolTable) :CheckResult[Type] = expr match {
	    case BoolLiteralExpression(_) 						=> CheckSuccess(Bool)
	    case IntLiteralExpression(_) 						=> CheckSuccess(Int32)
	    case e: StoreExpr	 								=> CheckError("No init on right side allowed", e)
	    case VarAccess(id) 		if symbols.containsStore(id)=> CheckSuccess(symbols.getStoreType(id))
	    case va: VarAccess									=> CheckError("Use of undeclared store", va)
	    case m: MonadicExpr	 								=> checkMonadicExpr(m) 
	    case d: DyadicExpr									=> checkDyadicExpr(d)
	    case f: FunCallExpr									=> checkFunCall(f)
	}
	
	def checkDyadicExpr(d: DyadicExpr)(implicit symbols: SymbolTable): CheckResult[Type] = d match {
	    case DyadicExpr(opr, expr1, expr2) => opr match {
	        case o: ArithOpr	=> checkType(Int32)(d)(checkEqualTypes(d)(checkValueExpr(expr1), checkValueExpr(expr2)))
	        case o: BoolOpr		=> checkType(Bool)(d)(checkEqualTypes(d)(checkValueExpr(expr1), checkValueExpr(expr2)))
	        case EqualsOpr		=> toOtherTypeResult(Bool)(checkEqualTypes(d)(checkValueExpr(expr1), checkValueExpr(expr2)))
	        case NotEqualsOpr	=> toOtherTypeResult(Bool)(checkEqualTypes(d)(checkValueExpr(expr1), checkValueExpr(expr2)))
	        case o: RelOpr		=> toOtherTypeResult(Bool)(checkType(Int32)(d)(checkEqualTypes(d)(checkValueExpr(expr1), checkValueExpr(expr2))))
	    }
	}
	
	def checkMonadicExpr(m: MonadicExpr)(implicit symbols: SymbolTable): CheckResult[Type] = m match {
	    case MonadicExpr(opr, expr) => opr match {
		    case PlusOpr	=> checkType(Int32)(m)(checkValueExpr(expr))
		    case MinusOpr	=> checkType(Int32)(m)(checkValueExpr(expr))
		    case NotOpr		=> checkType(Bool)(m)(checkValueExpr(expr))
		    case other		=> CheckError("Monadic operator expected.", m)
		}
	}
	
	def checkType(expected: Type)(n: Node)(res: CheckResult[Type]): CheckResult[Type] = res match {
	    case CheckSuccess(t)	if t == expected	=> CheckSuccess(t)
	    case CheckSuccess(t)						=> CheckError(expected + " type expected", n)
	    case e 										=> e
	}
	
	def checkFunCall(f: FunCallExpr)(implicit symbols: SymbolTable): CheckResult[Type] = {
	    if(!symbols.containsFunction(f.i)) CheckError("Call to undefined function: " + f.i.chars, f)
	    else {
	        f.exprs.map(checkValueExpr).foldLeft(CheckSuccess(Void):CheckResult[Type])(combineToResult) match {
	            case CheckSuccess(_) => CheckSuccess(symbols.getFunctionType(f.i)) // TODO check function arguments
	            case e => e
	        }
	    }
	}
	
	def checkEqualTypes(n: Node)(res1: CheckResult[Type], res2: CheckResult[Type]): CheckResult[Type] = {
	    (res1, res2) match {
	        case (CheckSuccess(t1), CheckSuccess(t2))	 if t1 == t2 							=> CheckSuccess(t1)
	        case (CheckSuccess(_), CheckSuccess(_))												=> CheckError("Incompatible types", n)
	        case (a, _)									 if a.isInstanceOf[CheckError[Type]]	=> a
	        case (_, b)									 if b.isInstanceOf[CheckError[Type]]	=> b
	    }
	}
	
	def toOtherTypeResult(t: Type)(res: CheckResult[Type]): CheckResult[Type] = res match {
	    case CheckSuccess(_) => CheckSuccess(t)
	    case e => e
	}
	
	def combineToResult(r1: CheckResult[Type], r2: CheckResult[Type]): CheckResult[Type] = r1 match {
	    case e: CheckError[Type]		=> e
	    case r							=> r2
	}
}