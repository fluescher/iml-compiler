package ch.fhnw.iml.ast

class AST(root: Node) {

}

trait Node

/* programs */

/* Commands */
sealed abstract class Command extends Node
case class BlockCommand(cmds: List[Command]) extends Command
case class SkipCommand extends Command
case class AssiCommand(expr1: Expr, expr2: Expr) extends Command
case class CondCommand(expr: Expr, cmd1: Command, cmd2: Command) extends Command
case class WhileCommand(expr: Expr, cmd: Command) extends Command
case class ProcCallComand(f: IdentAst, exprs:List[Expr], idents:List[IdentAst]) extends Command
case class InputCommand(expr: Expr) extends Command
case class OutputCommand(expr: Expr) extends Command
 
/* Expressions */
sealed abstract class Expr extends Node
case class BoolLiteralExpression(value: Boolean) extends Expr
case class IntLiteralExpression(value: Int) extends Expr
case class StoreExpr(i: IdentAst, isInitialization: Boolean) extends Expr
case class VarAccess(i: IdentAst) extends Expr
case class FunCallExpr(i: IdentAst, exprs:List[Expr]) extends Expr
case class MonadicExpr(opr: Opr, expr: Expr) extends Expr
case class DyadicExpr(opr: Opr, expr1: Expr, expr2: Expr) extends Expr

/* Operators */
sealed abstract class Opr extends Node
case object NotOpr extends Opr
case object AndOpr extends Opr
case object OrOpr extends Opr
case object EqualsOpr extends Opr
case object NotEqualsOpr extends Opr
case object GreaterThanOpr extends Opr
case object GreaterEqualsThanOpr extends Opr
case object LessThanOpr extends Opr
case object LessEqualsThanOpr extends Opr
case object PlusOpr extends Opr
case object MinusOpr extends Opr
case object TimesOpr extends Opr
case object DivOpr extends Opr
case object ModOpr extends Opr

/* types */
sealed abstract class TypeAst extends Node
case object Int32Ast extends TypeAst
case object BoolAst extends TypeAst

/* flow control */
sealed abstract class Flow extends Node
case object InOutFlow extends Flow
case object OutFlow extends Flow
case object InFlow extends Flow

/* Ident */
case class IdentAst(chars: String) extends Node

sealed abstract class ChangeModeAst extends Node
case object VarAst extends ChangeModeAst
case object ConstAst extends ChangeModeAst

sealed abstract class MechModeAst extends ChangeModeAst
case object RefAst extends MechModeAst
case object CopyAst extends MechModeAst