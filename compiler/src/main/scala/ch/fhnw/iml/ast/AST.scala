package ch.fhnw.iml.ast

class AST(root: Node) {
    
}

trait Node

/* programs */

/* Commands */
sealed abstract class Command extends Node
case class BlockCommand(cmds: List[Command]) extends Command

/* Expressions */
sealed abstract class Expr extends Node
case class IntLiteralExpression(value: Int) extends Expr
case class StoreExpr(i: String, isInitialization: Boolean) extends Expr
case class FunCallExpr /* RoutineCall */extends Expr
case class MonadicExpr(opr: Opr, expr: Expr) extends Expr
case class DyadicExpr(opr: Opr, expr1: Expr, expr2: Expr) extends Expr

/* Operators */
sealed abstract class Opr extends Node
case class Not extends Opr
case class AddOpr extends Opr
case class AndOpr extends Opr
case class OrOpr extends Opr