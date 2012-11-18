package ch.fhnw.iml.ast

class AST(root: Node) {

}

trait Node

/* programs */
case class ProgramAst(i: IdentAst, cps: CpsDecl, cmd: BlockCommand) extends Node

/* Commands */
sealed abstract class Command extends Node
case class BlockCommand(cmds: List[Command]) extends Command
case class SkipCommand extends Command
case class AssiCommand(expr1: Expr, expr2: Expr) extends Command
case class CondCommand(expr: Expr, cmd1: Command, cmd2: Command) extends Command
case class WhileCommand(expr: Expr, cmd: Command) extends Command
case class ProcCallComand(f: IdentAst, exprs: List[Expr], idents: List[IdentAst]) extends Command
case class InputCommand(expr: Expr) extends Command
case class OutputCommand(expr: Expr) extends Command

/* Expressions */
sealed abstract class Expr extends Node
case class BoolLiteralExpression(value: Boolean) extends Expr
case class IntLiteralExpression(value: Int) extends Expr
case class StoreExpr(i: IdentAst, isInitialization: Boolean) extends Expr
case class VarAccess(i: IdentAst) extends Expr
case class FunCallExpr(i: IdentAst, exprs: List[Expr]) extends Expr
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

/* Declarations */
sealed abstract class Decl extends Node
case class StoreDecl(change: ChangeModeAst, i: IdentAst, t: TypeAst) extends Decl
case class FunDecl(head: FunHead, global: GlobalImportList, cps: CpsDecl, cmd: BlockCommand) extends Decl
case class ProcDecl(head: ProcHead, global: GlobalImportList, cps: CpsDecl, cmd: BlockCommand) extends Decl
case class CpsDecl(decls: List[Decl]) extends Decl

case class FunHead(i: IdentAst, params: ParameterList, store: StoreDecl)
case class ProcHead(i: IdentAst, params: ParameterList)

/* Parameter  */
sealed abstract class Param extends Node
case class GlobalImport(flow: Flow, change: ChangeModeAst, i: IdentAst) extends Param
case class GlobalImportList(globals: List[GlobalImport]) extends Param
case class Parameter(flow: Flow, mech: MechModeAst, store: StoreDecl) extends Param
case class ParameterList(params: List[Parameter]) extends Param

/* Change Mode */
sealed abstract class ChangeModeAst extends Node
case object VarAst extends ChangeModeAst
case object ConstAst extends ChangeModeAst

sealed abstract class MechModeAst extends ChangeModeAst
case object RefAst extends MechModeAst
case object CopyAst extends MechModeAst