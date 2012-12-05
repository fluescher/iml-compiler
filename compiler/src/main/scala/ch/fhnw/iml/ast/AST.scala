package ch.fhnw.iml.ast

import scala.util.parsing.input.Positional

case class AST(val root: ProgramNode)

trait Node extends Positional

/* programs */
case class ProgramNode(i: IdentNode, cps: CpsDecl, cmd: BlockCommand, symbols: SymbolTable) extends Node

/* Commands */
sealed abstract class Command extends Node
case class BlockCommand(cmds: List[Command]) extends Command
case object SkipCommand extends Command
case class AssiCommand(expr1: Expr, expr2: Expr) extends Command
case class CondCommand(expr: Expr, cmd1: Command, cmd2: Command) extends Command
case class WhileCommand(expr: Expr, cmd: Command) extends Command
case class ProcCallComand(f: IdentNode, exprs: List[Expr], idents: List[IdentNode]) extends Command
case class InputCommand(expr: Expr) extends Command
case class OutputCommand(expr: Expr) extends Command

/* Expressions */
sealed abstract class Expr extends Node
case class BoolLiteralExpression(value: Boolean) extends Expr
case class IntLiteralExpression(value: Int) extends Expr
case class StoreExpr(i: IdentNode, isInitialization: Boolean) extends Expr
case class VarAccess(i: IdentNode) extends Expr
case class FunCallExpr(i: IdentNode, exprs: List[Expr]) extends Expr
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
sealed abstract class Type
case object Int32 extends Type
case object Bool extends Type

/* flow control */
sealed abstract class Flow extends Node
case object InOutFlow extends Flow
case object OutFlow extends Flow
case object InFlow extends Flow

/* Ident */
case class IdentNode(chars: String) extends Node

/* Declarations */
sealed abstract class Decl extends Node
case class StoreDecl(change: ChangeModeNode, i: IdentNode, t: Type) extends Decl
case class CpsDecl(decls: List[Decl]) extends Decl
case class FunDecl(head: FunHead, global: Option[GlobalImportList], cps: Option[CpsDecl], pre: Option[ConditionList], post: Option[ConditionList], cmd: BlockCommand, symbols: SymbolTable) extends Decl
case class ProcDecl(head: ProcHead, global: Option[GlobalImportList], cps: Option[CpsDecl], pre: Option[ConditionList], post: Option[ConditionList], cmd: BlockCommand, symbols: SymbolTable) extends Decl

case class FunHead(i: IdentNode, params: ParameterList, store: StoreDecl) extends Node
case class ProcHead(i: IdentNode, params: ParameterList) extends Node

/* Conditions */
case class Condition(name: Option[IdentNode], expr: Expr) extends Node
case class ConditionList(conditions: List[Condition]) extends Node

/* Parameter  */
sealed abstract class Param extends Node
case class GlobalImport(flow: Flow, change: ChangeModeNode, i: IdentNode) extends Param
case class GlobalImportList(globals: List[GlobalImport]) extends Param
case class Parameter(flow: Flow, mech: MechModeNode, store: StoreDecl) extends Param
case class ParameterList(params: List[Parameter]) extends Param

/* Change Mode */
sealed abstract class ChangeModeNode extends Node
case object VarNode extends ChangeModeNode
case object ConstNode extends ChangeModeNode

sealed abstract class MechModeNode extends ChangeModeNode
case object RefNode extends MechModeNode
case object CopyNode extends MechModeNode

sealed abstract class Scope
case object Local extends Scope
case object Global extends Scope

/* Symbol tables */
case class SymbolTable(functions: Map[IdentNode,FunctionSymbol], stores: Map[IdentNode,StorageSymbol])
object EmptyTable extends SymbolTable(Map.empty, Map.empty)

case class FunctionSymbol
case class StorageSymbol

