package ch.fhnw.iml.ast

import scala.util.parsing.input.Positional

case class AST(val root: ProgramNode)

trait Node extends Positional

/* programs */
case class ProgramNode(i: Ident, cps: CpsDecl, cmd: BlockCommand, symbols: SymbolTable) extends Node

/* Commands */
sealed abstract class Command extends Node
case class BlockCommand(cmds: List[Command]) extends Command
case object SkipCommand extends Command
case class AssiCommand(expr1: Expr, expr2: Expr) extends Command
case class CondCommand(expr: Expr, cmd1: Command, cmd2: Command) extends Command
case class WhileCommand(expr: Expr, cmd: Command) extends Command
case class ProcCallCommand(f: Ident, exprs: List[Expr], idents: List[Ident]) extends Command
case class InputCommand(expr: Expr) extends Command
case class OutputCommand(expr: Expr) extends Command

/* Expressions */
sealed abstract class Expr extends Node
case class BoolLiteralExpression(value: Boolean) extends Expr
case class IntLiteralExpression(value: Int) extends Expr
case class StoreExpr(i: Ident, isInitialization: Boolean) extends Expr
case class FunCallExpr(i: Ident, exprs: List[Expr], oldPos: Int = 0) extends Expr
case class MonadicExpr(opr: Opr, expr: Expr) extends Expr
case class DyadicExpr(opr: Opr, expr1: Expr, expr2: Expr) extends Expr

/* Operators */
sealed abstract class Opr extends Node
sealed abstract class BoolOpr extends Opr
case object NotOpr extends BoolOpr
case object AndOpr extends BoolOpr
case object OrOpr extends BoolOpr

sealed abstract class RelOpr extends Opr
case object EqualsOpr extends RelOpr
case object NotEqualsOpr extends RelOpr
case object GreaterThanOpr extends RelOpr
case object GreaterEqualsThanOpr extends RelOpr
case object LessThanOpr extends RelOpr
case object LessEqualsThanOpr extends RelOpr

sealed abstract class ArithOpr extends Opr
case object PlusOpr extends ArithOpr
case object MinusOpr extends ArithOpr
case object TimesOpr extends ArithOpr
case object DivOpr extends ArithOpr
case object ModOpr extends ArithOpr

/* types */
sealed abstract class Type
case object Int32 extends Type
case object Bool extends Type
case object Void extends Type

/* flow control */
sealed abstract class Flow extends Node
case object InOutFlow extends Flow
case object OutFlow extends Flow
case object InFlow extends Flow

/* Ident */
case class Ident(chars: String) extends Positional

/* Declarations */
sealed abstract class Decl extends Node
case class StoreDecl(change: ChangeMode, i: Ident, t: Type) extends Decl
case class CpsDecl(decls: List[Decl]) extends Decl
case class FunDecl(head: FunHead, global: Option[GlobalImportList], cps: Option[CpsDecl], pre: Option[ConditionList], post: Option[ConditionList], cmd: BlockCommand, symbols: SymbolTable) extends Decl
case class ProcDecl(head: ProcHead, global: Option[GlobalImportList], cps: Option[CpsDecl], pre: Option[ConditionList], post: Option[ConditionList], cmd: BlockCommand, symbols: SymbolTable) extends Decl

case class FunHead(i: Ident, params: ParameterList, retVal: StoreDecl) extends Node
case class ProcHead(i: Ident, params: ParameterList) extends Node

/* Conditions */
case class Condition(name: Option[Ident], expr: Expr) extends Node
case class ConditionList(conditions: List[Condition]) extends Node

/* Parameter  */
sealed abstract class Param extends Node
case class GlobalImport(flow: Flow, change: ChangeMode, i: Ident) extends Param
case class GlobalImportList(globals: List[GlobalImport]) extends Param
case class Parameter(flow: Flow, mech: MechMode, store: StoreDecl) extends Param
case class ParameterList(params: List[Parameter]) extends Param

/* Change Mode */
sealed abstract class ChangeMode extends Positional
case object VarNode extends ChangeMode
case object ConstNode extends ChangeMode

sealed abstract class MechMode extends ChangeMode
case object Ref extends MechMode
case object Copy extends MechMode

sealed abstract class Scope
case object Local extends Scope
case object Global extends Scope

/* Symbol tables */
case class SymbolTable(functions: Map[Ident,FunctionSymbol], procs: Map[Ident,ProcedureSymbol], stores: Map[Ident,StorageSymbol], global: SymbolTable) {
    
    def containsStore(id: Ident) = stores.contains(id)
    def getStoreType(id: Ident) = stores.get(id) match {
        case Some(s) => s.t
        case None => Void
    }
    def containsFunction(id: Ident) = functions.contains(id)
    def getFunctionType(id: Ident) = functions.get(id) match {
        case Some(f) => f.decl.head.retVal.t
        case None	  => Void
    }
    def containsProcedure(id: Ident) = procs.contains(id)
    
    def markStorageAsInitialized(id: Ident): SymbolTable = stores.get(id) match {
        case None => this
        case Some(StorageSymbol(a, b, c, d, e, f, g, h, j, isInit)) => SymbolTable(functions, procs, stores.updated(id, StorageSymbol(a,b,c,d,e,f,g,h,j,true)), global)
    }
    
    def isInitialized(id: Ident): Boolean = stores.get(id) match {
        case None => false
        case Some(StorageSymbol(_, _, _, _, _, _, _, _, _, isInit)) => isInit
    }
    
    def isConst(id: Ident): Boolean = stores.get(id) match {
        case None => false
        case Some(StorageSymbol(_, _, decl, _, _, _, _, _, _, _)) => decl.change == ConstNode
    }
    
    def getFunctionDeclaration(id: Ident):FunDecl = {
		val tab = if(global == null) functions else global.functions
	    tab.get(id) match {
	        case Some(FunctionSymbol(_,_,d)) => d
	        case None => null
		}
    }
    
    def getProcedureDeclaration(id: Ident):ProcDecl = {
		val tab = if(global == null) procs else global.procs
	    tab.get(id) match {
	        case Some(ProcedureSymbol(_,d)) => d
	        case None => null
		}
    }
    
    def getStore(id: Ident): StorageSymbol =  stores.get(id).getOrElse(null)
}
object EmptyTable extends SymbolTable(Map.empty, Map.empty, Map.empty, null)

case class ProcedureSymbol(id: Ident, decl: ProcDecl)
case class FunctionSymbol(id: Ident, t: Type, decl: FunDecl)
case class StorageSymbol(id: Ident, t: Type, decl: StoreDecl, isRet: Boolean, isGlobal: Boolean, isArgument: Boolean, isOut: Boolean, argpos: Int, localpos: Int, isInitialized: Boolean)

