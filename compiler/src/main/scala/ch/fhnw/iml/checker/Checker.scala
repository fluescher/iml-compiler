package ch.fhnw.iml.checker

import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.Node
import ch.fhnw.iml.ast.ChangeModeNode
import ch.fhnw.iml.ast.Type
import ch.fhnw.iml.ast.Scope

trait Checker {
    type SymbolTable = Map[String, VarDefinition]
    
	def apply(ast: AST, symbols: SymbolTable) : CheckResult
}

abstract class CheckResult
case class CheckSuccess(tbl: Map[String, VarDefinition]) extends CheckResult
case class CheckError(msg: String, node: Node) extends CheckResult

case class VarDefinition(id: String, t: Type, mode: ChangeModeNode, scope: Scope)