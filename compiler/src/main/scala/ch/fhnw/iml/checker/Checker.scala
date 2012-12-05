package ch.fhnw.iml.checker

import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.Node
import ch.fhnw.iml.ast.ChangeModeNode
import ch.fhnw.iml.ast.Type
import ch.fhnw.iml.ast.Scope

trait Checker {
	def apply(ast: AST) : CheckResult
}

abstract class CheckResult
case class CheckSuccess(ast: AST) extends CheckResult
case class CheckError(msg: String, node: Node) extends CheckResult
