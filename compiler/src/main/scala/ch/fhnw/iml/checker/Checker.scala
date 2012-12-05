package ch.fhnw.iml.checker

import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.Node
import ch.fhnw.iml.ast.ChangeMode
import ch.fhnw.iml.ast.Type
import ch.fhnw.iml.ast.Scope

trait Checker {
	def apply(ast: AST) : CheckResult[AST]
	
	def combine[A](f1: (A => CheckResult[A]), f2: (A => CheckResult[A]))(a: A): CheckResult[A] = f1(a) match {
	    case CheckSuccess(a) => f2(a)
	    case e => e
	}
}

abstract class CheckResult[A]
case class CheckSuccess[A](a: A) extends CheckResult[A]
case class CheckError[A](msg: String, node: Node) extends CheckResult[A]
