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
	
	def checkOrError[A,B](f: B => CheckResult[A])(res: CheckResult[A], next: B) = res match {
	    case CheckSuccess(_) => f(next)
	    case e => e
	}
	
	def and[A](res1: CheckResult[A], res2: CheckResult[A]) = res1 match {
	    case CheckSuccess(_) => res2 match {
	        case CheckSuccess(l) => CheckSuccess(l)
	    	case e => e
	        }
	    case e => e
	}
}

abstract class CheckResult[A] {
    def and(other: CheckResult[A]): CheckResult[A]
}
case class CheckSuccess[A](a: A) extends CheckResult[A] {
    override def and(other: CheckResult[A]) = {
        other
    }
}
case class CheckError[A](msg: String, node: Node) extends CheckResult[A] {
     override def and(other: CheckResult[A]) = {
        this
    }
}
