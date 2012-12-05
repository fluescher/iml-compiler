package ch.fhnw.iml.checker

import CheckError.apply
import CheckSuccess.apply
import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.Node
import ch.fhnw.iml.ast.ProgramNode
import ch.fhnw.iml.ast.StoreDecl
import ch.fhnw.iml.ast.Type
import ch.fhnw.iml.ast.Global

object TypeChecker extends Checker {
	override def apply(ast: AST) = {
	    typeCheck(ast.root) match {
	        case TypeCheckSuccess(_) 	=> CheckSuccess(null)
	        case TypeCheckError(m,n)	=> CheckError(m,n)
	    }
	}
	
	def typeCheck(n: Node) : TypeCheckResult = n match {
	    case ProgramNode(_, c, block) 		=> typeCheck(c)
	    case StoreDecl(change, id, t)		=> TypeCheckSuccess(t)
	}
	
	abstract class TypeCheckResult
	case class TypeCheckSuccess(t: Type) extends TypeCheckResult
	case class TypeCheckError(msg: String, n: Node) extends TypeCheckResult
}