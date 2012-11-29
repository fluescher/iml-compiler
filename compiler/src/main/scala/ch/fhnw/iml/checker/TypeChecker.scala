package ch.fhnw.iml.checker

import CheckError.apply
import CheckSuccess.apply
import VarDefinition.apply
import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.Node
import ch.fhnw.iml.ast.ProgramNode
import ch.fhnw.iml.ast.StoreDecl
import ch.fhnw.iml.ast.Type
import ch.fhnw.iml.ast.Global

object TypeChecker extends Checker {
	override def apply(ast: AST, symbols: SymbolTable) = {
	    typeCheck(ast.root, symbols) match {
	        case TypeCheckSuccess(_,syms) 	=> CheckSuccess(syms)
	        case TypeCheckError(m,n) 		=> CheckError(m,n)
	    }
	}
	
	def typeCheck(n: Node, symbols: SymbolTable) : TypeCheckResult = n match {
	    case ProgramNode(_, c, block) 		=> typeCheck(c, symbols)
	    case StoreDecl(change, id, t)		=> TypeCheckSuccess(t, symbols + (id.chars -> VarDefinition(id.chars, t, change, Global)))
	}
	
	abstract class TypeCheckResult
	case class TypeCheckSuccess(t: Type, symbols: SymbolTable) extends TypeCheckResult
	case class TypeCheckError(msg: String, n: Node) extends TypeCheckResult
}