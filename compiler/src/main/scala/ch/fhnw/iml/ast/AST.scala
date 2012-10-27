package ch.fhnw.iml.ast

class AST(root: Node) {
    
}

trait Node

/* programs */

/* Commands */
sealed abstract class Command extends Node
case class BlockCommand(cmds: List[Command]) extends Command