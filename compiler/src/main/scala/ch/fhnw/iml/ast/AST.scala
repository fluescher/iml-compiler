package ch.fhnw.iml.ast

class AST(root: Node) {
    
}

sealed abstract class Node(parent: Node, left: Node, right: Node)
