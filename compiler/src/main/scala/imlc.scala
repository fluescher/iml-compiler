import ch.fhnw.iml.parsing.IMLParsers
import ch.fhnw.iml.generation.JVMWriter
import ch.fhnw.iml.generation.JVMWriter
import ch.fhnw.iml.ast.AST

object imlc extends App {
    val code = scala.io.Source.fromFile(args(0)).mkString
	val parser = new IMLParsers
	val writer = JVMWriter
	
	parser.parse(code) match {
        case parser.Success(prog, _) => writer(AST(prog))
        case _ => println ("BLUBBER BLUBBER")
    }
	
}