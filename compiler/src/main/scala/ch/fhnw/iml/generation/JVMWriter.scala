package ch.fhnw.iml.generation

import java.io.FileOutputStream
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._
import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.BlockCommand
import ch.fhnw.iml.ast.Command
import ch.fhnw.iml.ast.ProgramNode
import ch.fhnw.iml.ast.SkipCommand
import org.objectweb.asm.MethodVisitor

object JVMWriter {
    
    val JVM_V5 = 49
    val IGNORED = 0
    
    case class Scope(writer: ClassWriter, method: MethodVisitor)
    
    def apply(ast: AST, filename : String = "dynamic") {
        val fileWriter = new FileOutputStream("target/" + ast.root.i.chars + ".class")
        fileWriter.write(generateClass(ast, filename))
        fileWriter.close
    }

    def generateClass(ast: AST, filename: String) = {
        implicit val writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
        writeProgram(ast.root, filename)
        writer.toByteArray
    }

    def writeProgram(p: ProgramNode, filename: String)(implicit writer: ClassWriter) {
        writer.visit(JVM_V5, ACC_PUBLIC + ACC_SUPER, p.i.chars, null, "java/lang/Object", null)
        writer.visitSource(filename, null)
        writeConstructor()
        writeEntryPoint(p)
        writeMain(p)
    }

    def writeConstructor()(implicit writer: ClassWriter) {
        val constructor = writer.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
        constructor.visitVarInsn(ALOAD, 0)
        /* make super call */
        constructor.visitMethodInsn(INVOKESPECIAL,
            "java/lang/Object",
            "<init>",
            "()V")
        
        constructor.visitInsn(RETURN)
        constructor.visitMaxs(IGNORED,IGNORED)
        constructor.visitEnd()
    }

    def writeEntryPoint(p: ProgramNode)(implicit writer: ClassWriter) {
        val entry = writer.visitMethod(ACC_PUBLIC + ACC_FINAL,
                p.i.chars,
                "()V",
                null,
                null)
        
        implicit val scope = Scope(writer, entry)
        writeBlock(p.cmd)
        entry.visitInsn(RETURN)
        entry.visitMaxs(IGNORED,IGNORED)
        entry.visitEnd()
    }
    
    def writeMain(p: ProgramNode)(implicit writer: ClassWriter) {
        val main = writer.visitMethod(ACC_PUBLIC + ACC_STATIC,
            "main",
            "([Ljava/lang/String;)V",
            null,
            null);
        
        /* call program constructor */
        main.visitTypeInsn(NEW, p.i.chars)
        main.visitInsn(DUP)
        main.visitMethodInsn(INVOKESPECIAL, p.i.chars, "<init>", "()V")
        
        /* call entry point */
        main.visitMethodInsn(INVOKEVIRTUAL, p.i.chars, p.i.chars, "()V")
        
        main.visitMaxs(IGNORED,IGNORED);
        main.visitInsn(RETURN)
        main.visitEnd()
    }
    
    def writeBlock(block: BlockCommand)(implicit scope: Scope) {
        block.cmds.map(writeCmd)
    }
    
    def writeCmd(cmd: Command)(implicit scope: Scope) = cmd match {
        case SkipCommand 	=> scope.method.visitInsn(NOP); 
        case _ 				=> 
    }
}