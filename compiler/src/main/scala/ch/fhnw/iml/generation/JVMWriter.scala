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
    
    type FrameSize = (Int, Int)
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
        constructor.visitMaxs(1, 1)
        constructor.visitEnd()
    }

    def writeEntryPoint(p: ProgramNode)(implicit writer: ClassWriter) {
        val entry = writer.visitMethod(ACC_PUBLIC + ACC_FINAL,
                p.i.chars,
                "()V",
                null,
                null)
        implicit val scope = Scope(writer, entry)
                
        val maxStack = writeBlock(p.cmd, (1,1))
        entry.visitInsn(RETURN)
        
        entry.visitMaxs(maxStack._1,maxStack._2)
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
        
        main.visitInsn(RETURN)
        main.visitMaxs(2, 2) /* maxstack, maxlocals */
        main.visitEnd()
    }
    
    def writeBlock(block: BlockCommand, frame: FrameSize)(implicit scope: Scope): FrameSize = {
        block.cmds.map(writeCmd).foldLeft(frame)(max)
    }
    
    def writeCmd(cmd: Command)(implicit scope: Scope): FrameSize = cmd match {
        case SkipCommand 	=> println("NOP"); scope.method.visitInsn(NOP); (0,0)
        case _ 				=> (0,0)
    }
    
    def max(frame1: FrameSize, frame2: FrameSize) = (math.max(frame1._1, frame2._1), math.max(frame1._2, frame2._2))
}