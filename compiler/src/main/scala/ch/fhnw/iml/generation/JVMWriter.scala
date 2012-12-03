package ch.fhnw.iml.generation

import java.io.FileOutputStream

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.ProgramNode

object JVMWriter {
    def apply(ast: AST) {
        val fileWriter = new FileOutputStream("target/" + ast.root.i.chars + ".class")
        fileWriter.write(generateClass(ast))
        fileWriter.close
    }

    def generateClass(ast: AST) = {
        implicit val writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
        writeProgram(ast.root)
        writer.toByteArray
    }

    def writeProgram(p: ProgramNode)(implicit writer: ClassWriter) {
        writer.visit(49, ACC_PUBLIC + ACC_SUPER, p.i.chars, null, "java/lang/Object", null)
        writer.visitSource(p.i.chars + ".java", null)
        writeConstructor()
        writeEntryPoint(p)
        writeMain(p)
    }

    def writeConstructor()(implicit writer: ClassWriter) {
        val constructor = writer.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        constructor.visitVarInsn(ALOAD, 0);
        /* make super call */
        constructor.visitMethodInsn(INVOKESPECIAL,
            "java/lang/Object",
            "<init>",
            "()V");
        
        constructor.visitFieldInsn(GETSTATIC,
            "java/lang/System",
            "out",
            "Ljava/io/PrintStream;");
        constructor.visitLdcInsn("hello");
        constructor.visitMethodInsn(INVOKEVIRTUAL,
            "java/io/PrintStream",
            "println",
            "(Ljava/lang/String;)V");
        
        constructor.visitInsn(RETURN);
        constructor.visitMaxs(2, 1);
        constructor.visitEnd();
    }

    def writeEntryPoint(p: ProgramNode)(implicit writer: ClassWriter) {
        val entry = writer.visitMethod(ACC_PUBLIC,
                p.i.chars,
                "()V",
                null,
                null)
                
        entry.visitInsn(NOP)
        entry.visitInsn(RETURN)
        
        entry.visitMaxs(1,1)
        entry.visitEnd();
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
        
        main.visitInsn(RETURN);
        main.visitMaxs(2, 1); /* maxstack, maxlocals */
        main.visitEnd();
    }
}