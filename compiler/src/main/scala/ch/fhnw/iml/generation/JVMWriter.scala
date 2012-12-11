package ch.fhnw.iml.generation

import java.io.FileOutputStream
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._
import ch.fhnw.iml.ast._
import org.objectweb.asm.MethodVisitor
import ch.fhnw.iml.checker.TypeChecker

object JVMWriter {
    
    val JVM_V7 = 51
    val IGNORED = 0

    val VM_TRUE = 1
    val VM_FALSE = 0
    
    case class Scope(className: String, writer: ClassWriter, method: MethodVisitor, symbols: SymbolTable)
    
    def apply(ast: AST, filename : String = "dynamic") {
        val fileWriter = new FileOutputStream("target/" + ast.root.i.chars + ".class")
        fileWriter.write(generateClass(ast, filename))
        fileWriter.close
    }

    def generateClass(ast: AST, filename: String) = {
        val writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
        writeProgram(ast.root, filename)(Scope(ast.root.i.chars, writer, null, ast.root.symbols))
        writer.toByteArray
    }

    def writeProgram(p: ProgramNode, filename: String)(implicit scope: Scope) {
        scope.writer.visit(JVM_V7, ACC_PUBLIC + ACC_SUPER, p.i.chars, null, "java/lang/Object", null)
        scope.writer.visitSource(filename, null)
        writeFields(p.symbols, scope)
        writeConstructor()
        writeEntryPoint(p)
        writeMain(p)
    }
    
    def writeFields(symbols: SymbolTable, scope: Scope) {
    	for((ident, store) <- symbols.stores if store.isGlobal) {
    	    scope.writer.visitField( ACC_PUBLIC, ident.chars, getVMType(store.t).toString, null, null)
    	}
    }

    def writeConstructor()(implicit scope: Scope) {
        val constructor = scope.writer.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
        constructor.visitVarInsn(ALOAD, 0)
        /* make super call */
        constructor.visitMethodInsn(INVOKESPECIAL,
						            "java/lang/Object",
						            "<init>",
						            "()V")
        
        /* initialize fields */
		for((ident, store) <- scope.symbols.stores if store.isGlobal) {
		   constructor.visitVarInsn(ALOAD, 0)
		   constructor.visitIntInsn(BIPUSH, 0)
    	   constructor.visitFieldInsn(PUTFIELD, scope.className, ident.chars, getVMType(store.t))
    	}
        
        constructor.visitInsn(RETURN)
        constructor.visitMaxs(IGNORED,IGNORED)
        constructor.visitEnd()
    }

    def writeEntryPoint(p: ProgramNode)(implicit scope: Scope) {
        val entry = scope.writer.visitMethod(	ACC_PUBLIC + ACC_FINAL,
								                p.i.chars,
								                "()V",
												null,
												null)
        
        val s: Scope = Scope(scope.className, scope.writer, entry, scope.symbols)
        writeBlock(p.cmd)(s)
        entry.visitInsn(RETURN)
        entry.visitMaxs(IGNORED,IGNORED)
        entry.visitEnd()
    }
    
    def writeMain(p: ProgramNode)(implicit scope: Scope) {
        val main = scope.writer.visitMethod(ACC_PUBLIC + ACC_STATIC,
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
        case SkipCommand 			=> scope.method.visitInsn(NOP);
        case AssiCommand(s, e)		=> writeAssiCmd(s, e) 				
        case OutputCommand(expr)	=> writeOutputCmd(expr, TypeChecker.getType(expr)(scope.symbols))
        case InputCommand(expr)		=> writeInputCmd(expr, TypeChecker.getType(expr)(scope.symbols))
        case _ 				=> 
    }
    
    def writeAssiCmd(s: Expr, e: Expr)(implicit scope: Scope) {
        scope.method.visitVarInsn(ALOAD, 0); 
        writeExpr(e)
        // TODO combine only to store expr
        s match {
            case StoreExpr(i, _) 	=> scope.method.visitFieldInsn(PUTFIELD, scope.className, i.chars, getVMType(scope.symbols.getStoreType(i)))
            case VarAccess(i)		=> scope.method.visitFieldInsn(PUTFIELD, scope.className, i.chars, getVMType(scope.symbols.getStoreType(i)))
        }
    }
    
    def writeInputCmd(expr: Expr, t: Type)(implicit scope: Scope) {
        scope.method.visitVarInsn(ALOAD, 0); 
        
        /* create scanner */
        scope.method.visitTypeInsn(NEW, "java/util/Scanner")
        scope.method.visitInsn(DUP)
        scope.method.visitFieldInsn(GETSTATIC, "java/lang/System", "in", "Ljava/io/InputStream;");
        scope.method.visitMethodInsn(INVOKESPECIAL,	"java/util/Scanner", "<init>", "(Ljava/io/InputStream;)V")
        
        /* Read int32 onto stack */
        scope.method.visitMethodInsn(INVOKEVIRTUAL, "java/util/Scanner", "nextInt", "()I")
        
        // TODO combine only to store expr
        expr match {
            case StoreExpr(i, _) 	=> scope.method.visitFieldInsn(PUTFIELD, scope.className, i.chars, getVMType(scope.symbols.getStoreType(i)))
            case VarAccess(i)		=> scope.method.visitFieldInsn(PUTFIELD, scope.className, i.chars, getVMType(scope.symbols.getStoreType(i)))
        }
        
    }
    
    def writeOutputCmd(expr: Expr, t: Type)(implicit scope: Scope) {
        scope.method.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        writeExpr(expr)
        scope.method.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "("+getVMType(t)+")V");
    }
    
    def writeExpr(expr: Expr)(implicit scope: Scope) = expr match {
        case IntLiteralExpression(v)		=> scope.method.visitIntInsn(BIPUSH, v)
        case BoolLiteralExpression(true)	=> scope.method.visitIntInsn(BIPUSH, VM_TRUE)
        case BoolLiteralExpression(false)	=> scope.method.visitIntInsn(BIPUSH, VM_FALSE)
        case VarAccess(i)					=> accessVar(i)
        case _								=> scope.method.visitIntInsn(BIPUSH, 0)
    }
    
    def accessVar(i: Ident)(implicit scope: Scope) = scope.symbols.stores.get(i) match { // TODO access arguments and local variables
        case Some(StorageSymbol(_, t, _, _, true, argument, apos, _, _)) 	=> scope.method.visitVarInsn(ALOAD, 0); scope.method.visitFieldInsn(GETFIELD, scope.className, i.chars, getVMType(t))
    }
    
    def getVMType(t: Type) = t match {
        case Int32			=> "I"
        case Bool			=> "Z"
        case Void			=> "V"
    }
}