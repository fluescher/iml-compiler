package ch.fhnw.iml.generation

import java.io.FileOutputStream

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

import ch.fhnw.iml.ast._
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
        scope.writer.visit(JVM_V7, ACC_PUBLIC + ACC_SUPER + ACC_FINAL, p.i.chars, null, "java/lang/Object", null)
        scope.writer.visitSource(filename, null)
        writeFunctions(p)
        writeProcedures(p)
        writeFields(p.symbols, scope)
        writeConstructor()
        writeEntryPoint(p)
        writeMain(p)
    }
    
    def writeProcedures(p: ProgramNode)(implicit scope: Scope) {
        val procs = p.cps.decls.filter(_.isInstanceOf[ProcDecl]).map(_.asInstanceOf[ProcDecl])
        		
        procs.map(writeProcedure)
    }
    
    def writeProcedure(p: ProcDecl)(implicit scope: Scope) {
        val cur = scope.writer.visitMethod(	ACC_PUBLIC,
								                p.head.i.chars,
								                getVMType(p),
												null,
												null)
		val s = Scope(scope.className, scope.writer, cur, p.symbols)        
        writeCmd(p.cmd)(s)
        cur.visitInsn(IRETURN)
        cur.visitMaxs(IGNORED,IGNORED)
        cur.visitEnd()
    }
    
    def writeFunctions(p: ProgramNode)(implicit scope: Scope) {
        val funs = p.cps.decls.filter(_.isInstanceOf[FunDecl]).map(_.asInstanceOf[FunDecl])
        		
        funs.map(writeFunction)
    }
    
    def writeFunction(f: FunDecl)(implicit scope: Scope) {
        val funStart = new Label()
        val funEnd = new Label()
        
        val cur = scope.writer.visitMethod(	ACC_PUBLIC,
								                f.head.i.chars,
								                getVMType(f),
												null,
												null)
        cur.visitLabel(funStart)
		val s = Scope(scope.className, scope.writer, cur, f.symbols)  
        writeCmd(f.cmd)(s)
        cur.visitLabel(funEnd)
        cur.visitVarInsn(ILOAD, getReturnIndex(f)(s))
        cur.visitInsn(IRETURN)
        cur.visitMaxs(IGNORED,IGNORED)
        cur.visitEnd()
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
        writeCmd(p.cmd)(s)
        entry.visitInsn(RETURN)
        entry.visitMaxs(IGNORED,IGNORED)
        entry.visitEnd()
    }
    
    def writeMain(p: ProgramNode)(implicit scope: Scope) {
        val main = scope.writer.visitMethod(	ACC_PUBLIC + ACC_STATIC,
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
    
    def writeBlock(cmds: List[Command])(implicit scope: Scope) {
        cmds.map(writeCmd)
    }
    
    def writeCmd(cmd: Command)(implicit scope: Scope): Unit = cmd match {
        case SkipCommand 					=> scope.method.visitInsn(NOP)
        case AssiCommand(s, e)				=> writeAssiCmd(s, e) 				
        case OutputCommand(expr)			=> writeOutputCmd(expr, TypeChecker.getType(expr)(scope.symbols))
        case InputCommand(expr)				=> writeInputCmd(expr, TypeChecker.getType(expr)(scope.symbols))
        case WhileCommand(expr, cmd) 		=> writeWhile(expr, cmd)
        case BlockCommand(cmds)				=> writeBlock(cmds)
        case CondCommand(expr, c1, c2)		=> writeCond(expr, c1, c2)
        case ProcCallCommand(f, exprs,_)	=> writeProcCall(f, exprs) 
    }
    
    def writeProcCall(f: Ident, exprs: List[Expr]) {
        // TODO
    }
    
    def writeCond(expr: Expr, c1: Command, c2: Command)(implicit scope: Scope) {
        val other = new Label()
        val end = new Label()
        
        writeExpr(expr)
        scope.method.visitJumpInsn(IFNE, other)
        writeCmd(c2)
        scope.method.visitJumpInsn(GOTO, end)
        scope.method.visitLabel(other)
        writeCmd(c1)
        scope.method.visitLabel(end)
    }
    
    def writeWhile(e: Expr, cmd: Command)(implicit scope: Scope) {
        val middle = new Label()
        val loop = new Label()
        
        scope.method.visitJumpInsn(GOTO, middle)
        scope.method.visitLabel(loop)
        writeCmd(cmd)
        scope.method.visitLabel(middle)
        writeExpr(e)
        scope.method.visitJumpInsn(IFNE, loop)
    }
    
    def saveTo(e: Expr)(implicit scope: Scope) = e match {// TODO combine only to store expr
        case StoreExpr(i, _) 	=> scope.symbols.stores.get(i) match {
            case Some(StorageSymbol(_, t, _, _, true, argument, apos, _, _)) 	=> scope.method.visitFieldInsn(PUTFIELD, scope.className, i.chars, getVMType(scope.symbols.getStoreType(i)))
            case Some(StorageSymbol(_, t, _, _, _, true, apos, _, _))			=> scope.method.visitVarInsn(ISTORE, apos)
            case Some(StorageSymbol(_, t, _, _, _, _, _, pos, _))				=> scope.method.visitVarInsn(ISTORE, pos+1)
        }
        case VarAccess(i)		=> scope.symbols.stores.get(i) match {
            case Some(StorageSymbol(_, t, _, _, true, argument, apos, _, _)) 	=> scope.method.visitFieldInsn(PUTFIELD, scope.className, i.chars, getVMType(scope.symbols.getStoreType(i)))
            case Some(StorageSymbol(_, t, _, _, _, true, apos, _, _))			=> scope.method.visitVarInsn(ISTORE, apos)
            case Some(StorageSymbol(_, t, _, _, _, _, _, pos, _))				=> scope.method.visitVarInsn(ISTORE, pos+1)
        }
    }
    
    def writeAssiCmd(s: Expr, e: Expr)(implicit scope: Scope) {
        if(isGlobal(s)) scope.method.visitVarInsn(ALOAD, 0); 
        writeExpr(e)
        saveTo(s)
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
        
        saveTo(expr)
    }
    
    def writeOutputCmd(expr: Expr, t: Type)(implicit scope: Scope) {
        scope.method.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        writeExpr(expr)
        scope.method.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "("+getVMType(t)+")V");
    }
    
    def writeExpr(expr: Expr)(implicit scope: Scope): Unit = expr match {
        case IntLiteralExpression(v)		=> scope.method.visitIntInsn(BIPUSH, v)
        case BoolLiteralExpression(true)	=> scope.method.visitIntInsn(BIPUSH, VM_TRUE)
        case BoolLiteralExpression(false)	=> scope.method.visitIntInsn(BIPUSH, VM_FALSE)
        case VarAccess(i)					=> writeAccessVar(i)
        case StoreExpr(i,_)					=> writeAccessVar(i)
        case FunCallExpr(f, exprs)			=> writeFunCall(f, exprs)
        case MonadicExpr(o,e)				=> writeMonadicExpr(o, e)
        case DyadicExpr(o, e1, e2)			=> writeDyadicExpr(o, e1, e2)
    }
    
    def writeFunCall(f: Ident, exprs: List[Expr])(implicit scope: Scope) {
        scope.method.visitVarInsn(ALOAD, 0);
        exprs.map(writeExpr)
        scope.method.visitMethodInsn(INVOKEVIRTUAL, scope.className, f.chars, getVMType(scope.symbols.getFunctionDeclaration(f)))
    }
    
    def writeDyadicExpr(o: Opr, e1: Expr, e2: Expr)(implicit scope: Scope) = o match {
        case EqualsOpr 				=> writeExpr(e1); writeExpr(e2); writeCheckEquals()
        case NotEqualsOpr 			=> writeExpr(e1); writeExpr(e2); writeCheckNotEquals()
        case GreaterThanOpr			=> writeExpr(e1); writeExpr(e2); writeCheckGreaterThan()
        case GreaterEqualsThanOpr 	=> writeExpr(e1); writeExpr(e2); writeCheckGreaterEqualsThan()
        case LessThanOpr			=> writeExpr(e1); writeExpr(e2); writeCheckLessThan()
        case LessEqualsThanOpr		=> writeExpr(e1); writeExpr(e2); writeCheckLessEqualsThan()
        case PlusOpr			    => writeExpr(e1); writeExpr(e2); scope.method.visitInsn(IADD)
        case MinusOpr				=> writeExpr(e1); writeExpr(e2); scope.method.visitInsn(ISUB)
        case TimesOpr				=> writeExpr(e1); writeExpr(e2); scope.method.visitInsn(IMUL)
        case DivOpr					=> writeExpr(e1); writeExpr(e2); scope.method.visitInsn(IDIV)
        case ModOpr					=> writeExpr(e1); writeExpr(e2); scope.method.visitInsn(IREM)
        case AndOpr					=> writeAnd(e1, e2)
        case OrOpr					=> writeOr(e1, e2)
        case NotOpr					=> /* not used */
    }
    
    def writeAnd(e1: Expr, e2: Expr)(implicit scope: Scope) {
        val returnFalse = new Label();
        val end = new Label();
        writeExpr(e1);
        scope.method.visitJumpInsn(IFEQ, returnFalse)
        writeExpr(e2);
        scope.method.visitJumpInsn(IFEQ, returnFalse)
        scope.method.visitInsn(ICONST_1) /* return true */
        scope.method.visitJumpInsn(GOTO, end)
        scope.method.visitLabel(returnFalse)
        scope.method.visitInsn(ICONST_0) /* return false */
        scope.method.visitLabel(end)
    }
    
    def writeOr(e1: Expr, e2: Expr)(implicit scope: Scope) {
        val returnTrue = new Label();
        val end = new Label();
        writeExpr(e1);
        scope.method.visitJumpInsn(IFNE, returnTrue)
        writeExpr(e2);
        scope.method.visitJumpInsn(IFNE, returnTrue)
        scope.method.visitInsn(ICONST_0) /* return false */
        scope.method.visitJumpInsn(GOTO, end)
        scope.method.visitLabel(returnTrue)
        scope.method.visitInsn(ICONST_1) /* return true */
        scope.method.visitLabel(end)
    }
    
    def writeCheckLessThan()(implicit scope: Scope) {
        val returnFalse = new Label();
        val end = new Label();
        scope.method.visitJumpInsn(IF_ICMPGE, returnFalse)
        scope.method.visitInsn(ICONST_1)
        scope.method.visitJumpInsn(GOTO, end)
        scope.method.visitLabel(returnFalse)
        scope.method.visitInsn(ICONST_0)
        scope.method.visitLabel(end)
    }
    
    def writeCheckLessEqualsThan()(implicit scope: Scope) {
        val returnFalse = new Label();
        val end = new Label();
        scope.method.visitJumpInsn(IF_ICMPGT, returnFalse)
        scope.method.visitInsn(ICONST_1)
        scope.method.visitJumpInsn(GOTO, end)
        scope.method.visitLabel(returnFalse)
        scope.method.visitInsn(ICONST_0)
        scope.method.visitLabel(end)
    }
    
    def writeCheckGreaterThan()(implicit scope: Scope) {
        val returnFalse = new Label();
        val end = new Label();
        scope.method.visitJumpInsn(IF_ICMPLE, returnFalse)
        scope.method.visitInsn(ICONST_1)
        scope.method.visitJumpInsn(GOTO, end)
        scope.method.visitLabel(returnFalse)
        scope.method.visitInsn(ICONST_0)
        scope.method.visitLabel(end)
    }
    
    def writeCheckGreaterEqualsThan()(implicit scope: Scope) {
        val returnFalse = new Label();
        val end = new Label();
        scope.method.visitJumpInsn(IF_ICMPLT, returnFalse)
        scope.method.visitInsn(ICONST_1)
        scope.method.visitJumpInsn(GOTO, end)
        scope.method.visitLabel(returnFalse)
        scope.method.visitInsn(ICONST_0)
        scope.method.visitLabel(end)
    }
    
    def writeCheckEquals()(implicit scope: Scope) {
        val returnFalse = new Label();
        val end = new Label();
        scope.method.visitJumpInsn(IF_ICMPNE, returnFalse)
        scope.method.visitInsn(ICONST_1)
        scope.method.visitJumpInsn(GOTO, end)
        scope.method.visitLabel(returnFalse)
        scope.method.visitInsn(ICONST_0)
        scope.method.visitLabel(end)
    }
    
    def writeCheckNotEquals()(implicit scope: Scope) {
        val returnTrue = new Label();
        val end = new Label();
        scope.method.visitJumpInsn(IF_ICMPNE, returnTrue)
        scope.method.visitInsn(ICONST_0)
        scope.method.visitJumpInsn(GOTO, end)
        scope.method.visitLabel(returnTrue)
        scope.method.visitInsn(ICONST_1)
        scope.method.visitLabel(end)
    }
    
    def writeMonadicExpr(o: Opr, e: Expr)(implicit scope: Scope) = o match {
        case NotOpr		=> {
            val returnFalse = new Label()
            val end = new Label
            writeExpr(e);
            scope.method.visitJumpInsn(IFNE, returnFalse)
            scope.method.visitInsn(ICONST_1)
            scope.method.visitJumpInsn(GOTO, end)
            scope.method.visitLabel(returnFalse)
            scope.method.visitInsn(ICONST_0)
            scope.method.visitLabel(end)
        }
        case MinusOpr	=> writeExpr(e); scope.method.visitInsn(INEG)
        case PlusOpr	=> writeExpr(e);
        case _ => 
    }
    
    def writeAccessVar(i: Ident)(implicit scope: Scope) = scope.symbols.stores.get(i) match { // TODO access arguments and local variables
        case Some(StorageSymbol(_, t, _, _, true, argument, apos, _, _)) 	=> scope.method.visitVarInsn(ALOAD, 0); scope.method.visitFieldInsn(GETFIELD, scope.className, i.chars, getVMType(t))
        case Some(StorageSymbol(_, t, _, _, _, true, apos, _, _))			=> scope.method.visitVarInsn(ILOAD, apos+1)
        case Some(StorageSymbol(_, t, _, _, _, _, _, pos, _))				=> scope.method.visitVarInsn(ILOAD, pos+1)
    }
    
    def getVMType(t: Type): String = t match {
        case Int32			=> "I"
        case Bool			=> "Z"
        case Void			=> "V"
    }
    
    
    def isGlobal(s: Expr)(implicit scope: Scope) = s match {
        // TODO only StoreExpr
        case StoreExpr(id,_) => scope.symbols.stores.get(id) match {
        	case Some(StorageSymbol(_, _, _, _, g, _, _, _, _)) => g
        }
        case VarAccess(id) => scope.symbols.stores.get(id) match {
        	case Some(StorageSymbol(_, _, _, _, g, _, _, _, _)) => g
        }
    }
    
    def getReturnIndex(f: FunDecl)(implicit scope: Scope) = scope.symbols.stores.get(f.head.retVal.i) match {
        case Some(StorageSymbol(_, t, _, _, _, _, _, pos, _)) => pos + 1 
    }
    
    def getVMType(f: FunDecl): String = {
    		"("+f.head.params.params.map(p => getVMType(p)).mkString+")"+getVMType(f.head.retVal.t)
    }
    
    def getVMType(p: Parameter): String = p match {
        case Parameter(_,_,store) => getVMType(store.t)
    }
    
    def getVMType(p: ProcDecl): String = {
    		"("+p.head.params.params.map(p => getVMType(p)).mkString+")V"
    }
    
}