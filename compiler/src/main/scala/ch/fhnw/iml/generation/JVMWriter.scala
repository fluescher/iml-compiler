package ch.fhnw.iml.generation

import java.io.FileOutputStream
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._
import ch.fhnw.iml.ast._
import ch.fhnw.iml.checker.TypeChecker
import scala.annotation.tailrec

object JVMWriter {
    
    val JVM_V7 		= 51
    val IGNORED		= 0

    val VM_TRUE 	= 1
    val VM_FALSE 	= 0
    
    case class Scope(p: ProgramNode, className: String, writer: ClassWriter, method: MethodVisitor, symbols: SymbolTable, inFun: Boolean, params: ParameterList, global: Option[GlobalImportList], locals: Int = 0)
    
    def apply(ast: AST, filename : String = "dynamic") {
        val fileWriter = new FileOutputStream("target/" + ast.root.i.chars + ".class")
        fileWriter.write(generateClass(ast, filename))
        fileWriter.close
    }

    def generateClass(ast: AST, filename: String) = {
        val writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
        writeProgram(ast.root, filename)(Scope(ast.root, ast.root.i.chars, writer, null, ast.root.symbols, false, ParameterList(Nil), None))
        writer.toByteArray
    }
    
    def writeProgram(p: ProgramNode, filename: String)(implicit scope: Scope) {
        scope.writer.visit(JVM_V7, ACC_PUBLIC + ACC_SUPER + ACC_FINAL, p.i.chars, null, "java/lang/Object", null)
        scope.writer.visitSource(filename, null)
        writeFunctions(p)
        writeProcedures()
        writeFields(p.symbols, scope)
        writeConstructor()
        writeEntryPoint()
        writeMain()
    }
    
    def writeProcedures()(implicit scope: Scope) {
        val procs = scope.p.cps.decls.filter(_.isInstanceOf[ProcDecl]).map(_.asInstanceOf[ProcDecl])
        		
        procs.map(writeProcedure)
    }
    
    def writeProcedure(p: ProcDecl)(implicit scope: Scope) {
       val cur = scope.writer.visitMethod(	ACC_PUBLIC,
								                p.head.i.chars,
								                getVMType(p),
												null,
												null)
		val s = Scope(scope.p, scope.className, scope.writer, cur, p.symbols, false, p.head.params, p.global)
		val (locals, post) = writeSavePreExecutionState(p.post)(s)
		val ssaved = Scope(scope.p, scope.className, scope.writer, cur, p.symbols, true, p.head.params, p.global, locals)
		writePre(p.pre)(ssaved)
        writeCmd(p.cmd)(ssaved)
        writePost(post)(ssaved)
        cur.visitInsn(RETURN)
        cur.visitMaxs(IGNORED,IGNORED)
        cur.visitEnd()
    }
    
    def writeFunctions(p: ProgramNode)(implicit scope: Scope) {
        val funs = p.cps.decls.filter(_.isInstanceOf[FunDecl]).map(_.asInstanceOf[FunDecl])
        		
        funs.map(writeFunction)
    }
    
    def writeFunction(f: FunDecl)(implicit scope: Scope) {
        val cur = scope.writer.visitMethod(	ACC_PUBLIC,
								                f.head.i.chars,
								                getVMType(f),
												null,
												null)
		val s = Scope(scope.p, scope.className, scope.writer, cur, f.symbols, true, f.head.params, f.global)
		val (locals, post) = writeSavePreExecutionState(f.post)(s)
		val ssaved = Scope(scope.p, scope.className, scope.writer, cur, f.symbols, true, f.head.params, f.global, locals)
		writePre(f.pre)(ssaved)
        writeCmd(f.cmd)(ssaved)
        writePost(post)(ssaved)
        cur.visitVarInsn(ILOAD, getReturnIndex(f)(s)) /* load local return variable onto stack */ 
        cur.visitInsn(IRETURN)
        cur.visitMaxs(IGNORED,IGNORED)
        cur.visitEnd()
    }
    
    def writeSavePreExecutionState(post: Option[ConditionList])(implicit scope: Scope): (Int, Option[ConditionList]) = post match {
        case Some(cs) 	=> writeSavePreExecutionExpressions(calculateLocalCount, cs.conditions.map(_.expr)) match {
            case (i, expr) => (i, Some(ConditionList(cs.conditions.zip(expr).map({ case(c, e) => Condition(c.name, e) }))))
        }
        case None 		=> (calculateLocalCount, None)
    }

    def writeSavePreExecutionExpressions(curLocals: Int, exprs: List[Expr])(implicit scope: Scope): (Int, List[Expr]) = exprs match {
        case Nil 	=> (curLocals, exprs)
        case e::es 	=> writeSavePreExecutionExpression(curLocals, e) match {
            case (locals, e) => writeSavePreExecutionExpressions(locals, es) match {
                case (locals, ses) => (locals, e :: ses)
            }
        }
    }
    
    def writeSavePreExecutionExpression(curLocals: Int, expr: Expr)(implicit scope: Scope): (Int, Expr) = expr match {
        case FunCallExpr(f, es, _)			if 	f == Ident("old")	=> saveExprInPreExecutionState(curLocals, es.head) 	match {case i => (i, FunCallExpr(f, es, i-1))}
        case FunCallExpr(f, es, c)									=> writeSavePreExecutionExpressions(curLocals, es) 	match {case (i, es) => (i, FunCallExpr(f, es, c))}
        case MonadicExpr(o,e)										=> writeSavePreExecutionExpression(curLocals, e)	match {case (i, e) => (i,MonadicExpr(o, e))}
        case DyadicExpr(o, e1, e2)									=> {
            val r = writeSavePreExecutionExpression(curLocals, e1)
            writeSavePreExecutionExpression(r._1, e2) match {case (i, e) => (i, DyadicExpr(o, r._2, e))}
        }
        case e														=> (curLocals, e)
    }
    
    def saveExprInPreExecutionState(pos: Int, e: Expr)(implicit scope: Scope): Int = {
        writeExpr(e)
        scope.method.visitVarInsn(ISTORE, pos)
        pos + 1
    }
    
    def calculateLocalCount()(implicit scope: Scope): Int =  {
        if (scope.inFun)
            localCount + 2 /* this + ret */
        else 
            localCount + 1 /* this */
    }
    
    def localCount(implicit scope: Scope): Int = {
        var i = 0
        for((_, sym) <- scope.symbols.stores) sym match {
            case StorageSymbol(_, _, _, false, false, false, _, _, _, _) => i = i + 1 	/* local */
            case StorageSymbol(_, _, _, false, false, true, _, _, _, _)	 => i = i + 1 	/* argument */
            case _ => 
        }
        i
    }
    
    def writePre(pre: Option[ConditionList])(implicit scope: Scope) = pre match {
        case Some(c) => c.conditions.map(writeCondition)
        case _ => 
    }
    
    def writeCondition(c: Condition)(implicit scope: Scope) {
        val end = new Label()
        val err = new Label()
        
        writeExpr(c.expr)
        scope.method.visitJumpInsn(IFEQ, err)
        scope.method.visitJumpInsn(GOTO, end)
        scope.method.visitLabel(err)
        scope.method.visitTypeInsn(NEW, "java/lang/AssertionError")
        scope.method.visitInsn(DUP)
        c.name match {
            case Some(n) => {
                scope.method.visitLdcInsn(n.chars)
                scope.method.visitMethodInsn(INVOKESPECIAL, "java/lang/AssertionError", "<init>", "(Ljava/lang/Object;)V")
            }
            case None => scope.method.visitMethodInsn(INVOKESPECIAL, "java/lang/AssertionError", "<init>", "()V")
        }
        scope.method.visitInsn(ATHROW)
        scope.method.visitLabel(end)
    }
    
    def writePost(post: Option[ConditionList])(implicit scope: Scope) = post match {
        case Some(c) => c.conditions.map(writeCondition)
        case _ => 
    }

    def writeFields(symbols: SymbolTable, scope: Scope) {
    	for((ident, store) <- symbols.stores if store.isGlobal) {
    	    scope.writer.visitField( ACC_PRIVATE, ident.chars, getVMType(store.t).toString, null, null)
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

    def writeEntryPoint()(implicit scope: Scope) {
        val entry = scope.writer.visitMethod(	ACC_PUBLIC,
								                scope.p.i.chars,
								                "()V",
												null,
												null)
        
        val s: Scope = Scope(scope.p,scope.className, scope.writer, entry, scope.symbols, false, ParameterList(Nil), None, 1)
        writeCmd(scope.p.cmd)(s)
        entry.visitInsn(RETURN)
        entry.visitMaxs(IGNORED,IGNORED)
        entry.visitEnd()
    }
    
    def writeMain()(implicit scope: Scope) {
        val main = scope.writer.visitMethod(	ACC_PUBLIC + ACC_STATIC,
									            "main",
									            "([Ljava/lang/String;)V",
									            null,
									            null);
        
        /* call program constructor */
        main.visitTypeInsn(NEW, scope.p.i.chars)
        main.visitInsn(DUP)
        main.visitMethodInsn(INVOKESPECIAL, scope.p.i.chars, "<init>", "()V")
        
        /* call entry point */
        main.visitMethodInsn(INVOKEVIRTUAL, scope.p.i.chars, scope.p.i.chars, "()V")
        
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
    
    def writeProcCall(p: Ident, exprs: List[Expr])(implicit scope: Scope) {
        val decl = scope.p.symbols.getProcedureDeclaration(p)
        val pairs = decl.head.params.params.zip(exprs)
        val localEnd = scope.locals
        
        /* setup (in)out params */
        setupInOutParameter(pairs, localEnd)
        
        /* load params & call method */
        scope.method.visitVarInsn(ALOAD, 0);
        loadParameter(pairs, localEnd)
        scope.method.visitMethodInsn(INVOKEVIRTUAL, scope.className, p.chars, getVMType(decl))
        
        /* copy variables back */
        loadOutParameterValues(pairs, localEnd)
    }
    
   @tailrec
   private def loadOutParameterValues(pairs: List[(Parameter, Expr)], localPos: Int)(implicit scope: Scope): Unit = pairs match {
        case (p, e)::ps if p.flow == OutFlow || p.flow == InOutFlow		=> {
             saveTo(e, s => {
	            scope.method.visitVarInsn(ALOAD, localPos)
	            scope.method.visitInsn(ICONST_0)
	        	if(p.store.t == Bool) {
	            	scope.method.visitInsn(BALOAD)
	        	} else {
	        	    scope.method.visitInsn(IALOAD)
	        	}
            })
            loadOutParameterValues(ps, localPos + 1)
        }
        case _::ps => loadOutParameterValues(ps, localPos)
        case Nil =>
    }
    
   @tailrec
    private def loadParameter(pairs: List[(Parameter, Expr)], localPos: Int)(implicit scope: Scope): Unit = pairs match {
        case (p, e)::ps if p.flow == OutFlow || p.flow == InOutFlow		=> {
            scope.method.visitVarInsn(ALOAD, localPos)
            loadParameter(ps, localPos+1)
        }
        case (_, e)::ps => writeExpr(e); loadParameter(ps, localPos)
        case _::ps => loadParameter(ps, localPos)
        case Nil =>
    }
    
   @tailrec
    private def setupInOutParameter(pairs: List[(Parameter, Expr)], localPos: Int)(implicit scope: Scope): Unit = pairs match {
        case (p, e)::ps if p.flow == OutFlow || p.flow == InOutFlow		=> {
            scope.method.visitInsn(ICONST_1)
            if(p.store.t == Bool) {
            	scope.method.visitIntInsn(NEWARRAY, T_BOOLEAN)
        	} else {
        	    scope.method.visitIntInsn(NEWARRAY, T_INT)
        	}
            scope.method.visitVarInsn(ASTORE, localPos)
	        if(p.flow == InOutFlow) { /* only copy input value if inout */
	            scope.method.visitVarInsn(ALOAD, localPos)
	            scope.method.visitInsn(ICONST_0)
	            writeExpr(e)
	            if(p.store.t == Bool) {
	            	 scope.method.visitInsn(BASTORE)
	        	} else {
	        	    scope.method.visitInsn(IASTORE)
	        	}
	        }
            setupInOutParameter(ps, localPos+1)
        }
        case _::ps => setupInOutParameter(ps, localPos)
        case Nil =>
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
    
    def saveTo(e: Expr, valueGen: (Scope => Unit))(implicit scope: Scope) = e match {
        case StoreExpr(i, _) 	=> scope.symbols.stores.get(i) match {
            case Some(StorageSymbol(_, t, _, _, false, true, true, apos, _, _)) 	if t == Bool => scope.method.visitVarInsn(ALOAD, apos+1); scope.method.visitInsn(ICONST_0); valueGen(scope); scope.method.visitInsn(BASTORE)
            case Some(StorageSymbol(_, t, _, _, false, true, true, apos, _, _))	=> scope.method.visitVarInsn(ALOAD, apos+1); scope.method.visitInsn(ICONST_0); valueGen(scope); scope.method.visitInsn(IASTORE)
            case Some(StorageSymbol(_, t, _, _, true, _, argument, apos, _, _)) 	=> scope.method.visitVarInsn(ALOAD, 0); valueGen(scope); scope.method.visitFieldInsn(PUTFIELD, scope.className, i.chars, getVMType(scope.symbols.getStoreType(i)))
            case Some(StorageSymbol(_, t, _, _, _, true, _, apos, _, _))			=> valueGen(scope); scope.method.visitVarInsn(ISTORE, apos+1)
            case Some(StorageSymbol(_, t, _, _, _, _, _, _, pos, _))				=> valueGen(scope); scope.method.visitVarInsn(ISTORE, pos+1)
        }
        case _ => throw new RuntimeException("ERROR. Checking should have failed")
    }
    
    def writeAssiCmd(s: Expr, e: Expr)(implicit scope: Scope) {
        saveTo(s, s => writeExpr(e))
    }
    
    def writeInputCmd(expr: Expr, t: Type)(implicit scope: Scope) {
        saveTo(expr, s => {
	        /* create scanner */
	        scope.method.visitTypeInsn(NEW, "java/util/Scanner")
	        scope.method.visitInsn(DUP)
	        scope.method.visitFieldInsn(GETSTATIC, "java/lang/System", "in", "Ljava/io/InputStream;");
	        scope.method.visitMethodInsn(INVOKESPECIAL,	"java/util/Scanner", "<init>", "(Ljava/io/InputStream;)V")
	        
	        /* Read int32 onto stack */
	        scope.method.visitMethodInsn(INVOKEVIRTUAL, "java/util/Scanner", "nextInt", "()I")
        })
    }
    
    def writeOutputCmd(expr: Expr, t: Type)(implicit scope: Scope) {
        scope.method.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        writeExpr(expr)
        scope.method.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "("+getVMType(t)+")V");
    }
    
    def writeExpr(expr: Option[Expr])(implicit scope: Scope): Unit = expr match {
        case Some(e) => writeExpr(e)
        case _ =>
    }
    
    def writeExpr(expr: Expr)(implicit scope: Scope): Unit = expr match {
        case IntLiteralExpression(v)		=> scope.method.visitIntInsn(BIPUSH, v)
        case BoolLiteralExpression(true)	=> scope.method.visitIntInsn(BIPUSH, VM_TRUE)
        case BoolLiteralExpression(false)	=> scope.method.visitIntInsn(BIPUSH, VM_FALSE)
        case StoreExpr(i,_)					=> writeAccessVar(i)
        case FunCallExpr(f, exprs, c)		=> writeFunCall(f, exprs, c)
        case MonadicExpr(o,e)				=> writeMonadicExpr(o, e)
        case DyadicExpr(o, e1, e2)			=> writeDyadicExpr(o, e1, e2)
    }
    
    def writeFunCall(f: Ident, exprs: List[Expr], c: Int)(implicit scope: Scope) = f match {
        case Ident("old") 	 =>  scope.method.visitVarInsn(ILOAD, c)
        case _ => {
        	scope.method.visitVarInsn(ALOAD, 0);
        	exprs.map(writeExpr)
        	scope.method.visitMethodInsn(INVOKEVIRTUAL, scope.className, f.chars, getVMType(scope.p.symbols.getFunctionDeclaration(f)))
        }
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
        case NotOpr					=> throw new RuntimeException("ERROR. Checking should have failed") /* not used */
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
    
    def writeAccessVar(i: Ident)(implicit scope: Scope) = scope.symbols.stores.get(i) match { 
        case Some(StorageSymbol(_, t, _, _, false, true, true, apos, _, _)) 	if t == Bool => scope.method.visitVarInsn(ALOAD, apos+1); scope.method.visitInsn(ICONST_0); scope.method.visitInsn(BALOAD)
        case Some(StorageSymbol(_, t, _, _, false, true, true, apos, _, _))	=> scope.method.visitVarInsn(ALOAD, apos+1); scope.method.visitInsn(ICONST_0); scope.method.visitInsn(IALOAD)
        case Some(StorageSymbol(_, t, _, _, true, _, _, apos, _, _)) 			=> scope.method.visitVarInsn(ALOAD, 0); scope.method.visitFieldInsn(GETFIELD, scope.className, i.chars, getVMType(t))
        case Some(StorageSymbol(_, t, _, _, _, true,_, apos, _, _))			=> scope.method.visitVarInsn(ILOAD, apos+1)
        case Some(StorageSymbol(_, t, _, _, _, _, _, _, pos, _))				=> scope.method.visitVarInsn(ILOAD, pos+1)
        case _ => throw new RuntimeException("ERROR. Checking should have failed")
    }
    
    def getVMType(t: Type): String = t match {
        case Int32			=> "I"
        case Bool			=> "Z"
        case Void			=> "V"
    }
    
    def getReturnIndex(f: FunDecl)(implicit scope: Scope) = scope.symbols.stores.get(f.head.retVal.i) match {
        case Some(StorageSymbol(_, t, _, _, _, _, _, _, pos, _)) => pos + 1
        case None => throw new RuntimeException("ERROR. Checking should have failed")
    }
    
    def getVMType(f: FunDecl): String = {
    		"("+f.head.params.params.map(p => getVMType(p)).mkString+")"+getVMType(f.head.retVal.t)
    }
    
    def getVMType(p: Parameter): String = p match {
        case Parameter(OutFlow,_,store) 	=> "["+getVMType(store.t)
        case Parameter(InOutFlow,_,store) 	=> "["+getVMType(store.t)
        case Parameter(_,_,store) 	=> getVMType(store.t)
    }
    
    def getVMType(p: ProcDecl): String = {
    		"("+p.head.params.params.map(p => getVMType(p)).mkString+")V"
    }
    
}