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
    
    case class Scope(className: String, writer: ClassWriter, method: MethodVisitor, symbols: SymbolTable, inFun: Boolean, params: ParameterList, global: Option[GlobalImportList])
    
    def apply(ast: AST, filename : String = "dynamic") {
        val fileWriter = new FileOutputStream("target/" + ast.root.i.chars + ".class")
        fileWriter.write(generateClass(ast, filename))
        fileWriter.close
    }

    def generateClass(ast: AST, filename: String) = {
        val writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
        writeProgram(ast.root, filename)(Scope(ast.root.i.chars, writer, null, ast.root.symbols, false, ParameterList(Nil), None))
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
        		
        procs.map(writeProcedure(p))
    }
    
    def writeProcedure(prog: ProgramNode)(p: ProcDecl)(implicit scope: Scope) {
       val cur = scope.writer.visitMethod(	ACC_PUBLIC,
								                p.head.i.chars,
								                getVMType(p),
												null,
												null)
		val s = Scope(scope.className, scope.writer, cur, p.symbols, false, p.head.params, p.global)
		writeSavePreExecutionState(p.post, p.head.params, p.global)(s)
		writePre(prog)(p.pre)(s)
        writeCmd(prog)(p.cmd)(s)
        writePost(prog)(p.post)(s)
        cur.visitInsn(RETURN)
        cur.visitMaxs(IGNORED,IGNORED)
        cur.visitEnd()
    }
    
    def writeFunctions(p: ProgramNode)(implicit scope: Scope) {
        val funs = p.cps.decls.filter(_.isInstanceOf[FunDecl]).map(_.asInstanceOf[FunDecl])
        		
        funs.map(writeFunction(p))
    }
    
    def writeFunction(p: ProgramNode)(f: FunDecl)(implicit scope: Scope) {
        val cur = scope.writer.visitMethod(	ACC_PUBLIC,
								                f.head.i.chars,
								                getVMType(f),
												null,
												null)
		val s = Scope(scope.className, scope.writer, cur, f.symbols, true, f.head.params, f.global)
		writeSavePreExecutionState(f.post, f.head.params, f.global)(s)
		writePre(p)(f.pre)(s)
        writeCmd(p)(f.cmd)(s)
        writePost(p)(f.post)(s)
        cur.visitVarInsn(ILOAD, getReturnIndex(f)(s)) /* load local return variable onto stack */ 
        cur.visitInsn(IRETURN)
        cur.visitMaxs(IGNORED,IGNORED)
        cur.visitEnd()
    }
    
    def writeSavePreExecutionState(post: Option[ConditionList], params: ParameterList, global: Option[GlobalImportList])(implicit scope: Scope) = post match {
        case Some(_) => {
            /* write arguments */
            for(p <- params.params) p match { 
                case Parameter(InFlow, _, StoreDecl(_, id, _)) => {
                	writeAccessVar(id); 
                	scope.method.visitVarInsn(ISTORE, calculateLocalOldPos(id))
                }
            }
            
            /* write globals */
            global match {
                case Some(l) => l.globals.foreach(a => {
                    writeAccessVar(a.i); 
                	scope.method.visitVarInsn(ISTORE, calculateLocalOldPos(a.i))
                })
                case None =>
            }
        }
        case None =>
    }
    
    def calculateLocalCount()(implicit scope: Scope): Int =  calculateLocalOldPos(null) + 1 /* this */
    
    def calculateLocalOldPos(id: Ident)(implicit scope: Scope): Int = {
        if(scope.inFun)
        	maxlocal() + 1 + oldPos(id, scope.params, scope.global)/* +1 -> return value */
        else 
            maxlocal() + oldPos(id, scope.params, scope.global) 
    }
    
    
    def maxlocal()(implicit scope: Scope): Int = {
        localCount  
    }
    
    def localCount(implicit scope: Scope): Int = {
        var i = 0
        for((_, sym) <- scope.symbols.stores) sym match {
            case StorageSymbol(_, _, _, false, false, false, _, _, _, _) => i = i + 1 /* local */
            case StorageSymbol(_, _, _, false, false, true, _, _, _, _)	 => i = i + 1 /* argument */
            case _ => 
        }
        i
    }
    
    def oldPos(id: Ident, params: ParameterList, global: Option[GlobalImportList])(implicit scope: Scope): Int = {
        var i = 0
        /* parameters */
        for(p <- params.params) p match { 
                case Parameter(InFlow, _, StoreDecl(_, i2, _)) if id == i2 	 => return i + 1
                case Parameter(InFlow, _, StoreDecl(_, i2, _))				 => i = i + 1
                case _ =>
        }
        /* globals */
        global match {
            case Some(l) => for(glob <- l.globals) {
                if(glob.i == id) return i + 1
            }
            case None => i = i + 1
        }
        return i
    }
    
    def writePre(prog: ProgramNode)(pre: Option[ConditionList])(implicit scope: Scope) = pre match {
        case Some(c) => c.conditions.map(writeCondition(prog))
        case _ => 
    }
    
    def writeCondition(prog: ProgramNode)(c: Condition)(implicit scope: Scope) {
        val end = new Label()
        val err = new Label()
        
        writeExpr(prog)(c.expr)
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
    
    def writePost(prog: ProgramNode)(post: Option[ConditionList])(implicit scope: Scope) = post match {
        case Some(c) => c.conditions.map(writeCondition(prog))
        case _ => 
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
        
        val s: Scope = Scope(scope.className, scope.writer, entry, scope.symbols, false, ParameterList(Nil), None)
        writeCmd(p)(p.cmd)(s)
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
    
    def writeBlock(cmds: List[Command], prog: ProgramNode)(implicit scope: Scope) {
        cmds.map(writeCmd(prog))
    }
    
    def writeCmd(prog: ProgramNode)(cmd: Command)(implicit scope: Scope): Unit = cmd match {
        case SkipCommand 					=> scope.method.visitInsn(NOP)
        case AssiCommand(s, e)				=> writeAssiCmd(prog)(s, e) 				
        case OutputCommand(expr)			=> writeOutputCmd(prog)(expr, TypeChecker.getType(expr)(scope.symbols))
        case InputCommand(expr)				=> writeInputCmd(expr, TypeChecker.getType(expr)(scope.symbols))
        case WhileCommand(expr, cmd) 		=> writeWhile(expr, cmd, prog)
        case BlockCommand(cmds)				=> writeBlock(cmds, prog)
        case CondCommand(expr, c1, c2)		=> writeCond(expr, c1, c2, prog)
        case ProcCallCommand(f, exprs,_)	=> writeProcCall(f, exprs, prog) 
    }
    
    def writeProcCall(p: Ident, exprs: List[Expr], prog: ProgramNode)(implicit scope: Scope) {// TODO
        val decl = prog.symbols.getProcedureDeclaration(p)
        val pairs = decl.head.params.params.zip(exprs)
        val localEnd = calculateLocalCount -1
        var local = localEnd
        
        /* setup (in)out params */
        for((p, e) <- pairs if p.flow == OutFlow || p.flow == InOutFlow) { 
            scope.method.visitInsn(ICONST_1)
            if(p.store.t == Bool) {
            	scope.method.visitIntInsn(NEWARRAY, T_BOOLEAN)
        	} else {
        	    scope.method.visitIntInsn(NEWARRAY, T_INT)
        	}
            scope.method.visitVarInsn(ASTORE, local)
            scope.method.visitVarInsn(ALOAD, local)
            scope.method.visitInsn(ICONST_0)
            writeExpr(prog)(e)
            if(p.store.t == Bool) {
            	 scope.method.visitInsn(BASTORE)
        	} else {
        	    scope.method.visitInsn(IASTORE)
        	}
            local = local + 1
        }
        
        scope.method.visitVarInsn(ALOAD, 0);
        
        local = localEnd
        for((p, e) <- pairs) { /* setup (in)out params */
            if (p.flow == OutFlow || p.flow == InOutFlow) {
                 scope.method.visitVarInsn(ALOAD, local)
                 local = local + 1
            } else {
                 writeExpr(prog)(e)
            }
        }

        scope.method.visitMethodInsn(INVOKEVIRTUAL, scope.className, p.chars, getVMType(decl))
        
        /* copy variables back */
        local = localEnd
        for((p, e) <- pairs if p.flow == OutFlow || p.flow == InOutFlow) {
            saveTo(e, s => {
				            scope.method.visitVarInsn(ALOAD, local)
				            scope.method.visitInsn(ICONST_0)
				        	if(p.store.t == Bool) {
				            	scope.method.visitInsn(BALOAD)
				        	} else {
				        	    scope.method.visitInsn(IALOAD)
				        	}
            })
        	local = local + 1
        }
    }
    
    def writeCond(expr: Expr, c1: Command, c2: Command, p: ProgramNode)(implicit scope: Scope) {
        val other = new Label()
        val end = new Label()
        
        writeExpr(p)(expr)
        scope.method.visitJumpInsn(IFNE, other)
        writeCmd(p)(c2)
        scope.method.visitJumpInsn(GOTO, end)
        scope.method.visitLabel(other)
        writeCmd(p)(c1)
        scope.method.visitLabel(end)
    }
    
    def writeWhile(e: Expr, cmd: Command, p: ProgramNode)(implicit scope: Scope) {
        val middle = new Label()
        val loop = new Label()
        
        scope.method.visitJumpInsn(GOTO, middle)
        scope.method.visitLabel(loop)
        writeCmd(p)(cmd)
        scope.method.visitLabel(middle)
        writeExpr(p)(e)
        scope.method.visitJumpInsn(IFNE, loop)
    }
    
    def saveTo(e: Expr, valueGen: (Scope => Unit))(implicit scope: Scope) = e match {// TODO combine only to store expr
        case StoreExpr(i, _) 	=> scope.symbols.stores.get(i) match {
            case Some(StorageSymbol(_, t, _, _, false, true, true, apos, _, _)) 	if t == Bool => scope.method.visitVarInsn(ALOAD, apos+1); scope.method.visitInsn(ICONST_0); valueGen(scope); scope.method.visitInsn(BASTORE)
            case Some(StorageSymbol(_, t, _, _, false, true, true, apos, _, _))	=> scope.method.visitVarInsn(ALOAD, apos+1); scope.method.visitInsn(ICONST_0); valueGen(scope); scope.method.visitInsn(IASTORE)
            case Some(StorageSymbol(_, t, _, _, true, _, argument, apos, _, _)) 	=> scope.method.visitVarInsn(ALOAD, 0); valueGen(scope); scope.method.visitFieldInsn(PUTFIELD, scope.className, i.chars, getVMType(scope.symbols.getStoreType(i)))
            case Some(StorageSymbol(_, t, _, _, _, true, _, apos, _, _))			=> valueGen(scope); scope.method.visitVarInsn(ISTORE, apos)
            case Some(StorageSymbol(_, t, _, _, _, _, _, _, pos, _))				=> valueGen(scope); scope.method.visitVarInsn(ISTORE, pos+1)
        }
        case _ => throw new RuntimeException("ERROR. Checking should have failed")
    }
    
    def writeAssiCmd(p: ProgramNode)(s: Expr, e: Expr)(implicit scope: Scope) {
        saveTo(s, s => writeExpr(p)(e))
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
    
    def writeOutputCmd(p: ProgramNode)(expr: Expr, t: Type)(implicit scope: Scope) {
        scope.method.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        writeExpr(p)(expr)
        scope.method.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "("+getVMType(t)+")V");
    }
    
    def writeExpr(p: ProgramNode, expr: Option[Expr])(implicit scope: Scope): Unit = expr match {
        case Some(e) => writeExpr(p)(e)
        case _ =>
    }
    
    def writeExpr(p: ProgramNode)(expr: Expr)(implicit scope: Scope): Unit = expr match {
        case IntLiteralExpression(v)		=> scope.method.visitIntInsn(BIPUSH, v)
        case BoolLiteralExpression(true)	=> scope.method.visitIntInsn(BIPUSH, VM_TRUE)
        case BoolLiteralExpression(false)	=> scope.method.visitIntInsn(BIPUSH, VM_FALSE)
        case StoreExpr(i,_)					=> writeAccessVar(i)
        case FunCallExpr(f, exprs)			=> writeFunCall(p)(f, exprs)
        case MonadicExpr(o,e)				=> writeMonadicExpr(p)(o, e)
        case DyadicExpr(o, e1, e2)			=> writeDyadicExpr(p)(o, e1, e2)
    }
    
    def writeFunCall(p: ProgramNode)(f: Ident, exprs: List[Expr])(implicit scope: Scope) = f match {
        case Ident("old") 	 =>  exprs.head match {
            case StoreExpr(i, _) 	=> scope.method.visitVarInsn(ILOAD, calculateLocalOldPos(i))
        	case _ =>
        }
        case _ => {
        	scope.method.visitVarInsn(ALOAD, 0);
        	exprs.map(writeExpr(p))
        	scope.method.visitMethodInsn(INVOKEVIRTUAL, scope.className, f.chars, getVMType(p.symbols.getFunctionDeclaration(f)))
        }
    }
    
    def writeDyadicExpr(p: ProgramNode)(o: Opr, e1: Expr, e2: Expr)(implicit scope: Scope) = o match {
        case EqualsOpr 				=> writeExpr(p)(e1); writeExpr(p)(e2); writeCheckEquals()
        case NotEqualsOpr 			=> writeExpr(p)(e1); writeExpr(p)(e2); writeCheckNotEquals()
        case GreaterThanOpr			=> writeExpr(p)(e1); writeExpr(p)(e2); writeCheckGreaterThan()
        case GreaterEqualsThanOpr 	=> writeExpr(p)(e1); writeExpr(p)(e2); writeCheckGreaterEqualsThan()
        case LessThanOpr			=> writeExpr(p)(e1); writeExpr(p)(e2); writeCheckLessThan()
        case LessEqualsThanOpr		=> writeExpr(p)(e1); writeExpr(p)(e2); writeCheckLessEqualsThan()
        case PlusOpr			    => writeExpr(p)(e1); writeExpr(p)(e2); scope.method.visitInsn(IADD)
        case MinusOpr				=> writeExpr(p)(e1); writeExpr(p)(e2); scope.method.visitInsn(ISUB)
        case TimesOpr				=> writeExpr(p)(e1); writeExpr(p)(e2); scope.method.visitInsn(IMUL)
        case DivOpr					=> writeExpr(p)(e1); writeExpr(p)(e2); scope.method.visitInsn(IDIV)
        case ModOpr					=> writeExpr(p)(e1); writeExpr(p)(e2); scope.method.visitInsn(IREM)
        case AndOpr					=> writeAnd(p)(e1, e2)
        case OrOpr					=> writeOr(p)(e1, e2)
        case NotOpr					=> /* not used */
    }
    
    def writeAnd(p: ProgramNode)(e1: Expr, e2: Expr)(implicit scope: Scope) {
        val returnFalse = new Label();
        val end = new Label();
        writeExpr(p)(e1);
        scope.method.visitJumpInsn(IFEQ, returnFalse)
        writeExpr(p)(e2);
        scope.method.visitJumpInsn(IFEQ, returnFalse)
        scope.method.visitInsn(ICONST_1) /* return true */
        scope.method.visitJumpInsn(GOTO, end)
        scope.method.visitLabel(returnFalse)
        scope.method.visitInsn(ICONST_0) /* return false */
        scope.method.visitLabel(end)
    }
    
    def writeOr(p: ProgramNode)(e1: Expr, e2: Expr)(implicit scope: Scope) {
        val returnTrue = new Label();
        val end = new Label();
        writeExpr(p)(e1);
        scope.method.visitJumpInsn(IFNE, returnTrue)
        writeExpr(p)(e2);
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
    
    def writeMonadicExpr(p: ProgramNode)(o: Opr, e: Expr)(implicit scope: Scope) = o match {
        case NotOpr		=> {
            val returnFalse = new Label()
            val end = new Label
            writeExpr(p)(e);
            scope.method.visitJumpInsn(IFNE, returnFalse)
            scope.method.visitInsn(ICONST_1)
            scope.method.visitJumpInsn(GOTO, end)
            scope.method.visitLabel(returnFalse)
            scope.method.visitInsn(ICONST_0)
            scope.method.visitLabel(end)
        }
        case MinusOpr	=> writeExpr(p)(e); scope.method.visitInsn(INEG)
        case PlusOpr	=> writeExpr(p)(e);
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