package ch.fhnw.iml.checker

import ch.fhnw.iml.ast.AST
import ch.fhnw.iml.ast.ProgramNode
import ch.fhnw.iml.ast.ProcDecl
import ch.fhnw.iml.ast.Decl
import ch.fhnw.iml.ast.Parameter
import ch.fhnw.iml.ast.StoreDecl
import ch.fhnw.iml.ast.InFlow
import ch.fhnw.iml.ast.Ref
import ch.fhnw.iml.ast.VarNode
import ch.fhnw.iml.ast.FunDecl
import ch.fhnw.iml.ast.InOutFlow
import ch.fhnw.iml.ast.OutFlow
import ch.fhnw.iml.ast.GlobalImport
import ch.fhnw.iml.ast.GlobalImportList
import ch.fhnw.iml.ast.ConstNode
import ch.fhnw.iml.ast.ProcCallCommand
import ch.fhnw.iml.ast.ProcCallCommand
import ch.fhnw.iml.ast.GlobalImportList
import ch.fhnw.iml.ast.SymbolTable
import ch.fhnw.iml.ast.Node
import ch.fhnw.iml.ast.WhileCommand
import ch.fhnw.iml.ast.SkipCommand
import ch.fhnw.iml.ast.InputCommand
import ch.fhnw.iml.ast.Command
import ch.fhnw.iml.ast.CondCommand
import ch.fhnw.iml.ast.OutputCommand
import ch.fhnw.iml.ast.AssiCommand
import ch.fhnw.iml.ast.BlockCommand
import ch.fhnw.iml.ast.StoreExpr
import ch.fhnw.iml.ast.FunCallExpr
import ch.fhnw.iml.ast.MonadicExpr
import ch.fhnw.iml.ast.DyadicExpr
import ch.fhnw.iml.ast.Expr
import ch.fhnw.iml.ast.Ident

object GlobalImportChecker extends Checker {
    
    def apply(ast: AST) = {
        globalCheck(ast.root) match {
	        case CheckSuccess(_) 	=> CheckSuccess(ast)
	        case CheckError(m,n)	=> CheckError(m,n)
	    }
    }
    
    private def globalCheck(p: ProgramNode): CheckResult[Any] = 
        checkProcDecls(p.cps.decls)(p.symbols) and checkFunDecls(p.cps.decls)(p.symbols)
    
    private def checkProcDecls(decls: List[Decl])(symbols : SymbolTable) = 
        toResult (decls	.filter(_.isInstanceOf[ProcDecl])
    					.map(_.asInstanceOf[ProcDecl])
						.map(proc => checkProc(proc)(symbols)))
    														
    private def checkFunDecls(decls: List[Decl])(symbols : SymbolTable) = 
        toResult (decls	.filter(_.isInstanceOf[FunDecl])
						.map(_.asInstanceOf[FunDecl])
						.map(fun => checkFun(fun)(symbols)))
																																	
    private def checkProc(p: ProcDecl)(symbols : SymbolTable): CheckResult[Any] = {
    	val globals = p.global match {
    	    case None 		=> List()
    	    case Some(glob)	=> glob.globals;
    	}
    	checkCommands(p.cmd.cmds)(globals)(symbols)
    }
    
    private def checkFun(f: FunDecl)(symbols : SymbolTable) : CheckResult[Any] = {
        val globals = f.global match {
    	    case None 		=> List()
    	    case Some(glob)	=> glob.globals;
    	}
        checkCommands(f.cmd.cmds)(globals)(symbols)
    }
    
    private def checkCommands(cmds : List[Command])(globals: List[GlobalImport])(symbols: SymbolTable): CheckResult[Any] = {
        cmds.map(cmd => checkCommand(cmd)(globals)(symbols))
    		.foldLeft(CheckSuccess[Any](None) : CheckResult[Any])(combineToResult)
    }
    
    private def checkCommand(cmd: Command)(globals: List[GlobalImport])(symbols: SymbolTable): CheckResult[Any] = cmd match {
        case block: BlockCommand 			=> checkCommands(block.cmds)(globals)(symbols)
        case SkipCommand 					=> CheckSuccess(None)
        case AssiCommand(left, right) 		=> checkExpr(right)(globals)(symbols)
        case CondCommand(expr, cmd1, cmd2) 	=> checkExpr(expr)(globals)(symbols) and checkCommand(cmd1)(globals)(symbols) and checkCommand(cmd2)(globals)(symbols)
        case WhileCommand(expr, cmd) 		=> checkExpr(expr)(globals)(symbols) and checkCommand(cmd)(globals)(symbols)
        case p: ProcCallCommand 			=> checkProcCall(p)(globals)(symbols)
        case InputCommand(expr) 			=> checkExpr(expr)(globals)(symbols)
        case OutputCommand(expr) 			=> checkExpr(expr)(globals)(symbols)
    }
    
    private def checkProcCall(cmd : ProcCallCommand)(globalsOuter : List[GlobalImport])(symbols : SymbolTable) : CheckResult[Any] = {
    	val procDecl = symbols.getProcedureDeclaration(cmd.f);
    	procDecl.global match {
    	    case None 		=> CheckSuccess(None)
    	    case Some(glob)	=> compareGlobals(cmd)(globalsOuter)(glob.globals)
    	}
    }
    
    private def compareGlobals(n: Node)(outer : List[GlobalImport])(inner : List[GlobalImport]) : CheckResult[Any] = {
    	inner.map(a => outer.contains(a))
        	 .foldLeft(true)((a,b) => a && b) match {
    	        case true	=> CheckSuccess(None)
    	        case false  => CheckError("Routine imports more or other globals than caller.", n)
    	    } 
    }
    
   private def checkExpr(expr: Expr)(globals: List[GlobalImport])(symbols: SymbolTable): CheckResult[Any] = expr match {
        case m: MonadicExpr => checkExpr(m.expr)(globals)(symbols)
        case d: DyadicExpr 	=> checkExpr(d.expr1)(globals)(symbols) and checkExpr(d.expr2)(globals)(symbols)
        case f: FunCallExpr => checkFunExpr(f)(globals)(symbols)
        case other			=> CheckSuccess(None)
    }
    
   private def checkFunExpr(f : FunCallExpr)(globalsOuter : List[GlobalImport])(symbols : SymbolTable) : CheckResult[Any] = {
    	val funDecl = symbols.getFunctionDeclaration(f.i);
  
    	funDecl.global match {
    	    case None 		=> CheckSuccess(None)
    	    case Some(glob)	=> compareGlobals(f)(globalsOuter)(glob.globals)
    	}
    }
   
    private def combineToResult(r1: CheckResult[Any], r2: CheckResult[Any]): CheckResult[Any] = r1 match {
        case e: CheckError[Any] => e
        case r => r2
    }
    
}