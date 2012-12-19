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

object FlowChecker extends Checker {

    def apply(ast: AST) = {
        flowCheck(ast.root) match {
	        case CheckSuccess(_) 	=> CheckSuccess(ast)
	        case CheckError(m,n)	=> CheckError(m,n)
	    }
    }
    
    private def flowCheck(p: ProgramNode): CheckResult[Any] = checkProcDecls(p.cps.decls) and checkFunDecls(p.cps.decls)
    
    private def checkProcDecls(decls: List[Decl]) = toResult (decls	.filter(_.isInstanceOf[ProcDecl])
    																.map(_.asInstanceOf[ProcDecl])
																	.map(checkProc))
    														
    private def checkFunDecls(decls: List[Decl]) = toResult (decls	.filter(_.isInstanceOf[FunDecl])
    																.map(_.asInstanceOf[FunDecl])
																	.map(checkFun))
																	
																	
    private def checkProc(p: ProcDecl): CheckResult[Any] = checkArguments(p.head.params.params, checkProcArgument) and checkProcImports(p.global.getOrElse(GlobalImportList(List())).globals)
    
    private def checkFun(f: FunDecl): CheckResult[Any] = checkArguments(f.head.params.params, checkFunArgument) and checkFunImports(f.global.getOrElse(GlobalImportList(List())).globals)
    
    private def checkArguments(args: List[Parameter], f: Parameter => CheckResult[Any]) = toResult(args.map(f))
    
    private def checkProcArgument(arg: Parameter): CheckResult[Any] = arg match {
        case Parameter(InFlow, Ref, StoreDecl(VarNode, _, _))	=>	CheckError("No in ref var allowed", arg)
        case Parameter(InOutFlow, _, _)							=>	CheckError("No inout const allowed", arg)
        case _ => CheckSuccess(None)
    } 
    
    private def checkProcImports(imps: List[GlobalImport]) = toResult(imps.map(checkProcImport)) 
    
    private def checkFunImports(imps: List[GlobalImport]) = toResult(imps.map(checkFunImport)) 
    
    private def checkFunImport(imp: GlobalImport): CheckResult[Any] = imp match {
        case GlobalImport(InFlow, VarNode, _) 	=> CheckError("No in ref var allowed", imp)
        case GlobalImport(InFlow, _, _) 		=> CheckSuccess(None)
        case _							 		=> CheckError("Only input allowed in functions", imp)
    }
    
    private def checkProcImport(imp: GlobalImport): CheckResult[Any] = imp match {
        case GlobalImport(InFlow, VarNode, _) 		=> CheckError("No in ref var allowed", imp)
        case GlobalImport(InOutFlow, ConstNode, _) 	=> CheckError("No inout const allowed", imp)
        case _ 										=> CheckSuccess(None)
    }
    
    private def checkFunArgument(arg: Parameter): CheckResult[Any] = arg match {
        case Parameter(InFlow, Ref, StoreDecl(VarNode, _, _))	=>	CheckError("No in ref var allowed", arg)
        case Parameter(OutFlow, _, _)							=>	CheckError("No out in function allowed", arg)
        case Parameter(InOutFlow, _, _)							=>	CheckError("No inout in function allowed", arg)
        case _ 													=> 	CheckSuccess(None)
    } 
}