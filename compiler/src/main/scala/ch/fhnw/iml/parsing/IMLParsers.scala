package ch.fhnw.iml.parsing

import scala.util.parsing.combinator.Parsers
import ch.fhnw.iml.scanning.IMLLexical
import ch.fhnw.iml.scanning.IMLTokens
import scala.util.parsing.combinator.syntactical.TokenParsers
import ch.fhnw.iml.scanning.IMLTokens
import scala.util.parsing.input.Reader
import ch.fhnw.iml.ast._

class IMLParsers extends TokenParsers {
    type Tokens = IMLTokens
    
    val lexical = new IMLLexical
    import lexical.{Not, AddOpr, Literal, IntLiteral, Program, Ident, Global, LParen, 
        			RParen, ChangeMode, Colon, SemiColon, Int32, Fun, Returns, Local,
        			Proc, FlowMode, Comma, Skip, Becomes, If, Else, While, Call, QuestionMark,
        			ExclamationMark, Init, LBrace, RBrace, True, False,
        			MultOpr, RelOpr, BoolOpr, Type, Or, And, Ref, Copy,
        			Equals, NotEquals, GreaterThan, GreaterEqualsThan, LessThan, LessEqualsThan,
        			Minus, Plus, Times, Div, Mod, Bool, InOut, Out, In, Var, Const}
    
    /* Programs */
    def program = ((Program ~ ident ~ Global ~ cpsDecl ~ blockCmd) 	^^ {case _ ~ i ~ _ ~ c ~ b => ProgramAst(i,c,b)}
    			|  (Program ~ ident ~ blockCmd)						^^ {case _ ~ i ~ b => ProgramAst(i,null,b)}
    			)
    
    /* declarations */
    def decl = (storeDecl
    		 	| funDecl
    		 	| procDecl)
    				 
    def storeDecl : Parser[StoreDecl] = ((opt(changeMode) ~ ident ~ Colon ~ imlType) ^^ {case c ~ i ~ _ ~ t => StoreDecl(c.getOrElse(null), i, t)})

    def funDecl : Parser[FunDecl] = (   
    				(funHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ blockCmd)	^^ {case f ~ _ ~ g ~ _ ~ c ~ b => FunDecl(f,g,c,b)}
    				| (funHead ~ Global ~ globImpList ~ blockCmd) 					^^ {case f ~ _ ~ g ~ b => FunDecl(f,g,null,b)}
            		| (funHead ~ Local ~ cpsDecl ~ blockCmd)						^^ {case f ~ _ ~ c ~ b => FunDecl(f,null,c,b)}
            		| (funHead ~ blockCmd)											^^ {case f ~ b => FunDecl(f,null,null,b)}
            	  )
    
    def funHead = ((Fun ~ ident ~ paramList ~ Returns ~ storeDecl) ^^ {case _ ~ i ~ p ~ _ ~ s => FunHead(i,p,s)})
            	  
    def procDecl : Parser[ProcDecl] = (  
            		  (procHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ blockCmd)	^^ {case p ~ _ ~ g ~ _ ~ c ~ b => new ProcDecl(p,g,c,b)}
    				| (procHead ~ Global ~ globImpList ~ blockCmd) 						^^ {case p ~ _ ~ g ~ b => new ProcDecl(p,g,null,b)}
            		| (procHead ~ Local ~ cpsDecl ~ blockCmd)							^^ {case p ~ _ ~ c ~ b => new ProcDecl(p,null,c,b)}
            		| (procHead ~ blockCmd)												^^ {case p ~ b => new ProcDecl(p,null,null, b)}
            	  )

    def procHead = ((Proc ~ ident ~ paramList) ^^ {case _ ~ i ~ p => ProcHead(i, p)})
            	  
    def cpsDecl = (decl ~ rep(SemiColon ~> decl) ^^ {case first ~ rest => CpsDecl(first::rest)})
    
    
    /* Parameter lists */
    def paramList  = ( (LParen ~ RParen) 								^^ {case _ => ParameterList(List())}
            		| (LParen ~> param ~ rep(Comma ~> param) <~ RParen)	^^ {case first ~ rest => ParameterList(first::rest)}
            		)
    
    def param = opt(flowMode) ~ opt(mechMode) ~ storeDecl ^^ {case f ~ c ~ s => Parameter(f.getOrElse(null), c.getOrElse(null), s)}
    
    def globImpList = globImp ~ rep(Comma ~> globImp) ^^ {case first ~ rest => GlobalImportList(first::rest)}
    
    def globImp = ((opt(flowMode) ~  opt(changeMode) ~  ident) ^^ {case f ~ c ~ i => GlobalImport(f.getOrElse(null), c.getOrElse(null),i)})
    
    			
    
    /* Commands */
    def cmd : Parser[Command] = (Skip 											^^ {case s => SkipCommand()}
			| (expr ~ Becomes ~ expr) 											^^ {case e1 ~ _ ~ e2 => AssiCommand(e1, e2) }
			| (If ~ LParen ~ expr ~ RParen ~ blockCmd ~ Else ~ blockCmd)		^^ {case _ ~ _ ~ ex ~ _ ~ cmd1 ~ _ ~ cmd2 => CondCommand(ex, cmd1, cmd2)}
			| (While ~ LParen ~ expr ~ RParen ~ blockCmd)						^^ {case _ ~ _ ~ ex ~ _ ~ cmd1 => WhileCommand(ex, cmd1)}
			| (Call ~ ident ~ exprList ~ Init ~ globInitList)					^^ {case _ ~ name ~ list ~ _ ~ initlist => ProcCallComand(name, list, initlist)}
			| (Call ~ ident ~ exprList)											^^ {case _ ~ name ~ list => ProcCallComand(name, list, List())}
			| (QuestionMark ~> expr)											^^ {case ex => InputCommand(ex)}
			| (ExclamationMark ~> expr)											^^ {case ex => OutputCommand(ex)})  			
    
    def blockCmd : Parser[BlockCommand] = LBrace ~> cmd ~ rep(SemiColon ~> cmd) <~ RBrace ^^ {case c ~ rest => BlockCommand(c::rest)}
    def globInitList = ident ~ rep(Comma ~> ident) ^^ {case i ~ rest => i::rest}
    
    /* Expressions */
    def expr = term1 ~ rep(boolOpr ~ term1)     ^^ {case t1 ~ rest => toDyadicExpr(t1, rest)}
    def term1 = ( (term2 ~ relOpr ~ term2) 		^^ {case t1 ~ opr ~ t2 => DyadicExpr(opr, t1, t2)}
    			  | term2 						^^ {case t2 => t2}
    			) 
    def term2 = term3 ~ rep(addOpr ~ factor) 	^^ { case t3 ~ rest => toDyadicExpr(t3, rest)}
    def term3 = factor ~ rep(multOpr ~ factor) 	^^ {	case fac ~ rest => toDyadicExpr(fac, rest) }
    
    def toDyadicExpr(e:Expr, l:List[Opr ~ Expr]) : Expr = l match {
      case List() => e
      case (o~ex)::rest => 	DyadicExpr(o, e, toDyadicExpr(ex, rest))										  
    }
    												
    def factor : Parser[Expr] = ( literal 						^^ {case l => l}
		            		   | (ident ~ Init)					^^ {case i ~ init => StoreExpr(i, true)}
		            		   | (ident ~ exprList)				^^ {case i ~ list => FunCallExpr(i, list)}
		            		   | ident							^^ {case i => VarAccess(i) }
		            		   | (monadicOpr ~ factor)			^^ {case opr ~ fac => MonadicExpr(opr, fac)}
		            		   | (LParen ~> expr <~ RParen) 	^^ {case rest => rest} )
   
    def exprList = ( 
            	     (LParen ~> expr ~ rep(Comma ~> expr) <~ RParen) ^^ {case first ~ rest => first::rest} 
            	   | (LParen ~ RParen) ^^^ List()
            	   )
            	   
    def monadicOpr = (notOpr | addOpr)
    
    def notOpr = accept(Not) ^^^ NotOpr
    
    def boolOpr = (	 Or  ^^^ OrOpr
            	   | And ^^^ AndOpr) 
    def relOpr = (	 Equals 			^^^ EqualsOpr
    			   | NotEquals 			^^^ NotEqualsOpr
    			   | GreaterThan 		^^^ GreaterThanOpr
    			   | GreaterEqualsThan 	^^^ GreaterEqualsThanOpr
    			   | LessThan 			^^^ LessThanOpr
    			   | LessEqualsThan 	^^^ LessEqualsThanOpr)
    			   
    def addOpr:Parser[Opr] = (	 Plus 	^^^ PlusOpr
    			   		      | Minus 	^^^ MinusOpr)
    			   
    def multOpr = (  Times 	^^^ TimesOpr
    			   | Div 	^^^ DivOpr
    			   | Mod 	^^^ ModOpr)
    			   
    def imlType = (  Int32 	^^^ Int32Ast
    			   | Bool 	^^^ BoolAst)
    			   
    def flowMode = ( InOut	^^^ InOutFlow
    			   | Out	^^^ OutFlow
    			   | In		^^^ InFlow)
    			   
    def changeMode = ( Var 		^^^ VarAst
    				 | Const 	^^^ ConstAst )

    def mechMode = ( Ref		^^^ RefAst
    			   | Copy		^^^ CopyAst)
    				 
    def ident = elem("ident", _.isInstanceOf[Ident]) ^^ {case i => IdentAst(i.chars)}

    
    private def literal = elem("Literal", _.isInstanceOf[Literal]) ^^ { case IntLiteral(x) => IntLiteralExpression(x) 
																	    case True => BoolLiteralExpression(true)
																	    case False => BoolLiteralExpression(false)}
    
    def parse(source : Reader[Char]) : AnyRef = {
        val scanner = new lexical.Scanner(source)
        println(phrase(program)(scanner))
        
        return null;
    }
    
}