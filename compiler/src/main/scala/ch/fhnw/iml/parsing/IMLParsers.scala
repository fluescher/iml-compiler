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
        			Minus, Plus, Times, Div, Mod, Bool, InOut, Out, In, Var, Const,
        			LBracket, RBracket, Ensures, Requires}
    
    /* Programs */
    def program = positioned((Program ~ ident ~ Global ~ cpsDecl ~ blockCmd) 	^^ {case _ ~ i ~ _ ~ c ~ b => ProgramAst(i,c,b)}
    			|  (Program ~ ident ~ blockCmd)						^^ {case _ ~ i ~ b => ProgramAst(i,null,b)}
    			)
    
    /* declarations */
    def decl = positioned(storeDecl
    		 	| funDecl
    		 	| procDecl)
    				 
    def storeDecl : Parser[StoreDecl] = positioned((opt(changeMode) ~ ident ~ Colon ~ imlType) ^^ {case c ~ i ~ _ ~ t => StoreDecl(c.getOrElse(null), i, t)})

    def funDecl : Parser[FunDecl] = positioned(   
    				  (funHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ require ~ ensure ~ blockCmd)	^^ {case p ~ _ ~ g ~ _ ~ c ~ r ~ e ~ b => new FunDecl(p,Some(g),Some(c),Some(r),Some(e),b)}
            		| (funHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ require ~ blockCmd)			^^ {case p ~ _ ~ g ~ _ ~ c ~ r ~ b => new FunDecl(p,Some(g),Some(c),Some(r),None,b)}
            		| (funHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ ensure ~ blockCmd)			^^ {case p ~ _ ~ g ~ _ ~ c ~ e ~ b => new FunDecl(p,Some(g),Some(c),None,Some(e),b)}
            		| (funHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ blockCmd)						^^ {case p ~ _ ~ g ~ _ ~ c ~ b => new FunDecl(p,Some(g),Some(c),None,None,b)}
            		
    				| (funHead ~ Global ~ globImpList ~ require ~ ensure ~ blockCmd) 					^^ {case p ~ _ ~ g ~ r ~ e ~ b => new FunDecl(p,Some(g),None,Some(r),Some(e),b)}
    				| (funHead ~ Global ~ globImpList ~ require ~ blockCmd) 							^^ {case p ~ _ ~ g ~ r ~  b => new FunDecl(p,Some(g),None,Some(r),None,b)}
    				| (funHead ~ Global ~ globImpList ~ ensure ~ blockCmd) 								^^ {case p ~ _ ~ g ~ e ~ b => new FunDecl(p,Some(g),None,None,Some(e),b)}
    				| (funHead ~ Global ~ globImpList ~ blockCmd) 										^^ {case p ~ _ ~ g ~ b => new FunDecl(p,Some(g),None,None,None,b)}
    				
    				| (funHead ~ Local ~ cpsDecl ~ require ~ ensure ~ blockCmd)							^^ {case p ~ _ ~ c ~ r ~ e ~ b => new FunDecl(p,None,Some(c),Some(r),Some(e),b)}
    				| (funHead ~ Local ~ cpsDecl ~ require ~ blockCmd)									^^ {case p ~ _ ~ c ~ r ~ b => new FunDecl(p,None,Some(c),Some(r),None,b)}
    				| (funHead ~ Local ~ cpsDecl ~ ensure ~ blockCmd)									^^ {case p ~ _ ~ c ~ e ~ b => new FunDecl(p,None,Some(c),None,Some(e),b)}
            		| (funHead ~ Local ~ cpsDecl ~ blockCmd)											^^ {case p ~ _ ~ c ~ b => new FunDecl(p,None,Some(c),None,None,b)}
            		
            		| (funHead ~ require ~ ensure ~ blockCmd)											^^ {case p ~ r ~ e ~ b => new FunDecl(p,None,None,Some(r),Some(e),b)}
            		| (funHead ~ require ~ blockCmd)													^^ {case p ~ r ~ b => new FunDecl(p,None,None,Some(r),None,b)}
            		| (funHead ~ ensure ~ blockCmd)														^^ {case p ~ e ~ b => new FunDecl(p,None,None,None,Some(e),b)}
            		| (funHead ~ blockCmd)																^^ {case p ~ b => new FunDecl(p,None,None,None,None,b)}
            	  )
    
    def funHead = positioned((Fun ~ ident ~ paramList ~ Returns ~ storeDecl) ^^ {case _ ~ i ~ p ~ _ ~ s => FunHead(i,p,s)})
            	  
    def procDecl : Parser[ProcDecl] = positioned(  
            		  (procHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ require ~ ensure ~ blockCmd)	^^ {case p ~ _ ~ g ~ _ ~ c ~ r ~ e ~ b => new ProcDecl(p,Some(g),Some(c),Some(r),Some(e),b)}
            		| (procHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ require ~ blockCmd)			^^ {case p ~ _ ~ g ~ _ ~ c ~ r ~ b => new ProcDecl(p,Some(g),Some(c),Some(r),None,b)}
            		| (procHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ ensure ~ blockCmd)			^^ {case p ~ _ ~ g ~ _ ~ c ~ e ~ b => new ProcDecl(p,Some(g),Some(c),None,Some(e),b)}
            		| (procHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ blockCmd)					^^ {case p ~ _ ~ g ~ _ ~ c ~ b => new ProcDecl(p,Some(g),Some(c),None,None,b)}
            		
    				| (procHead ~ Global ~ globImpList ~ require ~ ensure ~ blockCmd) 					^^ {case p ~ _ ~ g ~ r ~ e ~ b => new ProcDecl(p,Some(g),None,Some(r),Some(e),b)}
    				| (procHead ~ Global ~ globImpList ~ require ~ blockCmd) 							^^ {case p ~ _ ~ g ~ r ~  b => new ProcDecl(p,Some(g),None,Some(r),None,b)}
    				| (procHead ~ Global ~ globImpList ~ ensure ~ blockCmd) 							^^ {case p ~ _ ~ g ~ e ~ b => new ProcDecl(p,Some(g),None,None,Some(e),b)}
    				| (procHead ~ Global ~ globImpList ~ blockCmd) 										^^ {case p ~ _ ~ g ~ b => new ProcDecl(p,Some(g),None,None,None,b)}
    				
    				| (procHead ~ Local ~ cpsDecl ~ require ~ ensure ~ blockCmd)						^^ {case p ~ _ ~ c ~ r ~ e ~ b => new ProcDecl(p,None,Some(c),Some(r),Some(e),b)}
    				| (procHead ~ Local ~ cpsDecl ~ require ~ blockCmd)									^^ {case p ~ _ ~ c ~ r ~ b => new ProcDecl(p,None,Some(c),Some(r),None,b)}
    				| (procHead ~ Local ~ cpsDecl ~ ensure ~ blockCmd)									^^ {case p ~ _ ~ c ~ e ~ b => new ProcDecl(p,None,Some(c),None,Some(e),b)}
            		| (procHead ~ Local ~ cpsDecl ~ blockCmd)											^^ {case p ~ _ ~ c ~ b => new ProcDecl(p,None,Some(c),None,None,b)}
            		
            		| (procHead ~ require ~ ensure ~ blockCmd)											^^ {case p ~ r ~ e ~ b => new ProcDecl(p,None,None,Some(r),Some(e),b)}
            		| (procHead ~ require ~ blockCmd)													^^ {case p ~ r ~ b => new ProcDecl(p,None,None,Some(r),None,b)}
            		| (procHead ~ ensure ~ blockCmd)													^^ {case p ~ e ~ b => new ProcDecl(p,None,None,None,Some(e),b)}
            		| (procHead ~ blockCmd)																^^ {case p ~ b => new ProcDecl(p,None,None,None,None,b)}
            	  )

    def procHead = positioned((Proc ~ ident ~ paramList) ^^ {case _ ~ i ~ p => ProcHead(i, p)})
            	  
    def cpsDecl = positioned(decl ~ rep(SemiColon ~> decl) ^^ {case first ~ rest => CpsDecl(first::rest)})
    
    
    /* Pre-Postconditions */
    def require		= positioned(Requires ~> conditionList)	
    def ensure		= positioned(Ensures ~> conditionList)
    
    def conditionList = positioned(
            			LBracket ~> condition ~ rep(Comma ~> condition) <~ RBracket			^^ {case cond ~ rest  => ConditionList(cond::rest)}
    				  | LBracket ~ RBracket													^^^ ConditionList(List())
    				  )
    def condition =positioned( opt(ident <~ Colon) ~ expr									^^ {case i ~ expr => Condition(i, expr) })
    
    
    /* Parameter lists */
    def paramList  = positioned( (LParen ~ RParen) 								^^ {case _ => ParameterList(List())}
            		| (LParen ~> param ~ rep(Comma ~> param) <~ RParen)	^^ {case first ~ rest => ParameterList(first::rest)}
            		)
    
    def param = positioned(opt(flowMode) ~ opt(mechMode) ~ storeDecl ^^ {case f ~ c ~ s => Parameter(f.getOrElse(null), c.getOrElse(null), s)})
    
    def globImpList = positioned(globImp ~ rep(Comma ~> globImp) ^^ {case first ~ rest => GlobalImportList(first::rest)})
    
    def globImp = positioned((opt(flowMode) ~  opt(changeMode) ~  ident) ^^ {case f ~ c ~ i => GlobalImport(f.getOrElse(null), c.getOrElse(null),i)})
    
    			
    
    /* Commands */
    def cmd : Parser[Command] = positioned(Skip 								^^ {case s => SkipCommand()}
			| (expr ~ Becomes ~ expr) 											^^ {case e1 ~ _ ~ e2 => AssiCommand(e1, e2) }
			| (If ~ LParen ~> expr ~ RParen ~ blockCmd ~ Else ~ blockCmd)		^^ {case ex ~ _ ~ cmd1 ~ _ ~ cmd2 => CondCommand(ex, cmd1, cmd2)}
			| (While ~ LParen ~> expr ~ RParen ~ blockCmd)						^^ {case ex ~ _ ~ cmd1 => WhileCommand(ex, cmd1)}
			| (Call ~> ident ~ exprList ~ Init ~ globInitList)					^^ {case name ~ list ~ _ ~ initlist => ProcCallComand(name, list, initlist)}
			| (Call ~> ident ~ exprList)										^^ {case name ~ list => ProcCallComand(name, list, List())}
			| (QuestionMark ~> expr)											^^ {case ex => InputCommand(ex)}
			| (ExclamationMark ~> expr)											^^ {case ex => OutputCommand(ex)})  			
    
    def blockCmd : Parser[BlockCommand] = positioned(LBrace ~> cmd ~ rep(SemiColon ~> cmd) <~ RBrace ^^ {case c ~ rest => BlockCommand(c::rest)})
    def globInitList = ident ~ rep(Comma ~> ident) ^^ {case i ~ rest => i::rest}
    
    /* Expressions */
    def expr = positioned(term1 ~ rep(boolOpr ~ term1)     ^^ {case t1 ~ rest => toDyadicExpr(t1, rest)})
    def term1 = positioned( (term2 ~ relOpr ~ term2) 		^^ {case t1 ~ opr ~ t2 => DyadicExpr(opr, t1, t2)}
    			  | term2 						^^ {case t2 => t2}
    			) 
    def term2 = positioned(term3 ~ rep(addOpr ~ factor) 	^^ { case t3 ~ rest => toDyadicExpr(t3, rest)})
    def term3 = positioned(factor ~ rep(multOpr ~ factor) 	^^ { case fac ~ rest => toDyadicExpr(fac, rest) })
    
    def toDyadicExpr(e:Expr, l:List[Opr ~ Expr]) : Expr = l match {
      case List() => e
      case (o~ex)::rest => 	DyadicExpr(o, e, toDyadicExpr(ex, rest))										  
    }
    												
    def factor : Parser[Expr] = positioned( literal 						^^ {case l => l}
		            		   | (ident ~ Init)					^^ {case i ~ init => StoreExpr(i, true)}
		            		   | (ident ~ exprList)				^^ {case i ~ list => FunCallExpr(i, list)}
		            		   | ident							^^ {case i => VarAccess(i) }
		            		   | (monadicOpr ~ factor)			^^ {case opr ~ fac => MonadicExpr(opr, fac)}
		            		   | (LParen ~> expr <~ RParen) 	^^ {case rest => rest} )
   
    def exprList = ( 
            	     (LParen ~> expr ~ rep(Comma ~> expr) <~ RParen) ^^ {case first ~ rest => first::rest} 
            	   | (LParen ~ RParen) ^^^ List()
            	   )
            	   
    def monadicOpr = positioned(notOpr | addOpr)
    
    def notOpr = positioned(accept(Not) ^^^ NotOpr)
    
    def boolOpr = positioned(	 Or  ^^^ OrOpr
            	   | And ^^^ AndOpr) 
    def relOpr = positioned(	 Equals 			^^^ EqualsOpr
    			   | NotEquals 			^^^ NotEqualsOpr
    			   | GreaterThan 		^^^ GreaterThanOpr
    			   | GreaterEqualsThan 	^^^ GreaterEqualsThanOpr
    			   | LessThan 			^^^ LessThanOpr
    			   | LessEqualsThan 	^^^ LessEqualsThanOpr)
    			   
    def addOpr:Parser[Opr] = positioned(	 Plus 	^^^ PlusOpr
    			   		      | Minus 	^^^ MinusOpr)
    			   
    def multOpr = positioned(  Times 	^^^ TimesOpr
    			   | Div 	^^^ DivOpr
    			   | Mod 	^^^ ModOpr)
    			   
    def imlType = positioned(  Int32 	^^^ Int32Ast
    			   | Bool 	^^^ BoolAst)
    			   
    def flowMode = positioned( InOut	^^^ InOutFlow
    			   | Out	^^^ OutFlow
    			   | In		^^^ InFlow)
    			   
    def changeMode = positioned( Var 		^^^ VarAst
    				 | Const 	^^^ ConstAst )

    def mechMode = positioned( Ref		^^^ RefAst
    			   | Copy		^^^ CopyAst)
    				 
    def ident = positioned(elem("ident", _.isInstanceOf[Ident]) ^^ {case i => IdentAst(i.chars)})

    
    private def literal = positioned(elem("Literal", _.isInstanceOf[Literal]) ^^ { case IntLiteral(x) => IntLiteralExpression(x) 
																	    case True => BoolLiteralExpression(true)
																	    case False => BoolLiteralExpression(false)})
    
    def parse(source : Reader[Char]) = {
        val scanner = new lexical.Scanner(source)
       
        phrase(program)(scanner)
    }
    
}