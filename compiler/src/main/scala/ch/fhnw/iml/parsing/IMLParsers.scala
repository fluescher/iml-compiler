package ch.fhnw.iml.parsing

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.input.Reader

import ch.fhnw.iml.ast._
import ch.fhnw.iml.scanning.IMLLexical
import ch.fhnw.iml.scanning.IMLTokens
import ch.fhnw.iml.scanning.IMLTokens

class IMLParsers extends TokenParsers {
    type Tokens = IMLTokens
    
    val lexical = new IMLLexical
    import lexical.{Not, AddOpr, Literal, IntLiteral, Program, Global, LParen, 
        			RParen, ChangeMode, Colon, SemiColon, Int32, Fun, Returns, Local,
        			Proc, FlowMode, Comma, Skip, Becomes, If, Else, While, Call, QuestionMark,
        			ExclamationMark, Init, LBrace, RBrace, True, False,
        			MultOpr, RelOpr, BoolOpr, Type, Or, And, Ref, Copy,
        			Equals, NotEquals, GreaterThan, GreaterEqualsThan, LessThan, LessEqualsThan,
        			Minus, Plus, Times, Div, Mod, Bool, InOut, Out, In, Var, Const,
        			LBracket, RBracket, Ensures, Requires}
    
    /* Programs */
    def program = positioned(  (Program ~ ident ~ Global ~ cpsDecl ~ blockCmd) 	^^ {case _ ~ i ~ _ ~ c ~ b => ProgramNode(i,c,b,EmptyTable) }
    						|  (Program ~ ident ~ blockCmd)						^^ {case _ ~ i ~ b => ProgramNode(i,CpsDecl(Nil),b, EmptyTable) }
    						)
    
    /* declarations */
    def decl = positioned( storeDecl
		    		 	 | funDecl
		    		 	 | procDecl
		    		 	 )
    				 
    def storeDecl : Parser[StoreDecl] = positioned((opt(changeMode) ~ ident ~ Colon ~ imlType) ^^ {case c ~ i ~ _ ~ t => StoreDecl(c.getOrElse(ConstNode), i, t)})

    def funDecl : Parser[FunDecl] = positioned(   
    				  (funHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ require ~ ensure ~ blockCmd)	^^ {case p ~ _ ~ g ~ _ ~ c ~ r ~ e ~ b => new FunDecl(p,Some(g),Some(c),Some(r),Some(e),b,EmptyTable)}
            		| (funHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ require ~ blockCmd)			^^ {case p ~ _ ~ g ~ _ ~ c ~ r ~ b => new FunDecl(p,Some(g),Some(c),Some(r),None,b,EmptyTable)}
            		| (funHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ ensure ~ blockCmd)			^^ {case p ~ _ ~ g ~ _ ~ c ~ e ~ b => new FunDecl(p,Some(g),Some(c),None,Some(e),b,EmptyTable)}
            		| (funHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ blockCmd)						^^ {case p ~ _ ~ g ~ _ ~ c ~ b => new FunDecl(p,Some(g),Some(c),None,None,b,EmptyTable)}
            		
    				| (funHead ~ Global ~ globImpList ~ require ~ ensure ~ blockCmd) 					^^ {case p ~ _ ~ g ~ r ~ e ~ b => new FunDecl(p,Some(g),None,Some(r),Some(e),b,EmptyTable)}
    				| (funHead ~ Global ~ globImpList ~ require ~ blockCmd) 							^^ {case p ~ _ ~ g ~ r ~  b => new FunDecl(p,Some(g),None,Some(r),None,b,EmptyTable)}
    				| (funHead ~ Global ~ globImpList ~ ensure ~ blockCmd) 								^^ {case p ~ _ ~ g ~ e ~ b => new FunDecl(p,Some(g),None,None,Some(e),b,EmptyTable)}
    				| (funHead ~ Global ~ globImpList ~ blockCmd) 										^^ {case p ~ _ ~ g ~ b => new FunDecl(p,Some(g),None,None,None,b,EmptyTable)}
    				
    				| (funHead ~ Local ~ cpsDecl ~ require ~ ensure ~ blockCmd)							^^ {case p ~ _ ~ c ~ r ~ e ~ b => new FunDecl(p,None,Some(c),Some(r),Some(e),b,EmptyTable)}
    				| (funHead ~ Local ~ cpsDecl ~ require ~ blockCmd)									^^ {case p ~ _ ~ c ~ r ~ b => new FunDecl(p,None,Some(c),Some(r),None,b,EmptyTable)}
    				| (funHead ~ Local ~ cpsDecl ~ ensure ~ blockCmd)									^^ {case p ~ _ ~ c ~ e ~ b => new FunDecl(p,None,Some(c),None,Some(e),b,EmptyTable)}
            		| (funHead ~ Local ~ cpsDecl ~ blockCmd)											^^ {case p ~ _ ~ c ~ b => new FunDecl(p,None,Some(c),None,None,b,EmptyTable)}
            		
            		| (funHead ~ require ~ ensure ~ blockCmd)											^^ {case p ~ r ~ e ~ b => new FunDecl(p,None,None,Some(r),Some(e),b,EmptyTable)}
            		| (funHead ~ require ~ blockCmd)													^^ {case p ~ r ~ b => new FunDecl(p,None,None,Some(r),None,b,EmptyTable)}
            		| (funHead ~ ensure ~ blockCmd)														^^ {case p ~ e ~ b => new FunDecl(p,None,None,None,Some(e),b,EmptyTable)}
            		| (funHead ~ blockCmd)																^^ {case p ~ b => new FunDecl(p,None,None,None,None,b,EmptyTable)}
            	  )
    
    def funHead = positioned((Fun ~ ident ~ paramList ~ Returns ~ storeDecl) ^^ {case _ ~ i ~ p ~ _ ~ s => FunHead(i,p,s)})
            	  
    def procDecl : Parser[ProcDecl] = positioned(  
            		  (procHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ require ~ ensure ~ blockCmd)	^^ {case p ~ _ ~ g ~ _ ~ c ~ r ~ e ~ b => new ProcDecl(p,Some(g),Some(c),Some(r),Some(e),b,EmptyTable)}
            		| (procHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ require ~ blockCmd)			^^ {case p ~ _ ~ g ~ _ ~ c ~ r ~ b => new ProcDecl(p,Some(g),Some(c),Some(r),None,b,EmptyTable)}
            		| (procHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ ensure ~ blockCmd)			^^ {case p ~ _ ~ g ~ _ ~ c ~ e ~ b => new ProcDecl(p,Some(g),Some(c),None,Some(e),b,EmptyTable)}
            		| (procHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ blockCmd)					^^ {case p ~ _ ~ g ~ _ ~ c ~ b => new ProcDecl(p,Some(g),Some(c),None,None,b,EmptyTable)}
            		
    				| (procHead ~ Global ~ globImpList ~ require ~ ensure ~ blockCmd) 					^^ {case p ~ _ ~ g ~ r ~ e ~ b => new ProcDecl(p,Some(g),None,Some(r),Some(e),b,EmptyTable)}
    				| (procHead ~ Global ~ globImpList ~ require ~ blockCmd) 							^^ {case p ~ _ ~ g ~ r ~  b => new ProcDecl(p,Some(g),None,Some(r),None,b,EmptyTable)}
    				| (procHead ~ Global ~ globImpList ~ ensure ~ blockCmd) 							^^ {case p ~ _ ~ g ~ e ~ b => new ProcDecl(p,Some(g),None,None,Some(e),b,EmptyTable)}
    				| (procHead ~ Global ~ globImpList ~ blockCmd) 										^^ {case p ~ _ ~ g ~ b => new ProcDecl(p,Some(g),None,None,None,b,EmptyTable)}
    				
    				| (procHead ~ Local ~ cpsDecl ~ require ~ ensure ~ blockCmd)						^^ {case p ~ _ ~ c ~ r ~ e ~ b => new ProcDecl(p,None,Some(c),Some(r),Some(e),b,EmptyTable)}
    				| (procHead ~ Local ~ cpsDecl ~ require ~ blockCmd)									^^ {case p ~ _ ~ c ~ r ~ b => new ProcDecl(p,None,Some(c),Some(r),None,b,EmptyTable)}
    				| (procHead ~ Local ~ cpsDecl ~ ensure ~ blockCmd)									^^ {case p ~ _ ~ c ~ e ~ b => new ProcDecl(p,None,Some(c),None,Some(e),b,EmptyTable)}
            		| (procHead ~ Local ~ cpsDecl ~ blockCmd)											^^ {case p ~ _ ~ c ~ b => new ProcDecl(p,None,Some(c),None,None,b,EmptyTable)}
            		
            		| (procHead ~ require ~ ensure ~ blockCmd)											^^ {case p ~ r ~ e ~ b => new ProcDecl(p,None,None,Some(r),Some(e),b,EmptyTable)}
            		| (procHead ~ require ~ blockCmd)													^^ {case p ~ r ~ b => new ProcDecl(p,None,None,Some(r),None,b,EmptyTable)}
            		| (procHead ~ ensure ~ blockCmd)													^^ {case p ~ e ~ b => new ProcDecl(p,None,None,None,Some(e),b,EmptyTable)}
            		| (procHead ~ blockCmd)																^^ {case p ~ b => new ProcDecl(p,None,None,None,None,b,EmptyTable)}
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
    def condition =positioned( expr	~ opt(Colon ~> ident) 									^^ {case expr ~ i  => Condition(i, expr) })
    
    
    /* Parameter lists */
    def paramList  = positioned( (LParen ~ RParen) 								^^ {case _ => ParameterList(List())}
            		| (LParen ~> param ~ rep(Comma ~> param) <~ RParen)	^^ {case first ~ rest => ParameterList(first::rest)}
            		)
    
    def param = positioned(opt(flowMode) ~ opt(mechMode) ~ storeDecl ^^ {case f ~ c ~ s => Parameter(f.getOrElse(InFlow), c.getOrElse(ch.fhnw.iml.ast.Copy), s)})
    
    def globImpList = positioned(globImp ~ rep(Comma ~> globImp) ^^ {case first ~ rest => GlobalImportList(first::rest)})
    
    def globImp = positioned((opt(flowMode) ~  opt(changeMode) ~  ident) ^^ {case f ~ c ~ i => GlobalImport(f.getOrElse(null), c.getOrElse(null),i)})
    
    			
    
    /* Commands */
    def cmd : Parser[Command] = positioned(Skip 								^^ {case s => SkipCommand}
			| (expr ~ Becomes ~ expr) 											^^ {case e1 ~ _ ~ e2 => AssiCommand(e1, e2) }
			| (If ~ LParen ~> expr ~ RParen ~ blockCmd ~ Else ~ blockCmd)		^^ {case ex ~ _ ~ cmd1 ~ _ ~ cmd2 => CondCommand(ex, cmd1, cmd2)}
			| (While ~ LParen ~> expr ~ RParen ~ blockCmd)						^^ {case ex ~ _ ~ cmd1 => WhileCommand(ex, cmd1)}
			| (Call ~> ident ~ exprList ~ Init ~ globInitList)					^^ {case name ~ list ~ _ ~ initlist => ProcCallCommand(name, list, initlist)}
			| (Call ~> ident ~ exprList)										^^ {case name ~ list => ProcCallCommand(name, list, List())}
			| (QuestionMark ~> expr)											^^ {case ex => InputCommand(ex)}
			| (ExclamationMark ~> expr)											^^ {case ex => OutputCommand(ex)})  			
    
    def blockCmd : Parser[BlockCommand] = positioned(LBrace ~> cmd ~ rep(SemiColon ~> cmd) <~ RBrace ^^ {case c ~ rest => BlockCommand(c::rest)})
    def globInitList = ident ~ rep(Comma ~> ident) ^^ {case i ~ rest => i::rest}
    
    /* Expressions */ 
    def expr = positioned(term1 ~ rep(boolOpr ~ term1)      ^^ {case t1 ~ rest => toDyadicExpr(t1, rest)})
    def term1 = positioned( term2 ~ relOpr ~ term2 			^^ {case t1 ~ opr ~ t2 => DyadicExpr(opr, t1, t2)}
		    			  	| term2 						^^ {case t2 => t2}
													    			) 
    def term2 = positioned(term3 ~ rep(addOpr ~ term3) 	^^ { case t3 ~ rest => toDyadicExpr(t3, rest)})
    def term3 = positioned(factor ~ rep(multOpr ~ factor) 	^^ { case fac ~ rest => toDyadicExpr(fac, rest) })
    
    def toDyadicExpr(e:Expr, l:List[Opr ~ Expr]) : Expr = l match {
      case List() => e
      case (o~ex)::rest => 	DyadicExpr(o, e, toDyadicExpr(ex, rest))										  
    }
    												
    def factor : Parser[Expr] = positioned( literal 			^^ {case l => l}
		            		   | (ident ~ Init)					^^ {case i ~ init => StoreExpr(i, true)}
		            		   | (ident ~ exprList)				^^ {case i ~ list => FunCallExpr(i, list)}
		            		   | ident							^^ {case i => StoreExpr(i, false) }
		            		   | (monadicOpr ~ factor)			^^ {case opr ~ fac => MonadicExpr(opr, fac)}
		            		   | (LParen ~> expr <~ RParen) 	^^ {case rest => rest} )
   
    def exprList = ( 
            	     (LParen ~> expr ~ rep(Comma ~> expr) <~ RParen) ^^ {case first ~ rest => first::rest} 
            	   | (LParen ~ RParen) ^^^ List()
            	   )
            	   
    def monadicOpr = positioned(notOpr | addOpr)
    
    def notOpr = positioned(accept(Not) ^^^ NotOpr)
    
    def boolOpr = positioned(	  Or  ^^^ OrOpr
            	   				| And ^^^ AndOpr) 
            	   				
    def relOpr = positioned(	 Equals 			^^^ EqualsOpr
			    			   | NotEquals 			^^^ NotEqualsOpr
			    			   | GreaterThan 		^^^ GreaterThanOpr
			    			   | GreaterEqualsThan 	^^^ GreaterEqualsThanOpr
			    			   | LessThan 			^^^ LessThanOpr
			    			   | LessEqualsThan 	^^^ LessEqualsThanOpr)
    			   
    def addOpr:Parser[Opr] = positioned(	  Plus 		^^^ PlusOpr
    			   		      				| Minus 	^^^ MinusOpr)
    			   
    def multOpr = positioned(  Times 	^^^ TimesOpr
			    			   | Div 	^^^ DivOpr
			    			   | Mod 	^^^ ModOpr)
    			   
    def imlType = (  Int32 				^^^ ch.fhnw.iml.ast.Int32
    			   | Bool 				^^^ ch.fhnw.iml.ast.Bool)
    			   
    def flowMode = positioned( InOut	^^^ InOutFlow
    			   | Out	^^^ OutFlow
    			   | In		^^^ InFlow)
    			   
    def changeMode = positioned( Var 		^^^ VarNode
    				 | Const 				^^^ ConstNode )

    def mechMode = positioned( Ref		^^^ ch.fhnw.iml.ast.Ref
    			   | Copy				^^^ ch.fhnw.iml.ast.Copy)
    				 
    def ident = positioned(elem("ident", _.isInstanceOf[lexical.Ident]) ^^ {case i => ch.fhnw.iml.ast.Ident(i.chars)})

    
    private def literal = positioned(elem("Literal", _.isInstanceOf[Literal]) ^^ { case IntLiteral(x) => IntLiteralExpression(x) 
																	    case True => BoolLiteralExpression(true)
																	    case False => BoolLiteralExpression(false)})
    
    def parse(source : String) = {
        val scanner = new lexical.Scanner(source)
       
        phrase(program)(scanner)
    }
    
}