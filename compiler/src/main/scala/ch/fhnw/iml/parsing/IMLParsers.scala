package ch.fhnw.iml.parsing

import scala.util.parsing.combinator.Parsers
import ch.fhnw.iml.scanning.IMLLexical
import ch.fhnw.iml.scanning.IMLTokens
import scala.util.parsing.combinator.syntactical.TokenParsers
import ch.fhnw.iml.scanning.IMLTokens
import scala.util.parsing.input.Reader
import ch.fhnw.iml.ast._;

class IMLParsers extends TokenParsers {
    type Tokens = IMLTokens
    
    val lexical = new IMLLexical
    import lexical.{Not, AddOpr, Literal, IntLiteral, Program, Ident, Global, LParen, 
        			RParen, ChangeMode, Colon, SemiColon, Int32, Fun, Returns, Local,
        			Proc, FlowMode, Comma, Skip, Becomes, If, Else, While, Call, QuestionMark,
        			ExclamationMark, Init, LBrace, RBrace,
        			MultOpr, RelOpr, BoolOpr, Type, Or, And, Ref, Copy,
        			Equals, NotEquals, GreaterThan, GreaterEqualsThan, LessThan, LessEqualsThan,
        			Minus, Plus, Times, Div, Mod, Bool, InOut, Out, In, Var, Const}
    
    /* Programs */
    def program = ((Program ~ ident ~ Global ~ cpsDecl ~ blockCmd) 
    			|  (Program ~ ident ~ blockCmd))
    
    /* declarations */
    def decl = (storeDecl 
    		 | funDecl 
    		 | procDecl)
    				 
    def storeDecl = opt(changeMode) ~ ident ~ Colon ~ imlType

    def funDecl = (   (funHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ blockCmd)
    				| (funHead ~ Global ~ globImpList ~ blockCmd) 
            		| (funHead ~ Local ~ cpsDecl ~ blockCmd)
            		| (funHead ~ blockCmd)
            	  )
    
    def funHead = Fun ~ ident ~ paramList ~ Returns ~ storeDecl
            	  
    def procDecl = (  
            		  (procHead ~ Global ~ globImpList ~ Local ~ cpsDecl ~ blockCmd)
    				| (procHead ~ Global ~ globImpList ~ blockCmd) 
            		| (procHead ~ Local ~ cpsDecl ~ blockCmd)
            		| (procHead ~ blockCmd)
            	  )

    def procHead = (Proc ~ ident ~ paramList) 
            	  
    def cpsDecl : Parser[Any] = decl ~ rep(SemiColon ~ decl)
    
    
    /* Parameter lists */
    def paramList = ( (LParen ~ RParen) 
            		| (LParen ~> param ~ rep(Comma ~> param) <~ RParen)
            		)
    
    def param = opt(flowMode) ~ opt(changeMode) ~ storeDecl
    
    def globImpList = globImp ~ rep(Comma ~> globImp)
    
    def globImp = (opt(flowMode)
    			~  opt(changeMode)
    			~  ident)
    
    /* Commands */
    def cmd = (Skip
			| (expr ~ Becomes ~ expr)
			| (If ~ LParen ~> expr <~ RParen ~ blockCmd ~ Else ~ blockCmd)
			| (While ~ LParen ~> expr <~ RParen ~ blockCmd)
			| (Call ~ ident ~ exprList ~ Init ~ globInitList)
			| (Call ~ ident ~ exprList)
			| (QuestionMark ~ expr)
			| (ExclamationMark ~ expr))
            			
    
    def blockCmd : Parser[Any] = LBrace ~> cmd ~ rep(SemiColon ~ cmd) <~ RBrace
    def globInitList = ident ~ rep(Comma ~> ident)
    
    /* Expressions */
    def expr = term1 ~ rep(boolOpr ~ term1)
    def term1 = ( (term2 ~ relOpr ~ term2) | term2)
    def term2 = term3 ~ rep(addOpr ~ factor)
    def term3 = factor ~ rep(multOpr ~ factor)
    
    def factor : Parser[Any] = ( literal 
		            		   | (ident ~ Init)
		            		   | (ident ~ exprList)
		            		   | ident
		            		   | (monadicOpr ~ factor)			^^ {case opr ~ fac => MonadicExpr(opr, fac)}
		            		   | (LParen ~> expr <~ RParen)) 	^^ {case rest => rest}
   
    def exprList = ( 
            	     (LParen ~> expr ~ rep(Comma ~> expr) <~ RParen) ^^ {case first ~ rest => first::rest} 
            	   | (LParen ~ RParen) ^^^ List()
            	   )
            	   
    def monadicOpr: Parser[Opr] = (notOpr | addOpr)
    
    def notOpr :Parser[Opr] = accept(Not) ^^^ NotOpr
    
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
    				 | Const 	^^^ ConstAst
    				 | Ref		^^^ RefAst
    				 | Copy		^^^ CopyAst)
    				 
    def ident = elem("ident", _.isInstanceOf[Ident]) ^^^ IdentAst

    
    private def literal = elem("Literal", _.isInstanceOf[Literal]) ^^ { case IntLiteral(x) => IntLiteralExpression(x) }
    
    def parse(source : Reader[Char]) : AnyRef = {
        val scanner = new lexical.Scanner(source)
        println(phrase(program)(scanner))
        
        return null;
    }
    
}