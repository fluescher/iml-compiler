package ch.fhnw.iml.parsing

import scala.util.parsing.combinator.Parsers
import ch.fhnw.iml.scanning.IMLLexical
import ch.fhnw.iml.scanning.IMLTokens
import scala.util.parsing.combinator.syntactical.TokenParsers
import ch.fhnw.iml.scanning.IMLTokens
import scala.util.parsing.input.Reader

class IMLParsers extends TokenParsers {
    type Tokens = IMLTokens
    
    val lexical = new IMLLexical
    import lexical.{Not, AddOpr, Literal, IntLiteral, Program, Ident, Global, LParen, 
        			RParen, ChangeMode, Colon, SemiColon, Int32, Fun, Returns, Local,
        			Proc, FlowMode, Comma, Skip, Becomes, If, Else, While, Call, QuestionMark,
        			ExclamationMark, Init, LBrace, RBrace,
        			MultOpr, RelOpr, BoolOpr, Type}
    
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

    def procHead = Proc ~ ident ~ paramList
            	  
    def cpsDecl : Parser[Any] = decl ~ rep(SemiColon ~ decl)
    
    
    /* Parameter lists */
    def paramList = ( (LParen ~ RParen)
            		| (LParen ~> param ~ rep(Comma ~ param) <~ RParen))
    
    def param = opt(flowMode) ~ opt(changeMode) ~ storeDecl
    
    def globImpList = globImp ~ rep(Comma ~ globImp)
    
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
    def globInitList = ident ~ rep(Comma ~ ident)
    
    /* Expressions */
    def expr = term1 ~ rep(boolOpr ~ term1)
    def term1 = ( (term2 ~ relOpr ~ term2) | term2)
    def term2 = term3 ~ rep(addOpr ~ factor)
    def term3 = factor ~ rep(multOpr ~ factor)
    
    def factor : Parser[Any] = ( literal
		            		   | (ident ~ opt(Init | exprList))
		            		   | (monadicOpr ~ factor)
		            		   | (LParen ~> expr <~ RParen))
   
    def exprList = ( 
            	     (LParen ~> expr ~ rep(Comma ~ expr) <~ RParen)
            	   | (LParen ~ RParen)
            	   )
    def monadicOpr = Not | addOpr
    
    def boolOpr = elem("boolopr", _.isInstanceOf[BoolOpr])
    def relOpr = elem("relopr", _.isInstanceOf[RelOpr])
    def addOpr = elem("addopr", _.isInstanceOf[AddOpr])
    def multOpr = elem("multopr", _.isInstanceOf[MultOpr])
    def imlType = elem("type", _.isInstanceOf[Type])
    def flowMode = elem("flowmode", _.isInstanceOf[FlowMode])
    def changeMode = elem("changemode", _.isInstanceOf[ChangeMode])
    def ident = elem("identifier", _.isInstanceOf[Ident])

    
    private def literal = elem("Literal", _.isInstanceOf[Literal]) ^^ { case IntLiteral(x) => x }
    
    def parse(source : Reader[Char]) : AnyRef = {
        val scanner = new lexical.Scanner(source)
        println(phrase(program)(scanner))
        
        return null;
    }
    
}