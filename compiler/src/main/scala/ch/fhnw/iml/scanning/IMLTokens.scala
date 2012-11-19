package ch.fhnw.iml.scanning

import scala.util.parsing.combinator.token.Tokens

trait IMLTokens extends Tokens {
    /**
     * Implements chars of the Token trait. Uses the classname as
     * String representation.
     */
    trait IMLTokenChars {
        def chars = IMLTokenChars.this.toString()
    }
    
    sealed abstract class Keyword extends Token with IMLTokenChars
    case object Program extends Keyword
    case object Call extends Keyword
    case object Returns extends Keyword
    case object Local extends Keyword
    case object Not extends Keyword
    case object Fun extends Keyword
    case object Global extends Keyword
    case object Init extends Keyword
    case object Proc extends Keyword
    case object Skip extends Keyword
    case object Ensures extends Keyword
    case object Requires extends Keyword
    
    sealed abstract class ChangeMode extends Keyword
    case object Var extends ChangeMode
    case object Const extends ChangeMode

    sealed abstract class MechMode extends ChangeMode
    case object Ref extends MechMode
    case object Copy extends MechMode
    
    /* types 14*/
    sealed abstract class Type extends Keyword
    case object Int32 extends Type
    case object Bool extends Type
    
    /* flow control16 */
    sealed abstract class FlowMode extends Keyword
    case object InOut extends FlowMode
    case object Out extends FlowMode
    case object In extends FlowMode
    
    /* while 19*/
    case object While extends Keyword
    case object Do extends Keyword
    
    /* if 21*/
    case object If extends Token with IMLTokenChars 
    case object Else extends Token with IMLTokenChars 
    
    case class Ident(chars: String) extends Token {
        override def toString() = "Ident("+chars+")"
    }
    
    /* literals */
    sealed abstract class Literal extends Token
    case class IntLiteral(v: Int) extends Literal {
        def chars = v.toString
        override def toString() = "IntLiteral("+chars+")"
    }
    sealed abstract class BoolLiteral(v: Boolean) extends Literal {
        def chars = v.toString
        override def toString() = "BoolLiteral("+chars+")"
    }
    case object True extends BoolLiteral(true)
    case object False extends BoolLiteral(false)
    
    /* symbols */
    sealed abstract class Symbol extends Token with IMLTokenChars
    case object LParen extends Symbol
    case object RParen extends Symbol
    case object Comma extends Symbol
    case object SemiColon extends Symbol
    case object Colon extends Symbol
    case object QuestionMark extends Symbol
    case object ExclamationMark extends Symbol
    case object Becomes extends Symbol
    case object LBrace extends Symbol
    case object RBrace extends Symbol
    case object LBracket extends Symbol
    case object RBracket extends Symbol
    
    /* Arith 38*/
    sealed abstract class ArithOpr extends Symbol with IMLTokenChars 
    
    /* Add */
    sealed abstract class AddOpr extends ArithOpr
    case object Plus extends AddOpr 
    case object Minus extends AddOpr
    
    /* Mult */
    sealed abstract class MultOpr extends ArithOpr
    case object Times extends MultOpr
    case object Div extends MultOpr
    case object Mod extends MultOpr
    
    /* Rel */
    sealed abstract class RelOpr extends Symbol with IMLTokenChars 
    case object Equals extends RelOpr
    case object NotEquals extends RelOpr
    case object GreaterThan extends RelOpr
    case object GreaterEqualsThan extends RelOpr
    case object LessThan extends RelOpr
    case object LessEqualsThan extends RelOpr
    
    /* bool */
    sealed abstract class BoolOpr extends Keyword with IMLTokenChars
    case object And extends BoolOpr
    case object Or extends BoolOpr
}