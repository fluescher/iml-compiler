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
    
    case object Program extends Token with IMLTokenChars 
    case object EndProgram extends Token with IMLTokenChars 
    case object Command extends Token with IMLTokenChars 
    case object Declaration extends Token with IMLTokenChars
    case object Var extends Token with IMLTokenChars
    case object TypeSeparator extends Token with IMLTokenChars
    
    /* while */
    case object While extends Token with IMLTokenChars 
    case object Do extends Token with IMLTokenChars 
    case object EndWhile extends Token with IMLTokenChars 
    
    /* if */
    case object If extends Token with IMLTokenChars 
    case object Then extends Token with IMLTokenChars 
    case object Else extends Token with IMLTokenChars 
    case object EndIf extends Token with IMLTokenChars 
    
    /* Zuweisung */
    case object Becomes extends Token with IMLTokenChars 
    
    /* Arith */
    sealed class ArithOpr extends Token with IMLTokenChars 
    case object Plus extends ArithOpr 
    case object Minus extends ArithOpr
    
    /* Bool */
    sealed class RelOpr extends Token with IMLTokenChars 
    case object Equals extends RelOpr
    case object NotEquals extends RelOpr
    case object GreaterThan extends RelOpr
    case object GreaterEqualsThan extends RelOpr
    case object LessThan extends RelOpr
    case object LessEqualsThan extends RelOpr
    
    /* IO */
    sealed class IOOpr extends Token with IMLTokenChars 
    case object Read extends IOOpr
    case object Print extends IOOpr
    
    case class Ident(chars: String) extends Token {
        override def toString() = "Ident("+chars+")"
    }
    
    case class Delimiter(chars: String) extends Token   {
        override def toString() = "Delimiter("+chars+")"
    }
}

