datatype term
  = LPAREN
  | RPAREN
  | COMMA
  | SEMICOLON
  | COLON
  | QUESTMARK
  | EXCLAMARK
  | BECOMES
  | LBRACE
  | RBRACE
  | MULTOPR
  | ADDOPR
  | RELOPR
  | TYPE
  | CALL
  | BOOLOPR
  | CHANGEMODE
  | MECHMODE
  | ELSE
  | LITERAL
  | FUN
  | GLOBAL
  | IF
  | FLOWMODE
  | INIT
  | LOCAL
  | NOT
  | PROC
  | PROGRAM
  | RETURNS
  | SKIP
  | WHILE
  | IDENT
  | LBRACKET
  | RBRACKET
  | REQUIRES
  | ENSURES

val string_of_term =
  fn LPAREN  => "LPAREN"
   | RPAREN   => "RPAREN"
   | COMMA  => "COMMA"
   | SEMICOLON => "SEMICOLON"
   | COLON  => "COLON"
   | QUESTMARK => "QUESTMARK"
   | EXCLAMARK => "EXCLAMARK"
   | BECOMES => "BECOMES"
   | LBRACE => "LBRACE"
   | RBRACE => "RBRACE"
   | MULTOPR => "MULTOPR"
   | ADDOPR => "ADDOPR"
   | RELOPR => "RELOPR"
   | TYPE => "TYPE"
   | CALL => "CALL"
   | BOOLOPR => "BOOLOPR"
   | CHANGEMODE => "CHANGEMODE"
   | MECHMODE => "MECHMODE"
   | ELSE => "ELSE"
   | LITERAL => "LITERAL"
   | FUN => "FUN"
   | GLOBAL => "GLOBAL"
   | IF => "IF"
   | FLOWMODE => "FLOWMODE"
   | INIT => "INIT"
   | LOCAL => "LOCAL"
   | NOT => "NOT"
   | PROC => "PROC"
   | PROGRAM => "PROGRAM"
   | RETURNS => "RETURNS"
   | SKIP => "SKIP"
   | WHILE => "WHILE"
   | IDENT => "IDENT"
   | LBRACKET => "LBRACKET"
   | RBRACKET => "RBRACKET"
   | REQUIRES => "REQUIRES"
   | ENSURES => "ENSURES"

datatype nonterm
  = program
  | optGlobalCpsDecl
  | cpsDecl
  | repDecl
  | blockCmd
  | decl
  | storeDecl
  | funDecl
  | optGlobImpList
  | optLocalCpsDecl
  | procDecl
  | paramList
  | optParam
  | repParam
  | globImplList
  | repGlobImp
  | param
  | optFlowMode
  | optMechMode
  | globImp
  | optChangeMode
  | cmd
  | optGlobInitList
  | repCmd
  | expr
  | exprList
  | globInitList
  | repIdent
  | term1
  | repBoolOprTerm1
  | term2
  | optRelOprTerm2
  | term3
  | repAddOprTerm3
  | factor
  | optFactorIdent
  | repMultOprFactor
  | optExpr
  | repExpr
  | monadicOpr
  | condition
  | optCondition
  | conditionList
  | repCondition
  | optIdent
  | requires
  | optRequires
  | ensures
  | optEnsures

val string_of_nonterm =
  fn program => "program"
   | optGlobalCpsDecl => "optGlobalCpsDecl"
   | cpsDecl => "cpsDecl"
   | repDecl => "repDecl"
   | blockCmd => "blockCmd"
   | decl => "decl"
   | storeDecl => "storeDecl"
   | funDecl => "funDecl"
   | optGlobImpList => "optGlobImpList"
   | optLocalCpsDecl => "optLocalCpsDecl"
   | procDecl => "procDecl"
   | paramList => "paramList"
   | optParam => "optParam"
   | repParam => "repParam"
   | globImplList => "globImplList"
   | repGlobImp => "repGlobImp"
   | param => "param"
   | optFlowMode => "optFlowMode"
   | optMechMode => "optMechMode"
   | globImp => "globImp"
   | optChangeMode => "optChangeMode"
   | cmd => "cmd"
   | optGlobInitList => "optGlobInitList"
   | repCmd => "repCmd"
   | expr => "expr"
   | exprList => "exprList"
   | globInitList => "globInitList"
   | repIdent => "repIdent"
   | term1 => "term1"
   | repBoolOprTerm1 => "repBoolOprTerm1"
   | term2 => "term2"
   | optRelOprTerm2 => "optRelOprTerm2"
   | term3 => "term3"
   | repAddOprTerm3 => "repAddOprTerm3"
   | factor => "factor"
   | optFactorIdent => "optFactorIdent"
   | repMultOprFactor => "repMultOprFactor"
   | optExpr => "optExpr"
   | repExpr => "repExpr"
   | monadicOpr => "monadicOpr"
   | condition => "condition"
   | optCondition => "optCondition"
   | conditionList => "conditionList"
   | repCondition => "repCondition"
   | optIdent => "optIdent"
   | requires => "requires"
   | optRequires => "optRequires"
   | ensures => "ensures"
   | optEnsures => "optEnsures"
	
val string_of_gramsym = (string_of_term, string_of_nonterm)

local
  open FixFoxi.FixFoxiCore
in

val productions =
[
(program,
    [[T PROGRAM, T IDENT, N optGlobalCpsDecl, N blockCmd]]),
(optGlobalCpsDecl,
    [[T GLOBAL, N cpsDecl],
    []]),
(decl,
    [[N storeDecl],
    [N funDecl],
    [N procDecl]]),
(storeDecl,
    [[T CHANGEMODE, T IDENT, T COLON, T TYPE],
    [T IDENT, T COLON, T TYPE]]),
(funDecl,
    [[T FUN, T IDENT, N paramList, T RETURNS, N storeDecl, N optGlobImpList, N optLocalCpsDecl, N optRequires, N optEnsures, N blockCmd]]),
(optGlobImpList,
    [[T GLOBAL, N globImplList],
    []]),
(optLocalCpsDecl,
    [[T LOCAL, N cpsDecl],
    []]),
(optRequires,
    [[T REQUIRES, N conditionList],
    []]),
(optEnsures,
    [[T ENSURES, N conditionList],
    []]),
(procDecl,
    [[T PROC, T IDENT, N paramList, N optGlobImpList, N optLocalCpsDecl, N optRequires, N optEnsures, N blockCmd]]),
(cpsDecl,
    [[N decl, N repDecl]]),
(repDecl,
    [[T SEMICOLON, N decl, N repDecl],
    []]),
(paramList,
    [[T LPAREN, N optParam, T RPAREN]]),
(optParam,
    [[N param, N repParam],
    []]),
(repParam,
    [[T COMMA, N repParam],
    []]),
(param,
    [[N optFlowMode, N optMechMode, N storeDecl]]),
(optFlowMode,
    [[T FLOWMODE],
    []]),
(optMechMode,
    [[T MECHMODE],
    []]),
(conditionList,
    [[T LBRACKET, N optCondition, T RBRACKET]]),
(optCondition,
    [[N condition, N repCondition],
    []]),
(repCondition,
    [[T COMMA, N repCondition],
    []]),
(condition,
    [[N optIdent, N expr]]),
(optIdent,
    [[T IDENT, T COLON],
    []]),
(globImplList,
    [[N globImp, N repGlobImp]]),
(repGlobImp,
    [[T COMMA, N globImp, N repGlobImp],
    []]),
(globImp,
    [[N optFlowMode, N optChangeMode, T IDENT]]),
(optChangeMode,
    [[T CHANGEMODE],
    []]),	
(cmd,
    [[T SKIP],
    [N expr, T BECOMES, N expr],
    [T IF, T LPAREN, N expr, T RPAREN, N blockCmd, T ELSE, N blockCmd],
    [T WHILE, T LPAREN, N expr, T RPAREN, N blockCmd],
    [T CALL, T IDENT, N exprList, N optGlobInitList],
    [T QUESTMARK, N expr],
    [T EXCLAMARK, N expr]]),
(optGlobInitList,
    [[T INIT, N globInitList],
    []]),
(blockCmd,
    [[T LBRACE, N cmd, N repCmd, T RBRACE]]),
(repCmd,
    [[T SEMICOLON, N cmd, N repCmd],
    []]),
(globInitList,
    [[T IDENT, N repIdent]]),
(repIdent,
    [[T COMMA, T IDENT, N repIdent],
    []]),
(expr,
    [[N term1, N repBoolOprTerm1]]),
(repBoolOprTerm1,
    [[T BOOLOPR, N term1, N repBoolOprTerm1],
    []]),
(term1,
    [[N term2, N optRelOprTerm2]]),
(optRelOprTerm2,
    [[T RELOPR, N term2],
    []]),
(term2,
    [[N term3, N repAddOprTerm3]]),
(repAddOprTerm3,
    [[T ADDOPR, N term3, N repAddOprTerm3],
    []]),
(term3,
    [[N factor, N repMultOprFactor]]),
(repMultOprFactor,
    [[T MULTOPR, N factor, N repMultOprFactor],
    []]),
(factor,
    [[T LITERAL],
    [T IDENT, N optFactorIdent],
    [N monadicOpr, N factor],
    [T LPAREN, N expr, T RPAREN]]),
(optFactorIdent,
    [[T INIT],
    [N exprList],
    []]),
(exprList,
    [[T LPAREN, N optExpr, T RPAREN]]),
(optExpr,
    [[N expr, N repExpr],
    []]),
(repExpr,
    [[T COMMA, N expr, N repExpr],
    []]),
(monadicOpr,
    [[T NOT],
    [T ADDOPR]])
]

val S = program

val result = fix_foxi productions S string_of_gramsym

end (* local *)