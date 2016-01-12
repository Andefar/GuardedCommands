// Signature file for parser generated by fsyacc
module Parser
type token = 
  | HIGH
  | EOF
  | PRINT
  | ASG
  | SKIP
  | ABORT
  | FUN
  | RETURN
  | PROC
  | NEG
  | PLUS
  | MINUS
  | TIMES
  | AND
  | EQ
  | LE
  | LT
  | GT
  | NEQ
  | COMMA
  | COLON
  | SEMI
  | BAR
  | TO
  | IF
  | FI
  | DO
  | OD
  | BEGIN
  | END
  | LP
  | LCP
  | LSP
  | RP
  | RCP
  | RSP
  | ITYP
  | BTYP
  | NAME of (string)
  | STRING of (string)
  | BOOL of (bool)
  | INT of (int)
type tokenId = 
    | TOKEN_HIGH
    | TOKEN_EOF
    | TOKEN_PRINT
    | TOKEN_ASG
    | TOKEN_SKIP
    | TOKEN_ABORT
    | TOKEN_FUN
    | TOKEN_RETURN
    | TOKEN_PROC
    | TOKEN_NEG
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_AND
    | TOKEN_EQ
    | TOKEN_LE
    | TOKEN_LT
    | TOKEN_GT
    | TOKEN_NEQ
    | TOKEN_COMMA
    | TOKEN_COLON
    | TOKEN_SEMI
    | TOKEN_BAR
    | TOKEN_TO
    | TOKEN_IF
    | TOKEN_FI
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_BEGIN
    | TOKEN_END
    | TOKEN_LP
    | TOKEN_LCP
    | TOKEN_LSP
    | TOKEN_RP
    | TOKEN_RCP
    | TOKEN_RSP
    | TOKEN_ITYP
    | TOKEN_BTYP
    | TOKEN_NAME
    | TOKEN_STRING
    | TOKEN_BOOL
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM__startProg
    | NONTERM_Main
    | NONTERM_Prog
    | NONTERM_BasicTyp
    | NONTERM_Typ
    | NONTERM_Dec
    | NONTERM_FunDec
    | NONTERM_DecL
    | NONTERM_DecList
    | NONTERM_Access
    | NONTERM_Stm
    | NONTERM_StmL
    | NONTERM_StmList
    | NONTERM_GuardedCommand
    | NONTERM_GCList
    | NONTERM_Exp
    | NONTERM_ExpL
    | NONTERM_ExpList
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val Main : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Program) 
val Prog : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Program) 
