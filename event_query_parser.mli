type token =
  | LPAREN
  | RPAREN
  | AND
  | OR
  | EOF
  | STR of (string)
  | WHITESPACES of (string)
  | ATTENDING
  | DECLINED
  | INVITED
  | SUP
  | INF
  | EXCLM
  | EQ
  | COLON
  | DQUOTE
  | SQUOTE
  | LOCATION
  | OWNER
  | NAME

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Event_query_ast.expr
