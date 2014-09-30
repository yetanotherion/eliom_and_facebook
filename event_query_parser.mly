%token LPAREN RPAREN AND OR EOF
%token <string> STR WHITESPACES
%left OR
%token ATTENDING DECLINED INVITED
%token SUP INF EXCLM EQ COLON
%token DQUOTE SQUOTE
%token LOCATION OWNER NAME
%start main
%type <Event_query_ast.expr> main
%%

main:
    stripped_expr EOF { $1 }

expr:
   LPAREN stripped_expr RPAREN { $2 }
 | expr WHITESPACES expr { `And ($1, $3)}
 | expr WHITESPACES OR WHITESPACES expr { `Or ($1, $5) }
 | one_rule { `Single $1 }

stripped_expr:
 | WHITESPACES expr { $2 }
 | expr WHITESPACES { $1 }
 | WHITESPACES expr WHITESPACES { $2 }
 | expr { $1 }

one_rule:
   ATTENDING op int { `Attending ($2, $3)}
 | DECLINED op int { `Declined ($2, $3)}
 | INVITED op int { `Invited ($2, $3)}
 | LOCATION optional_ws_eqop str { `Location ($2, $3) }
 | OWNER optional_ws_eqop str { `Owner ($2, $3) }
 | NAME optional_ws_eqop str { `Name ($2, $3) }

int:
   STR { int_of_string($1) }

str:
   STR { $1 }
 | DQUOTE in_quote DQUOTE { $2 }
 | SQUOTE in_quote SQUOTE { $2 }

in_quote:
 | string_or_ws in_quote { $1 ^ $2 }
 | string_or_ws { $1 }

string_or_ws:
 | WHITESPACES { $1 }
 | STR { $1 }

op:
   optional_ws_diffop { `Diffop $1 }
 | optional_ws_eqop { `Eqop $1 }

optional_ws_diffop:
 | diff_op { $1 }
 | diff_op WHITESPACES { $1 }
 | WHITESPACES diff_op { $2 }
 | WHITESPACES diff_op WHITESPACES { $2 }

diff_op:
 | SUP EQ { `Gte }
 | INF EQ { `Lte }
 | SUP { `Gt }
 | INF { `Lt }

optional_ws_eqop:
 | eq_op { $1 }
 | eq_op WHITESPACES { $1 }
 | WHITESPACES eq_op { $2 }
 | WHITESPACES eq_op WHITESPACES { $2 }

eq_op:
 | EQ { `Eq }
 | COLON { `Eq }
 | EXCLM EQ { `Neq }
