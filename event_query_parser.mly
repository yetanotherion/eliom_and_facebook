%token LPAREN RPAREN AND OR EOF
%left OR
%token ATTENDING DECLINED INVITED
%token SUP INF EXCLM EQ COLON
%token LOCATION OWNER NAME
%token <int> INT
%token <string> STR
%start main
%type <Event_query_ast.expr> main
%%

main:
    expr EOF { $1 }

expr:
   LPAREN expr RPAREN { $2 }
 | one_rule expr { `And (`Single $1, $2)}
 | expr OR expr { `Or ($1, $3) }
 | one_rule { `Single $1 }

one_rule:
   ATTENDING op INT { `Attending ($2, $3)}
 | DECLINED op INT { `Declined ($2, $3)}
 | INVITED op INT { `Invited ($2, $3)}
 | LOCATION eq_op STR { `Location ($2, $3) }
 | OWNER eq_op STR { `Owner ($2, $3) }
 | NAME eq_op STR { `Name ($2, $3) }

op:
   diff_op { `Diffop $1 }
 | eq_op { `Eqop $1 }

diff_op:
   SUP EQ { `Gte }
 | INF EQ { `Lte }
 | SUP { `Gt }
 | INF { `Lt }

eq_op:
   EQ { `Eq }
 | COLON { `Eq }
 | EXCLM EQ { `Neq }
