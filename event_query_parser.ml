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

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* AND *);
  260 (* OR *);
    0 (* EOF *);
  263 (* ATTENDING *);
  264 (* DECLINED *);
  265 (* INVITED *);
  266 (* SUP *);
  267 (* INF *);
  268 (* EXCLM *);
  269 (* EQ *);
  270 (* COLON *);
  271 (* DQUOTE *);
  272 (* SQUOTE *);
  273 (* LOCATION *);
  274 (* OWNER *);
  275 (* NAME *);
    0|]

let yytransl_block = [|
  261 (* STR *);
  262 (* WHITESPACES *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\003\000\002\000\002\000\002\000\
\002\000\004\000\004\000\004\000\004\000\004\000\004\000\006\000\
\008\000\008\000\008\000\009\000\009\000\010\000\010\000\005\000\
\005\000\011\000\011\000\011\000\011\000\012\000\012\000\012\000\
\012\000\007\000\007\000\007\000\007\000\013\000\013\000\013\000\
\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\005\000\001\000\002\000\002\000\003\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\001\000\
\001\000\003\000\003\000\002\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000\002\000\003\000\002\000\002\000\001\000\
\001\000\001\000\002\000\002\000\003\000\001\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\041\000\000\000\000\000\005\000\000\000\000\000\
\000\000\000\000\000\000\000\000\038\000\039\000\000\000\025\000\
\024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\002\000\000\000\000\000\000\000\030\000\
\031\000\040\000\016\000\010\000\027\000\035\000\011\000\012\000\
\017\000\000\000\000\000\013\000\014\000\015\000\000\000\000\000\
\029\000\037\000\023\000\022\000\000\000\000\000\000\000\000\000\
\000\000\018\000\020\000\019\000\000\000"

let yydgoto = "\002\000\
\011\000\012\000\056\000\014\000\023\000\044\000\024\000\052\000\
\061\000\062\000\025\000\026\000\027\000"

let yysindex = "\255\255\
\006\255\000\000\006\255\029\255\047\255\047\255\047\255\050\255\
\050\255\050\255\000\000\016\000\015\255\000\000\033\255\039\255\
\055\255\027\255\057\255\075\255\000\000\000\000\084\255\000\000\
\000\000\085\255\086\255\084\255\084\255\060\255\012\255\012\255\
\012\255\000\000\025\255\000\000\025\255\087\255\088\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\255\013\255\000\000\000\000\000\000\089\255\090\255\
\000\000\000\000\000\000\000\000\082\255\013\255\074\255\029\255\
\025\255\000\000\000\000\000\000\090\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\002\000\
\000\000\049\255\073\255\000\000\000\000\000\000\000\000\000\000\
\000\000\093\255\034\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\000\000\004\000\094\255\036\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\065\255\000\000\000\000\
\000\000\000\000\000\000\000\000\006\000"

let yygindex = "\000\000\
\000\000\097\000\007\000\000\000\076\000\056\000\067\000\054\000\
\225\255\000\000\000\000\084\000\248\255"

let yytablesize = 264
let yytable = "\001\000\
\009\000\006\000\007\000\008\000\003\000\004\000\003\000\013\000\
\039\000\013\000\016\000\004\000\005\000\006\000\007\000\034\000\
\049\000\059\000\060\000\063\000\035\000\039\000\008\000\009\000\
\010\000\003\000\050\000\051\000\055\000\003\000\067\000\005\000\
\006\000\007\000\036\000\005\000\006\000\007\000\034\000\040\000\
\036\000\008\000\009\000\010\000\037\000\008\000\009\000\010\000\
\034\000\034\000\036\000\036\000\017\000\032\000\032\000\030\000\
\018\000\019\000\020\000\021\000\022\000\020\000\021\000\022\000\
\018\000\019\000\020\000\021\000\022\000\041\000\069\000\020\000\
\021\000\022\000\031\000\032\000\033\000\033\000\033\000\021\000\
\021\000\028\000\029\000\047\000\048\000\053\000\054\000\042\000\
\043\000\068\000\045\000\046\000\057\000\058\000\064\000\065\000\
\066\000\026\000\028\000\015\000\038\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\000\006\000\007\000\008\000\003\000\004\000"

let yycheck = "\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\001\001\000\
\017\000\003\000\004\000\006\001\007\001\008\001\009\001\000\000\
\005\001\005\001\006\001\051\000\006\001\030\000\017\001\018\001\
\019\001\001\001\015\001\016\001\004\001\001\001\062\000\007\001\
\008\001\009\001\002\001\007\001\008\001\009\001\005\001\013\001\
\005\001\017\001\018\001\019\001\006\001\017\001\018\001\019\001\
\015\001\016\001\015\001\016\001\006\001\005\001\006\001\006\001\
\010\001\011\001\012\001\013\001\014\001\012\001\013\001\014\001\
\010\001\011\001\012\001\013\001\014\001\013\001\064\000\012\001\
\013\001\014\001\008\000\009\000\010\000\005\001\006\001\015\001\
\016\001\006\000\007\000\028\000\029\000\032\000\033\000\013\001\
\005\001\016\001\006\001\006\001\006\001\006\001\006\001\006\001\
\015\001\005\001\005\001\003\000\017\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\002\001\002\001\002\001\002\001\002\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  AND\000\
  OR\000\
  EOF\000\
  ATTENDING\000\
  DECLINED\000\
  INVITED\000\
  SUP\000\
  INF\000\
  EXCLM\000\
  EQ\000\
  COLON\000\
  DQUOTE\000\
  SQUOTE\000\
  LOCATION\000\
  OWNER\000\
  NAME\000\
  "

let yynames_block = "\
  STR\000\
  WHITESPACES\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stripped_expr) in
    Obj.repr(
# 29 "event_query_parser.mly"
                      ( _1 )
# 213 "event_query_parser.ml"
               : Event_query_ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stripped_expr) in
    Obj.repr(
# 32 "event_query_parser.mly"
                               ( _2 )
# 220 "event_query_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "event_query_parser.mly"
                         ( `And (_1, _3))
# 229 "event_query_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "event_query_parser.mly"
                                        ( `Or (_1, _5) )
# 239 "event_query_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'one_rule) in
    Obj.repr(
# 35 "event_query_parser.mly"
            ( `Single _1 )
# 246 "event_query_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 38 "event_query_parser.mly"
                    ( _2 )
# 254 "event_query_parser.ml"
               : 'stripped_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "event_query_parser.mly"
                    ( _1 )
# 262 "event_query_parser.ml"
               : 'stripped_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 40 "event_query_parser.mly"
                                ( _2 )
# 271 "event_query_parser.ml"
               : 'stripped_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "event_query_parser.mly"
        ( _1 )
# 278 "event_query_parser.ml"
               : 'stripped_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int) in
    Obj.repr(
# 44 "event_query_parser.mly"
                    ( `Attending (_2, _3))
# 286 "event_query_parser.ml"
               : 'one_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int) in
    Obj.repr(
# 45 "event_query_parser.mly"
                   ( `Declined (_2, _3))
# 294 "event_query_parser.ml"
               : 'one_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int) in
    Obj.repr(
# 46 "event_query_parser.mly"
                  ( `Invited (_2, _3))
# 302 "event_query_parser.ml"
               : 'one_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optional_ws_eqop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str) in
    Obj.repr(
# 47 "event_query_parser.mly"
                                 ( `Location (_2, _3) )
# 310 "event_query_parser.ml"
               : 'one_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optional_ws_eqop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str) in
    Obj.repr(
# 48 "event_query_parser.mly"
                              ( `Owner (_2, _3) )
# 318 "event_query_parser.ml"
               : 'one_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optional_ws_eqop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str) in
    Obj.repr(
# 49 "event_query_parser.mly"
                             ( `Name (_2, _3) )
# 326 "event_query_parser.ml"
               : 'one_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "event_query_parser.mly"
       ( int_of_string(_1) )
# 333 "event_query_parser.ml"
               : 'int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "event_query_parser.mly"
       ( _1 )
# 340 "event_query_parser.ml"
               : 'str))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'in_quote) in
    Obj.repr(
# 56 "event_query_parser.mly"
                          ( _2 )
# 347 "event_query_parser.ml"
               : 'str))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'in_quote) in
    Obj.repr(
# 57 "event_query_parser.mly"
                          ( _2 )
# 354 "event_query_parser.ml"
               : 'str))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'string_or_ws) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'in_quote) in
    Obj.repr(
# 60 "event_query_parser.mly"
                         ( _1 ^ _2 )
# 362 "event_query_parser.ml"
               : 'in_quote))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'string_or_ws) in
    Obj.repr(
# 61 "event_query_parser.mly"
                ( _1 )
# 369 "event_query_parser.ml"
               : 'in_quote))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "event_query_parser.mly"
               ( _1 )
# 376 "event_query_parser.ml"
               : 'string_or_ws))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "event_query_parser.mly"
       ( _1 )
# 383 "event_query_parser.ml"
               : 'string_or_ws))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'optional_ws_diffop) in
    Obj.repr(
# 68 "event_query_parser.mly"
                      ( `Diffop _1 )
# 390 "event_query_parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'optional_ws_eqop) in
    Obj.repr(
# 69 "event_query_parser.mly"
                    ( `Eqop _1 )
# 397 "event_query_parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'diff_op) in
    Obj.repr(
# 72 "event_query_parser.mly"
           ( _1 )
# 404 "event_query_parser.ml"
               : 'optional_ws_diffop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'diff_op) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "event_query_parser.mly"
                       ( _1 )
# 412 "event_query_parser.ml"
               : 'optional_ws_diffop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'diff_op) in
    Obj.repr(
# 74 "event_query_parser.mly"
                       ( _2 )
# 420 "event_query_parser.ml"
               : 'optional_ws_diffop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'diff_op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "event_query_parser.mly"
                                   ( _2 )
# 429 "event_query_parser.ml"
               : 'optional_ws_diffop))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "event_query_parser.mly"
          ( `Gte )
# 435 "event_query_parser.ml"
               : 'diff_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "event_query_parser.mly"
          ( `Lte )
# 441 "event_query_parser.ml"
               : 'diff_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "event_query_parser.mly"
       ( `Gt )
# 447 "event_query_parser.ml"
               : 'diff_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "event_query_parser.mly"
       ( `Lt )
# 453 "event_query_parser.ml"
               : 'diff_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eq_op) in
    Obj.repr(
# 84 "event_query_parser.mly"
         ( _1 )
# 460 "event_query_parser.ml"
               : 'optional_ws_eqop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'eq_op) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "event_query_parser.mly"
                     ( _1 )
# 468 "event_query_parser.ml"
               : 'optional_ws_eqop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'eq_op) in
    Obj.repr(
# 86 "event_query_parser.mly"
                     ( _2 )
# 476 "event_query_parser.ml"
               : 'optional_ws_eqop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'eq_op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "event_query_parser.mly"
                                 ( _2 )
# 485 "event_query_parser.ml"
               : 'optional_ws_eqop))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "event_query_parser.mly"
      ( `Eq )
# 491 "event_query_parser.ml"
               : 'eq_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "event_query_parser.mly"
         ( `Eq )
# 497 "event_query_parser.ml"
               : 'eq_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "event_query_parser.mly"
            ( `Neq )
# 503 "event_query_parser.ml"
               : 'eq_op))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Event_query_ast.expr)
