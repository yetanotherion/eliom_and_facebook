{
  open Event_query_parser
}

rule token = parse
          | "OR" | "or"    { OR }
          | '('            { LPAREN }
          | ')'            { RPAREN }
          | '='            { EQ }
          | '>'            { SUP }
          | '<'            { INF }
          | '!'            { EXCLM }
          | ':'            { COLON }
          | '"'            { DQUOTE }
          | '\''           { SQUOTE }
          | [' ']+ as ws   { WHITESPACES(ws) }
          | "nb_attending" { ATTENDING }
          | "nb_declined"  { DECLINED }
          | "nb_invited"   { INVITED }
          | "location"     { LOCATION }
          | "name"         { NAME }
          | "owner"        { OWNER }
          | ['\x00'-'\x1F''#'-'&''*'-'9'';''?'-'\xff']+ as str { STR(str) }
          (* XXX: add rule to catch non ascii bytes *)
          | eof            { EOF }
