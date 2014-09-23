{
  open Event_query_parser
}

rule token = parse
           [' ' '\t']     { token lexbuf }     (* skip blanks *)
          | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
          | "OR" | "or"    { OR }
          | '('            { LPAREN }
          | ')'            { RPAREN }
          | '='            { EQ }
          | '>'            { SUP }
          | '<'            { INF }
          | '!'            { EXCLM }
          | ':'            { COLON }
          | "nb_attending" {ATTENDING}
          | "nb_declined" {DECLINED}
          | "nb_invited" {INVITED}
          | "location" {LOCATION}
          | "name" {NAME}
          | "owner" {OWNER}
          | ['a'-'z''A'-'Z']+ as str { STR(str) }
          | eof            { EOF }
