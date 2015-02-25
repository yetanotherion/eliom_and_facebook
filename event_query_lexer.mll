(**************************************************************************)
(*  Copyright 2014, Ion Alberdi <nolaridebi at gmail.com>                 *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)

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
