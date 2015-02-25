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
(* XXX: find a more conveniant way than:
   ocaml -I ../_server test_event_query.ml
   to run it *)
#load "../_server/event_query_parser.cmo";;
#load "../_server/event_query_lexer.cmo";;
#load "../_server/event_query_ast.cmo";;

let parse_query str =
  let lexbuf = Lexing.from_string str in
  Event_query_parser.main Event_query_lexer.token lexbuf

let assert_ast_eq x y =
  let open Event_query_ast in
  if Pervasives.compare x y != 0 then
    raise (Failure (Printf.sprintf "%s != %s" (expr_to_string x) (expr_to_string y)));;

let assert_ast query expected_ast =
  try
    assert_ast_eq (parse_query query) expected_ast
  with _ ->
    raise (Failure (Printf.sprintf "query: %s could not be parsed" query))

let test_ast () =
  let () = assert_ast "location = Toulouse" (`Single (`Location (`Eq, "Toulouse"))) in
  let () = assert_ast "location: Toulouse" (`Single (`Location (`Eq, "Toulouse"))) in
  let () = assert_ast "location=Toulouse owner!=Ulmet" (`And (`Single (`Location (`Eq, "Toulouse")),
                                                              `Single (`Owner (`Neq, "Ulmet")))) in
  let () = assert_ast "(location:Toulouse owner:Ulmet) or nb_attending >= 2" (`Or (`And (`Single (`Location (`Eq, "Toulouse")),
                                                                                         (`Single (`Owner (`Eq, "Ulmet")))),
                                                                                   `Single (`Attending (`Diffop `Gte, 2)))) in
  (* I don't manage to make the one above equivalent to the one below yet *)
  assert_ast "location:Toulouse owner:Ulmet or nb_attending >= 2" (`And (`Single (`Location (`Eq, "Toulouse")),
                                                                         `Or (`Single (`Owner (`Eq, "Ulmet")),
                                                                              `Single (`Attending (`Diffop `Gte, 2)))));;
  assert_ast "owner:\"Raphael Ulmet\"" (`Single (`Owner (`Eq, "Raphael Ulmet")));;
  assert_ast "owner:' Raphael Ulmet '" (`Single (`Owner (`Eq, " Raphael Ulmet ")));;
  (* XXX deactivated for now *)
  assert_ast "owner:' Raphaël Ulmet '" (`Single (`Owner (`Eq, " Raphaël Ulmet ")));;

let () = test_ast ();;
