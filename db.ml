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
open Eliom_content
open Html5.D

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

let connect_to_db () =
  try_lwt
    Lwt_PGOCaml.connect ~database:"testapp" ()
  with _ -> Lwt_PGOCaml.connect ~host:"127.0.0.1"
    ~user:"postgres"
    ~password:"postgres"
    ~port:5432
    ~database:"testapp" ()

let get_db () =
  let db_handler = ref None in
  match !db_handler with
    | Some h -> Lwt.return h
    | None -> begin
      lwt h = connect_to_db () in
      db_handler:= Some h;
      Lwt.return h
      end

let events = <:table< events (
  url text NOT NULL,
  location text NOT NULL,
  start_date integer NOT NULL,
  owner text NOT NULL,
  name text NOT NULL,
  nb_attending integer NOT NULL,
  nb_declined integer NOT NULL,
  nb_invited integer NOT NULL
) >>

let users = <:table< users (
  id text NOT NULL,
  name text NOT NULL,
  nb_event_added integer NOT NULL
) >>

let make_event event_db =
  let open Utils in
      { url = event_db#!url;
        location = event_db#!location;
        start_date = event_db#!start_date;
        owner = event_db#!owner;
        name = event_db#!name;
        nb_attending = Int32.to_int event_db#!nb_attending;
        nb_declined = Int32.to_int event_db#!nb_declined;
        nb_invited = Int32.to_int event_db#!nb_invited;
      }

let make_user user_db =
  {Server_state.user_id = user_db#!id;
   Server_state.user_name = user_db#!name;
   Server_state.nb_event_added = Int32.to_int user_db#!nb_event_added}

let get_event url =
  lwt dbh = get_db () in
  match_lwt (Lwt_Query.query dbh
             <:select< row | row in $events$; row.url = $string:url$ >>)
  with
    | [] -> Lwt.return None
    | hd :: _ -> Lwt.return (Some (make_event hd))


let get_user userid =
  lwt dbh = get_db () in
  match_lwt (Lwt_Query.query dbh
             <:select< row | row in $users$; row.id = $string:userid$ >>)
  with
    | [] -> Lwt.return None
    | hd :: _ -> Lwt.return (Some (make_user hd))

let do_insert_event event =
  let open Utils in
  lwt dbh = get_db () in
  Lwt_Query.query dbh
   <:insert< $events$ := {url = $string:event.url$;
                          location = $string:event.location$;
                          start_date = $int32:event.start_date$;
                          owner = $string:event.owner$;
                          name = $string:event.name$;
                          nb_attending = $int32:Int32.of_int event.nb_attending$;
                          nb_declined = $int32:Int32.of_int event.nb_declined$;
                          nb_invited = $int32:Int32.of_int event.nb_invited$
                         } >>
let do_insert_user user =
  let open Server_state in
  lwt dbh = get_db () in
  Lwt_Query.query dbh
   <:insert< $users$ := {id = $string:user.user_id$;
                         name = $string:user.user_name$;
                         nb_event_added = $int32:Int32.of_int user.nb_event_added$;
                        } >>

let insert_event event =
  match_lwt (get_event event.Utils.url) with
    | None -> begin
      lwt _ = do_insert_event event in
      Lwt.return None
      end
    | Some hd -> Lwt.return (Some hd)

let insert_user user =
  match_lwt (get_user user.Server_state.user_id) with
    | None -> begin
      lwt _ = do_insert_user user in
      Lwt.return_unit
      end
    | Some _ -> Lwt.return_unit

let update_user_nb_event userid nb_event =
  let open Server_state in
  lwt dbh = get_db () in
  Lwt_Query.query dbh
   <:update< t in $users$ := {nb_event_added = $int32:Int32.of_int nb_event$}
             |  t.id = $string:userid$ >>

let parse_query str =
   let lexbuf = Lexing.from_string str in
   Event_query_parser.main Event_query_lexer.token lexbuf

let single_expr_view sexpr table =
  match sexpr with
    | `Attending (op, int) -> begin
      match op with
        | `Eqop `Eq -> << row | row in $table$; row.nb_attending = $int32:Int32.of_int int$ >>
        | `Eqop `Neq -> << row | row in $table$; row.nb_attending <> $int32:Int32.of_int int$ >>
        | `Diffop `Gte -> << row | row in $table$; row.nb_attending >= $int32:Int32.of_int int$ >>
        | `Diffop `Lte -> << row | row in $table$; row.nb_attending <= $int32:Int32.of_int int$ >>
        | `Diffop `Gt -> << row | row in $table$; row.nb_attending > $int32:Int32.of_int int$ >>
        | `Diffop `Lt -> << row | row in $table$; row.nb_attending < $int32:Int32.of_int int$ >>
      end
   | `Declined (op, int) -> begin
      match op with
        | `Eqop `Eq -> << row | row in $table$; row.nb_declined = $int32:Int32.of_int int$ >>
        | `Eqop `Neq -> << row | row in $table$; row.nb_declined <> $int32:Int32.of_int int$ >>
        | `Diffop `Gte -> << row | row in $table$; row.nb_declined >= $int32:Int32.of_int int$ >>
        | `Diffop `Lte -> << row | row in $table$; row.nb_declined <= $int32:Int32.of_int int$ >>
        | `Diffop `Gt -> << row | row in $table$; row.nb_declined > $int32:Int32.of_int int$ >>
        | `Diffop `Lt -> << row | row in $table$; row.nb_declined < $int32:Int32.of_int int$ >>
      end
   | `Invited (op, int) -> begin
      match op with
        | `Eqop `Eq -> << row | row in $table$; row.nb_invited = $int32:Int32.of_int int$ >>
        | `Eqop `Neq -> << row | row in $table$; row.nb_invited <> $int32:Int32.of_int int$ >>
        | `Diffop `Gte -> << row | row in $table$; row.nb_invited >= $int32:Int32.of_int int$ >>
        | `Diffop `Lte -> << row | row in $table$; row.nb_invited <= $int32:Int32.of_int int$ >>
        | `Diffop `Gt -> << row | row in $table$; row.nb_invited > $int32:Int32.of_int int$ >>
        | `Diffop `Lt -> << row | row in $table$; row.nb_invited < $int32:Int32.of_int int$ >>
      end
   | `Location (eqop, string) -> begin
      match eqop with
        | `Eq -> << row | row in $table$; row.location = $string:string$ >>
        | `Neq -> << row | row in $table$; row.location <> $string:string$ >>
      end
   | `Owner (eqop, string) -> begin
      match eqop with
        | `Eq -> << row | row in $table$; row.owner = $string:string$ >>
        | `Neq -> << row | row in $table$; row.owner <> $string:string$ >>
      end
   | `Name (eqop, string) -> begin
      match eqop with
        | `Eq -> << row | row in $table$; row.name = $string:string$ >>
        | `Neq -> << row | row in $table$; row.name <> $string:string$ >>
      end


let rec event_ast_to_view table ast =
  match ast with
    | `Or (expr1, expr2) -> << union $event_ast_to_view table expr1$ $event_ast_to_view table expr2$ >>
    | `And (expr1, expr2) -> let table2 = <<row | row in $event_ast_to_view table expr1$ >> in
                              << row | row in $event_ast_to_view table2 expr2$ >>
    | `Single (sexpr) ->  single_expr_view sexpr table

let get_events_from_query ?limit:(l=10) ?offset:(o=0l) queryo =
  lwt dbh = get_db () in
  lwt events = match queryo with
    | Some query -> begin
      let lexbuf = Lexing.from_string query in
      let ast = Event_query_parser.main Event_query_lexer.token lexbuf in
      let event_view = event_ast_to_view << row | row in $events$ >> ast in
      let limited_view = << row order by row.start_date desc limit $int32:Int32.of_int l$ offset $int32:o$ | row in $event_view$ >> in
      Lwt_Query.view dbh limited_view
      end
   | None -> begin
      let limited_select = <:select< row order by row.start_date desc limit $int32:Int32.of_int l$ offset $int32:o$ | row in $events$ >> in
      Lwt_Query.query dbh limited_select
      end
  in
  Lwt.return (List.map make_event events)
