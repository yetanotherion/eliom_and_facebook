open Eliom_content
open Html5.D

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

let get_db : unit -> unit Lwt_PGOCaml.t Lwt.t =
  let db_handler = ref None in
  fun () ->
    match !db_handler with
      | Some h -> Lwt.return h
      | None -> (try_lwt
                   Lwt_PGOCaml.connect ~database:"testapp" ()
                 with _ -> Lwt_PGOCaml.connect ~host:"127.0.0.1"
                                               ~user:"postgres"
                                               ~password:"postgres"
                                               ~port:5432
                                               ~database:"testapp" ())
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

let get_events () =
  lwt dbh = get_db () in
  lwt events = Lwt_Query.query dbh <:select< r | r in $events$ >> in
  Lwt.return (List.map make_event events)

let get_event url =
  lwt dbh = get_db () in
  match_lwt (Lwt_Query.query dbh
             <:select< row | row in $events$; row.url = $string:url$ >>)
  with
    | [] -> Lwt.return None
    | hd :: _ -> Lwt.return (Some (make_event hd))


let do_insert event =
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

let insert event =
  match_lwt (get_event event.Utils.url) with
    | None -> begin
      lwt _ = do_insert event in
      Lwt.return None
      end
    | Some hd -> Lwt.return (Some hd)

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

let get_events_from_query ?limit:(l=10) ?offset:(o=0l) query =
  let lexbuf = Lexing.from_string query in
  let ast = Event_query_parser.main Event_query_lexer.token lexbuf in
  lwt dbh = get_db () in
  let event_view = event_ast_to_view << row | row in $events$ >> ast in
  let limited_view = << row order by row.start_date limit $int32:Int32.of_int l$ offset $int32:o$ | row in $event_view$ >> in
  lwt events = Lwt_Query.view dbh limited_view in
  Lwt.return (List.map make_event events)
