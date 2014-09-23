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

let get_urls location =
  lwt dbh = get_db () in
  Lwt_Query.query dbh <:select< r | r in $events$; r.location = $string:location$ >>

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
