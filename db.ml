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
      | None -> Lwt_PGOCaml.connect ~database:"testapp" ()

let events = <:table< events (
  url text NOT NULL,
  location text NOT NULL,
  start_date integer NOT NULL
) >>

let get_urls location =
  lwt dbh = get_db () in
  Lwt_Query.query dbh <:select< r | r in $events$; r.location = $string:location$ >>

let make_event event_db =
  let open Utils in
      { url = event_db#!url;
        location = event_db#!location;
        start_date = event_db#!start_date;
      }

let get_event url =
  lwt dbh = get_db () in
  match_lwt (Lwt_Query.query dbh
             <:select< row | row in $events$; row.url = $string:url$ >>)
  with
    | [] -> Lwt.return None
    | hd :: _ -> Lwt.return (Some hd)


let do_insert url location start_date =
  lwt dbh = get_db () in
  Lwt_Query.query dbh
   <:insert< $events$ := {url = $string:url$;
                          location = $string:location$;
                          start_date = $int32: start_date$;
                         } >>

let insert url location start_date =
  match_lwt (get_event url) with
    | None -> begin
      lwt _ = do_insert url location start_date in
      Lwt.return None
      end
    | Some hd -> Lwt.return (Some (make_event hd))
