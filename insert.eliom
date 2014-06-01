{shared{
  open Eliom_content
  open Html5.D
  open Eliom_parameter
}}

let my_insert (url, location, date) =
  Db.insert url location (Utils.to_epoch date)

let rpc_insert_url =
  server_function Json.t<(string * string * string)> my_insert

let insert_service unused unused2 =
  let url_input = string_input ~input_type:`Text () in
  let button = button ~a:[a_class ["hidden"]] ~button_type:`Button [pcdata "record_in_db"] in
  let span_elt = span [] in
  let _ = {unit{
    let button_dom = Html5.To_dom.of_element %button in
    let to_insert = ref None in
    Lwt_js_events.(
      async (fun () ->
        let url_input_dom = Html5.To_dom.of_element %url_input in
        ignore(changes url_input_dom
                 (fun _ _ ->
                   Utils.hidde_button button_dom;
                   Html5.Manip.replaceChildren %span_elt [];
                   let url_input = Js.to_string (Js.Unsafe.coerce url_input_dom)##value in
                   try_lwt
                     lwt res = Utils.lwt_api_event url_input in
                     Utils.process_event_answer url_input res %span_elt ~button:(Some button_dom) ~to_insert:(Some to_insert) (*647147472010945*)
                   with x -> (Html5.Manip.replaceChildren %span_elt
                                [div [pcdata (Printf.sprintf "Invalid event %s" (Printexc.to_string x))]];
                              Lwt.return ())
        ));
        clicks button_dom
          (fun _ _ ->
            let arg = match !to_insert with
              | None -> assert(false)
              | Some x -> x
            in
            lwt res =
                try_lwt
                  match_lwt (%rpc_insert_url arg) with
                    | None -> Lwt.return ("Event inserted")
                    | Some event ->
                      Lwt.return (Printf.sprintf "Event already inserted %s" (Utils.event_to_string event))
                  with e -> Lwt.return (Printf.sprintf "An exception occured with the db: %s" (Printexc.to_string e))
           in
          Html5.Manip.replaceChildren
%span_elt
            [div [pcdata res]];
          Utils.hidde_button button_dom;
          to_insert := None;
          Lwt.return ())
      )
    )
  }}
  in
  Lwt.return (Eliom_tools.D.html ~title: "insert an url"
                ~css:[["css"; "bootstrap.min.css"];
                      ["css"; "mb.css"]]
                ~js:[["js"; "jquery.min.js"];
                     ["js"; "bootstrap.min.js"]]
                (body [Utils.fb_root_div; url_input; button; span_elt]))
