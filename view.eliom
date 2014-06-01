{shared{
  open Eliom_content
  open Html5.D
  open Eliom_parameter
}}

let my_get () =
  Db.get_events ()

let rpc_get_events =
  server_function Json.t<unit> my_get

{client{
  let process_event event span_element =
    try_lwt
      lwt res = Utils.lwt_api_event event.Utils.url in
      lwt unused = Utils.process_event_answer event.Utils.url res span_element in
      Lwt.return_unit
    with x -> begin
      Html5.Manip.replaceChildren span_element
        [pcdata (Printf.sprintf "Invalid event %s" (Printexc.to_string x))];
      Lwt.return_unit
    end
}}

let view_service unused unused2 =
  let span_elt = span [] in
  let url_input = string_input ~input_type:`Text () in
  let _ = {unit{
    Lwt_js_events.(
      async (fun () ->
        let url_input_dom = Html5.To_dom.of_element %url_input in
        changes url_input_dom
          (fun _ _ ->
            lwt () = Utils.lwt_installSdk () in
            lwt events = %rpc_get_events () in
            let event_and_span = List.map (fun x -> (x, Dom_html.createSpan Dom_html.window##document)) events in
            let m = Dom_html.createTable Dom_html.window##document in
            List.iter (fun (event, span) ->
              let tr = m##insertRow (-1) in
              let td = tr##insertCell (-1) in
              Dom.appendChild td span;
              Dom.appendChild tr td;
              Dom.appendChild m tr) event_and_span;
            let to_html5 m =
              Html5.Of_dom.of_element (Js.Unsafe.coerce m)
            in
            Html5.Manip.replaceChildren %span_elt [to_html5 m];
            Lwt.join (List.map (fun (event, span) -> process_event event (to_html5 span)) event_and_span)
      )))

  }}
  in

  Lwt.return (Eliom_tools.D.html ~title: "advertise your event"
                ~css:[["css"; "bootstrap.min.css"];
                      ["css"; "mb.css"]]
                ~js:[["js"; "jquery.min.js"];
                     ["js"; "bootstrap.min.js"]]
                (body [Utils.fb_root_div; url_input; span_elt])
                ~other_head:Utils.bootstrap_metas)
