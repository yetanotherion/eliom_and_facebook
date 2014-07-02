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
  let process_event event spans =
    try_lwt
      let event_url = event.Utils.url in
      lwt res = Utils.lwt_api_event event_url in
      match (Utils.process_event_answer event.Utils.url res) with
        | `Err x -> begin
          Html5.Manip.replaceChildren spans.Utils.event_name_span x;
          Lwt.return_unit
        end
        | `Ok event -> begin
          Utils.replace_event_spans event spans;
          lwt (attending, declined, invited) = Utils.process_all_rsvp event_url in
           Utils.display_all_rsvp [(attending, spans.Utils.attending_span);
                                   (declined, spans.Utils.declined_span);
                                   (invited, spans.Utils.invited_span)];
          Lwt.return_unit
          end
    with x -> begin
      Html5.Manip.replaceChildren spans.Utils.event_name_span
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
            let event_and_span = List.map (fun x -> (x, Utils.create_spans ())) events in
            let trs = List.map (fun (event, s) -> tr (Utils.integrate_spans_in_td s)) event_and_span in
            let table = Utils.make_complete_event_table trs in
            Html5.Manip.replaceChildren %span_elt [table];
            Lwt.join (List.map (fun (event, span) -> process_event event span) event_and_span)
      )))

  }}
  in

  Lwt.return (Eliom_tools.D.html ~title: "advertise your event"
                ~css:[["css"; "bootstrap.min.css"];
                      ["css"; "mb.css"];
                      ["css"; "signin.css"]]
                ~js:[["js"; "jquery.min.js"];
                     ["js"; "bootstrap.min.js"]]
                (body [Utils.fb_root_div;
                       div ~a:[a_class ["container"; "form-signin"];
                               a_style "text-align:center"]
                         [url_input];
                       div ~a:[a_class ["container"]] [span_elt]])
                ~other_head:Utils.bootstrap_metas)
