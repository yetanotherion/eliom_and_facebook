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
  let process_event event spans user_ref =
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
          user_ref := Some (Utils.make_event_and_users event attending declined invited);
          Lwt.return_unit
          end
    with x -> begin
      Html5.Manip.replaceChildren spans.Utils.event_name_span
        [pcdata (Printf.sprintf "Invalid event %s" (Printexc.to_string x))];
      Lwt.return_unit
    end

 let nb_in_comon l1 l2 =
   Utils.RsvpSet.cardinal (Utils.RsvpSet.inter (Utils.make_rsvp_set l1) (Utils.make_rsvp_set l2))

 let compare_rsvp event compared_users compared_span =
   let common_attending = nb_in_comon event.Utils.attending compared_users in
   let common_declined = nb_in_comon event.Utils.declined compared_users in
   let common_invited = nb_in_comon event.Utils.invited compared_users in
   Html5.Manip.replaceChildren compared_span
     [pcdata (Printf.sprintf "%d, ref_attending:%d, ref_declined, %d, ref_invited %d" (List.length compared_users) common_attending common_declined common_invited)]

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
            let event_and_span = List.map (fun x -> (x, Utils.create_spans (), ref None)) events in
            let trs = List.map (fun (event, s, _) -> tr (Utils.integrate_spans_in_td s)) event_and_span in
            let table = Utils.make_complete_event_table trs in
            Html5.Manip.replaceChildren %span_elt [table];
            lwt () = Lwt.join (List.map (fun (event, span, user_ref) -> process_event event span user_ref) event_and_span) in
            match event_and_span with
              | [] -> Lwt.return_unit
              | hd :: tl ->
                let (_, spans, event) = hd in
                let must ev = match !ev with | None -> assert(false) | Some x -> x in
                let event_ref = must event in
                List.iter (fun (_, s, other_event) ->
                  let curr_event = must other_event in
                  List.iter (fun (u, s) -> compare_rsvp event_ref u s)
                    [(curr_event.Utils.attending, s.Utils.attending_span);
                     (curr_event.Utils.declined, s.Utils.declined_span);
                     (curr_event.Utils.invited, s.Utils.invited_span)])
                  tl;
                Lwt.return_unit
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
