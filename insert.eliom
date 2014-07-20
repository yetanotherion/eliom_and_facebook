{shared{
  open Eliom_content
  open Html5.D
  open Eliom_parameter
}}
(*647147472010945*)

{client{
  let process_res res url_input span_elt to_insert button_dom =
    match (Utils.process_event_answer url_input res) with
      | `Err x -> begin
        Html5.Manip.replaceChildren span_elt x;
        Lwt.return_unit end
      | `Ok event -> begin
          let attending_s, declined_s, invited_s =
          Utils.make_wait_span (), Utils.make_wait_span (), Utils.make_wait_span ()
        in
        let event_line = tr (Utils.make_event_display_line event attending_s declined_s invited_s) in
        let displayed_table = Utils.make_complete_event_table [event_line] in
        Html5.Manip.replaceChildren span_elt [displayed_table];
        lwt res = Utils.process_all_rsvp url_input in
        let (attending, declined, invited) = res in
        Utils.display_all_rsvp [(attending, attending_s);
                                (declined, declined_s);
                                (invited, invited_s)];
        Utils.show_button button_dom;
        let result = Utils.make_event_and_users event attending declined invited in
        let event_data = result.Utils.ev_data in
        to_insert := Some (url_input, event_data.Fb.venue.Fb.city,
                           event_data.Fb.start_time,
                           event_data.Fb.owner.Fb.name, event_data.Fb.name);
         Lwt.return_unit
        end
}}

let my_insert (url, location, date, owner, name) =
  Db.insert url location (Utils.to_epoch date) owner name

let rpc_insert_url =
  server_function Json.t<(string * string * string * string * string)> my_insert

let insert_service unused unused2 =
  let url_input = string_input ~a:[a_class ["form-control"];
                                   a_autofocus `Autofocus]
    ~input_type:`Text () in
  let text_over_input = h2 ~a:[a_class ["form-signin-heading"]] [pcdata "Enter event's id"] in
  let button = button ~a:[a_class ["hidden"; "btn"; "btn-lg"; "btn-primary"]] ~button_type:`Button [pcdata "Record"] in
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
                   try_lwt begin
                     lwt res = Utils.lwt_api_event url_input in
                     lwt () = process_res res url_input %span_elt to_insert button_dom in
                     Lwt.return_unit
                   end
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
                    | None -> Lwt.return [pcdata "Event inserted"]
                    | Some event ->
                      let inserted_msg = pcdata "Event already inserted" in
                      let tds = Utils.make_tds [pcdata event.Utils.url;
                                                pcdata event.Utils.location;
                                                pcdata (Utils.epoch_to_tz_date event.Utils.start_date)] in
                      let table = Utils.make_table ["url"; "location"; "start_date"] [tr tds] in
                      Lwt.return [inserted_msg; table]
                  with e -> Lwt.return [pcdata (Printf.sprintf "An exception occured with the db: %s" (Printexc.to_string e))]
           in
           Html5.Manip.replaceChildren %span_elt [div ~a:[a_style "text-align:center"] res];
           Utils.hidde_button button_dom;
           to_insert := None;
           Lwt.return ())
      )
    )
  }}
  in
  Lwt.return (Eliom_tools.D.html ~title: "insert an url"
                ~css:[["css"; "bootstrap.min.css"];
                      ["css"; "mb.css"];
                      ["css"; "signin.css"]]
                ~js:[["js"; "jquery.min.js"];
                     ["js"; "bootstrap.min.js"]]
                (body [Utils.fb_root_div;
                       div ~a:[a_class ["container"; "form-signin"];
                               a_style "text-align:center"]
                         [text_over_input; url_input; button;];
                       div ~a:[a_class ["container"]] [span_elt]])
                ~other_head:Utils.bootstrap_metas)
