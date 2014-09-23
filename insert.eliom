{shared{
  open Eliom_content
  open Html5.D
  open Eliom_parameter
  type jsonable_event = {
    url: string;
    location: string;
    start_time: string;
    owner: string;
    name: string;
    nb_attending: int;
    nb_declined: int;
    nb_invited: int;
  }
  deriving(Json)

}}

{client{

  let make_jsonable_event url fb_event attending declined invited = {
    url = url;
    location = fb_event.Fb.venue.Fb.city;
    (* we can only set it in the server
       as date conversion methods are not available in
       js_of_eliom. we carry that parameter until we reach
       the server, and make the conversion there *)
    start_time = fb_event.Fb.start_time;
    owner = fb_event.Fb.owner.Fb.name;
    name = fb_event.Fb.name;
    nb_attending = List.length attending;
    nb_declined = List.length declined;
    nb_invited = List.length invited;
  }

  let make_event_display_line event attending_span declined_span invited_span =
    Utils.make_tds [pcdata event.Fb.name;
                    pcdata event.Fb.owner.Fb.name;
                    pcdata event.Fb.venue.Fb.city;
                    pcdata event.Fb.start_time;
                    attending_span;
                    declined_span;
                    invited_span]

  let display_event_already_inserted event =
    let inserted_msg = pcdata "Event already inserted" in
    let tds = Utils.make_tds [pcdata event.Utils.name;
                              pcdata event.Utils.owner;
                              pcdata event.Utils.location;
                              pcdata (Utils.epoch_to_light_date event.Utils.start_date);
                              pcdata (string_of_int event.Utils.nb_attending);
                              pcdata (string_of_int event.Utils.nb_declined);
                              pcdata (string_of_int event.Utils.nb_invited)
                             ] in
    [inserted_msg; Utils.make_complete_event_table [tr tds]]


  let process_res res url_input span_elt to_insert button_dom =
    match (Utils.process_event_answer url_input res) with
      | `Err x -> begin
        Html5.Manip.replaceChildren span_elt x;
        Lwt.return_unit end
      | `Ok event -> begin
        let attending_s, declined_s, invited_s =
         Utils.make_wait_span (), Utils.make_wait_span (), Utils.make_wait_span ()
        in
        let event_line = tr (make_event_display_line event attending_s declined_s invited_s) in
        let displayed_table = Utils.make_complete_event_table [event_line] in
        Html5.Manip.replaceChildren span_elt [displayed_table];
        lwt res = Utils.process_all_rsvp url_input in
        let (attending, declined, invited) = res in
        Utils.display_all_rsvp [(attending, attending_s);
                                (declined, declined_s);
                                (invited, invited_s)];
        Utils.show_button button_dom;
        to_insert := Some (make_jsonable_event url_input event attending declined invited);
        Lwt.return_unit
        end
}}

let my_insert event =
  (* we make the conversion now *)
  Db.insert {
    Utils.url = event.url;
    Utils.location = event.location;
    Utils.start_date = Utils.to_epoch event.start_time;
    Utils.owner = event.owner;
    Utils.name = event.name;
    Utils.nb_attending = event.nb_attending;
    Utils.nb_declined = event.nb_declined;
    Utils.nb_invited = event.nb_invited;
  }

let rpc_insert_event =
  server_function Json.t<jsonable_event> my_insert

let rpc_event_exists =
  server_function Json.t<string> Db.get_event


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
                     match_lwt %rpc_event_exists url_input with
                       | None -> begin
                         lwt res = Utils.lwt_api_event url_input in
                         lwt () = process_res res url_input %span_elt to_insert button_dom in
                         Lwt.return_unit
                       end
                       | Some event -> begin
                         let res = display_event_already_inserted event in
                         Html5.Manip.replaceChildren %span_elt [div ~a:[a_style "text-align:center"] res];
                         Lwt.return_unit
                       end
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
                  match_lwt (%rpc_insert_event arg) with
                    | None -> Lwt.return [pcdata "Event inserted"]
                    | Some event -> Lwt.return (display_event_already_inserted event)
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
