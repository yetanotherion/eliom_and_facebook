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

{server{

let my_insert_event event =
  (* we make the conversion now *)
  Db.insert_event {
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
  server_function Json.t<jsonable_event> my_insert_event

let rpc_event_exists =
  server_function Json.t<string> Db.get_event
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

  let shorten_date date_str =
    let len = String.length date_str in
    let max = if len >= 16 then 16 else len in
    String.sub date_str 0 max

  let make_event_display_line event attending_user_container declined_user_container invited_user_container =
    Utils.make_tds [pcdata event.Fb.name;
                    pcdata event.Fb.owner.Fb.name;
                    pcdata event.Fb.venue.Fb.city;
                    pcdata (shorten_date event.Fb.start_time);
                    div [attending_user_container;
                         declined_user_container;
                         invited_user_container]]

  let display_event_already_inserted event =
    let inserted_msg = pcdata "Event already inserted" in
    let tds = Utils.make_tds [pcdata event.Utils.name;
                              pcdata event.Utils.owner;
                              pcdata event.Utils.location;
                              pcdata (Utils.epoch_to_light_date event.Utils.start_date);
                              div [div [pcdata (Printf.sprintf "attending: %d" event.Utils.nb_attending)];
                                   div [pcdata (Printf.sprintf "declined: %d" event.Utils.nb_declined)];
                                   div [pcdata (Printf.sprintf "invited: %d" event.Utils.nb_invited)]]
                             ] in
    [inserted_msg; Utils.make_insert_event_table [tr tds]]

  type 'a t = {
     url_input: Dom_html.inputElement Js.t;
     button: 'a Eliom_content.Html5.elt;
     user_container: 'a Eliom_content.Html5.elt;
     mutable to_insert: jsonable_event option;
     logged_user_ref: Utils.application_user option ref;
   }

  let create url_input button user_container logged_user_ref =
    {url_input=Html5.To_dom.of_input url_input;
     button=button;
     user_container=user_container;
     to_insert=None;
     logged_user_ref = logged_user_ref}

  let get_input_text t = Js.to_string t.url_input##value

  let is_int_char c = let i = int_of_char c in i >= 48 && i <= 57
  let is_string_int_only s =
    let res = ref true in
    let () = String.iter (fun c -> if not (is_int_char c) then res := false) s in
    !res

  let process_res t res =
   let input_text = get_input_text t in
   match (Utils.process_event_answer input_text res) with
      | `Err x -> begin
        Html5.Manip.replaceChildren t.user_container x;
        Lwt.return_unit end
      | `Ok event -> begin
        let attending_s, declined_s, invited_s =
          Utils.make_wait_user_container ~label:"attending:" (),
          Utils.make_wait_user_container ~label:"declined:" (),
          Utils.make_wait_user_container ~label:"invited:" ()
        in
        let event_line = tr (make_event_display_line event attending_s declined_s invited_s) in
        let displayed_table = Utils.make_insert_event_table [event_line] in
        Html5.Manip.replaceChildren t.user_container [displayed_table];
        lwt res = Utils.process_all_rsvp t.logged_user_ref input_text in
        let (attending, declined, invited) = res in
        Utils.display_all_rsvp [("attending", attending, attending_s);
                                ("declined", declined, declined_s);
                                ("invited", invited, invited_s)];
        Utils.show_element (Html5.To_dom.of_element t.button);
        t.to_insert <- Some (make_jsonable_event input_text event attending declined invited);
        Lwt.return_unit
        end


   let on_url_input_changes t _ _ =
     let button_dom = Html5.To_dom.of_element t.button in
     let () = match !(t.logged_user_ref) with
       | None -> Utils.log "insert: no one"
       | Some x -> Utils.log (Printf.sprintf "insert: logged as %s" (x.Utils.user_id))
     in
     Utils.hidde_element button_dom;
     Html5.Manip.replaceChildren t.user_container [];
     let input_text = get_input_text t in
     if input_text = "" then begin
       let () = Html5.Manip.replaceChildren t.user_container [] in
       Lwt.return_unit
     end
     else if not (is_string_int_only input_text) then begin
       let event_id = "000000000001" in
       let example_str = Printf.sprintf "https://www.facebook.com/events/%s type %s" event_id event_id in
       let () = Html5.Manip.replaceChildren t.user_container [pcdata (Printf.sprintf "Please write event id only. For example to insert %s" example_str)] in
       Lwt.return_unit
     end
     else begin
     try_lwt begin
       match_lwt %rpc_event_exists input_text with
         | None -> begin
           lwt res = Utils.lwt_api_event t.logged_user_ref input_text in
           lwt () = process_res t res in
           Lwt.return_unit
         end
         | Some event -> begin
           let res = display_event_already_inserted event in
           Html5.Manip.replaceChildren t.user_container [div ~a:[a_style "text-align:center"] res];
           Lwt.return_unit
         end
     end
     with x -> (Html5.Manip.replaceChildren t.user_container
                  [div [pcdata (Printf.sprintf "Invalid event %s" (Printexc.to_string x))]];
                Lwt.return ())
 end

   let on_button_clicks t _ _ =
     let arg = match t.to_insert with
              | None -> assert(false)
              | Some x -> x
     in
     lwt res =
         try_lwt
           match_lwt (%rpc_insert_event arg) with
             | None -> Lwt.return []
             | Some event -> Lwt.return (display_event_already_inserted event)
          with e -> Lwt.return [pcdata (Printf.sprintf "An exception occured with the db: %s" (Printexc.to_string e))]
      in
      Html5.Manip.replaceChildren t.user_container [div ~a:[a_style "text-align:center"] res];
      Utils.hidde_element (Html5.To_dom.of_element t.button);
      t.to_insert <- None;
      t.url_input##value <- Js.string "";
      Lwt.return ()

   let setup t =
    let open Lwt_js_events in
    async (fun () ->
           changes t.url_input (on_url_input_changes t));
    async (fun () ->
           clicks (Html5.To_dom.of_element t.button) (on_button_clicks t))
}}
