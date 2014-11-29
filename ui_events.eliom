let get_query arg =
  let queryo, l, o = arg in
  Db.get_events_from_query ~limit:l ~offset:o queryo

let rpc_get_events = server_function Json.t<(string  option) * int * Int32.t> get_query

{shared{
  open Eliom_content
  open Html5.D
  open Eliom_parameter

  let user_type_to_icon_file x =
    match x with
      | `All -> "users.png"
      | `All_events -> "events_all.png"
      | `Attended_ref -> "events_ref_attended.png"
      | `Declined_ref -> "events_ref_declined.png"
      | `Invited_ref -> "events_ref_invited.png"
      | `Not_invited_ref -> "events_ref_not_invited.png"

  let make_users_in_div ?usert:(u=`All) ?userid:(uid=None) ?draggable:(d=true) ?size:(s=16) text =
    let attributes = [a_height s; a_width s;
                      a_draggable d] in
    let attributes = match uid with
      | None -> attributes
      | Some x -> (a_id (Printf.sprintf "%d" x)) :: attributes
    in
    let icon = img ~src:(uri_of_string (fun () -> "imgs/" ^ (user_type_to_icon_file u))) ~alt:"users" ~a:attributes () in
    [icon; pcdata text]

  let make_users_basket_in_div number_of_users =
    make_users_in_div (Printf.sprintf "%d fans" number_of_users)
      ~draggable:false ~size:42
}}

{client{
  open Eliom_content
  open Html5.D

let make_next () =
  span ~a:[a_id "next"; a_class ["nvgt"]] []

let make_prev () =
  span ~a:[a_id "prev"; a_class ["nvgt"]] []


module Events_store = Hashtbl.Make (struct
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end)

type selected_events = [
| `Resolved of Utils.event_and_users
| `Resolving of Utils.event ]

type reference_event = [
| `Undefined
| `Resolving of (Utils.event * Utils.spans)
| `Resolved of (Utils.event_and_users * Utils.spans)
]

type user_set = {
  users: Utils.RsvpSet.t;
  user_id: int;
}

type user_sets = {
  all_users: user_set list;
  next_id: int;
}

let create_user_sets () = {
  all_users = [];
  next_id = 0;
}

let common_users l1 l2 =
  Utils.RsvpSet.inter (Utils.make_rsvp_set l1) (Utils.make_rsvp_set l2)

let get_button_id user_set =
  Printf.sprintf "_userbox-%d" user_set.user_id

let append_new_user_set user_sets users =
  let new_user = {
    users = users;
    user_id = user_sets.next_id;
  } in
  ({
    all_users = user_sets.all_users @ [new_user];
    next_id = user_sets.next_id + 1;
  }, new_user)


let find_user user_sets button_id =
  List.find (fun x -> x.user_id = button_id) user_sets.all_users

let check_event_coherence event =
  let attending = Utils.make_rsvp_set event.Utils.attending in
  let declined = Utils.make_rsvp_set event.Utils.declined in
  let invited = Utils.make_rsvp_set event.Utils.invited in
  let open Utils.RsvpSet in
  Firebug.console##log(Printf.sprintf "attending&declined:%d" (cardinal (inter attending declined)));
  Firebug.console##log(Printf.sprintf "attending&invited:%d" (cardinal (inter attending invited)));
  Firebug.console##log(Printf.sprintf "declined&invited:%d" (cardinal (inter declined invited)))

let make_selectable_event x =
 let res = tr ~a:[a_draggable true] (Utils.print_event x) in
 let res_dom = Html5.To_dom.of_element res in
 let open Lwt_js_events in
 let ondragstarts ev _ =
   let event_id = x.Utils.url in
   ev##dataTransfer##setData((Js.string "event_id"), (Js.string event_id));
   Lwt.return_unit
 in
 Lwt.async (fun () -> dragstarts res_dom ondragstarts);
 res

type 'a ui_events = {
  events_in_db_container: Utils.event Events_store.t;
  selected_events: (selected_events * Utils.spans) Events_store.t;
  resolved_events_cache: Utils.event_and_users Events_store.t;
  nb_event_per_request: int;
  url_input: Dom_html.inputElement Js.t;
  db_selected_events_span: 'a Eliom_content.Html5.elt;
  all_users_div: Dom_html.element Js.t;
  reference_event_span: 'a Eliom_content.Html5.elt;
  reference_event_div: Dom_html.element Js.t;
  reference_event_img: Dom_html.element Js.t;
  selected_events_span: 'a Eliom_content.Html5.elt;
  selected_events_div: Dom_html.element Js.t;
  selected_events_img: Dom_html.element Js.t;
  legend_div: Dom_html.element Js.t;
  mutable ref_event: reference_event;
  mutable user_sets: user_sets;
  mutable curr_offset: Int32.t;
  mutable curr_query: string option;
  mutable buttons_to_move: 'a Eliom_content.Html5.elt list;
  mutable buttons_move: ('a Eliom_content.Html5.elt * Animation.move list list) option;
  mutable all_users_container: Utils.RsvpSet.t;
}

let create
    url_input
    db_selected_events_span
    all_users_div
    reference_event_span reference_event_img reference_event_div
    selected_events_span selected_events_img selected_events_div
    legend_div =
  {
    events_in_db_container = Events_store.create 100;
    selected_events = Events_store.create 100;
    resolved_events_cache = Events_store.create 100;
    nb_event_per_request = 5;
    url_input = Html5.To_dom.of_input url_input;
    db_selected_events_span = db_selected_events_span;
    all_users_div = Html5.To_dom.of_element all_users_div;
    reference_event_span = reference_event_span;
    reference_event_div = Html5.To_dom.of_element reference_event_div;
    reference_event_img = Html5.To_dom.of_element reference_event_img;
    selected_events_span = selected_events_span;
    selected_events_div = Html5.To_dom.of_element selected_events_div;
    selected_events_img = Html5.To_dom.of_element selected_events_img;
    legend_div = Html5.To_dom.of_element legend_div;
    ref_event = `Undefined;
    user_sets = create_user_sets ();
    curr_offset = 0l;
    curr_query = None;
    buttons_to_move = [];
    buttons_move = None;
    all_users_container = Utils.RsvpSet.empty;
  }

let make_user_button t user utype text =
  let button = make_users_in_div ~usert:utype ~userid:(Some user.user_id) text in
  let res = div button in
  let () = match button with
    | hd :: _ -> t.buttons_to_move <- hd :: t.buttons_to_move
    | _ -> ()
  in
  let res_dom = Html5.To_dom.of_element res in
  let open Lwt_js_events in
  let ondragstarts ev _ =
    let user_id = Printf.sprintf "%d" user.user_id in
    ev##dataTransfer##setData((Js.string "button_id"), (Js.string user_id));
    Firebug.console##log("set user_id:" ^ user_id);
    Lwt.return_unit
  in
  Lwt.async (fun () -> dragstarts res_dom ondragstarts);
  res

let display_compared_rsvp t event compared_users compared_span =
  (*let () = check_event_coherence event in*)
  let compared_users_set = Utils.make_rsvp_set compared_users in
  let user_sets, compared_user = append_new_user_set t.user_sets compared_users_set in
  let user_sets, common_attending = append_new_user_set user_sets (common_users event.Utils.attending compared_users) in
  let user_sets, common_declined = append_new_user_set user_sets (common_users event.Utils.declined compared_users) in
  let user_sets, common_invited = append_new_user_set user_sets (common_users event.Utils.invited compared_users) in
  let not_invited_users = Utils.RsvpSet.diff compared_users_set common_invited.users in
  let user_sets, not_invited = append_new_user_set user_sets not_invited_users in
  Html5.Manip.replaceChildren compared_span
    (List.map (fun (u, x, y) -> make_user_button t u x y) [(compared_user, `All_events,
                                                          Printf.sprintf "%d" (List.length compared_users));
                                                         (common_attending, `Attended_ref,
                                                          Printf.sprintf "%d" (Utils.RsvpSet.cardinal common_attending.users));
                                                         (common_declined, `Declined_ref ,
                                                          Printf.sprintf "%d" (Utils.RsvpSet.cardinal common_declined.users));
                                                         (common_invited, `Invited_ref,
                                                          Printf.sprintf "%d" (Utils.RsvpSet.cardinal common_invited.users));
                                                         (not_invited, `Not_invited_ref,
                                                          Printf.sprintf "%d" (Utils.RsvpSet.cardinal not_invited.users))]);
  t.user_sets <- user_sets

let display_rsvp t users span =
  (*let () = check_event_coherence event in*)
  let user_sets, displayed_users = append_new_user_set t.user_sets (Utils.make_rsvp_set users) in
  Html5.Manip.replaceChildren span [make_user_button t displayed_users `All_events
                                       (Printf.sprintf "%d" (List.length users))];
  t.user_sets <- user_sets

let display_resolved_event t resolved_event spans =
  List.iter (fun (u, s) -> display_rsvp t u s) [(resolved_event.Utils.attending, spans.Utils.attending_span);
                                                (resolved_event.Utils.declined, spans.Utils.declined_span);
                                                (resolved_event.Utils.invited, spans.Utils.invited_span)]

let process_event t event spans user_ref =
 try_lwt
  let event_url = event.Utils.url in
  lwt res = Utils.lwt_api_event event_url in
  match (Utils.process_event_answer event.Utils.url res) with
    | `Err x -> begin
      Html5.Manip.replaceChildren spans.Utils.event_name_span x;
      Lwt.return_unit
    end
    | `Ok event -> begin
      (* XXX check whether db value should be updated and update it if so*)
      lwt (attending, declined, invited) = Utils.process_all_rsvp event_url in
      let resolved_event = Utils.make_event_and_users event_url event attending declined invited in
      user_ref := Some resolved_event;
      display_resolved_event t resolved_event spans;
      Lwt.return_unit
    end
  with x -> begin
    Html5.Manip.replaceChildren spans.Utils.event_name_span
      [pcdata (Printf.sprintf "Invalid event %s" (Printexc.to_string x))];
    Lwt.return_unit
  end

let resolve_event t event spans =
 let event_ref = ref None in
 let must x = match x with | None -> assert(false) | Some x -> x in
 lwt () = process_event t event spans event_ref in
 let () = Events_store.remove t.selected_events event.Utils.url in
 let () = Events_store.remove t.resolved_events_cache event.Utils.url in
 let resolved_event = must !event_ref in
 let () = Events_store.add t.selected_events event.Utils.url (`Resolved resolved_event,
                                                              spans) in
 let () = Events_store.add t.resolved_events_cache event.Utils.url resolved_event in
 Lwt.return_unit

let display_event ?ref_event:(r=None) t event span =
  let f arg =
    let u, s = arg in
    match r with
    | None -> display_rsvp t u s
    | Some ref_event -> display_compared_rsvp t ref_event u s
  in
  List.iter f [(event.Utils.attending, span.Utils.attending_span);
               (event.Utils.declined, span.Utils.declined_span);
               (event.Utils.invited, span.Utils.invited_span)]


let refresh_ui t =
 let resolved_events = ref [] in
 let () = Events_store.iter (fun k (event, spans) ->
   match event with
     | `Resolving _ -> ()
     | `Resolved x -> resolved_events:= (x, spans) :: !resolved_events)
   t.selected_events
 in
 let ref_event_resolved =
 match t.ref_event with
   | `Undefined | `Resolving (_, _) -> begin
     List.iter (fun (curr_event, span) ->
       display_event t curr_event span) !resolved_events;
     false
   end
   | `Resolved (ref_event, ref_span) -> begin
     List.iter (fun (curr_event, span) ->
       display_event ~ref_event:(Some ref_event) t curr_event span) !resolved_events;
     display_event t ref_event ref_span;
     true
   end
 in
 Utils.show_element t.all_users_div;
 if List.length !resolved_events > 0 && ref_event_resolved then (
   Utils.show_element t.legend_div)

let make_rpc_get_events_args ?offset:(o=0l) t queryo = (queryo, t.nb_event_per_request, o)

let rec get_and_record_events t queryo =
   let compute_link next_offset link_type =
     let link =
       match link_type with
         | `Next -> make_next ()
         | `Prev -> make_prev ()
     in
     let clickh _ _ =
        t.curr_offset <- next_offset;
        get_and_record_events t t.curr_query
     in
     let open Lwt_js_events in
     async (fun () -> clicks (Html5.To_dom.of_element link) clickh);
     link
   in
   let compute_prev_next_handlers events =
     let events_length = List.length events in
     let links = ref [] in
     let nb_event_per_request = Int32.of_int t.nb_event_per_request in
     let next_offset = Int32.add t.curr_offset nb_event_per_request in
     if t.curr_offset <> 0l then begin
       let prev_offset = Int32.sub t.curr_offset nb_event_per_request in
       links := (compute_link prev_offset `Prev) :: !links
     end;
     (* XXX find a better way to write that if *)
     lwt () = if events_length  == t.nb_event_per_request then (
       lwt events = %rpc_get_events (make_rpc_get_events_args t queryo ~offset:next_offset) in
       if List.length events > 0 then links := !links @ [compute_link next_offset `Next];
       Lwt.return_unit)
              else Lwt.return_unit
     in
     t.curr_offset <- next_offset;
     Lwt.return !links
  in
  let () = Events_store.clear t.events_in_db_container in
  t.curr_query <- queryo;
  lwt events = %rpc_get_events (make_rpc_get_events_args t queryo ~offset:t.curr_offset)  in
  let () = List.iter (fun x -> Events_store.add t.events_in_db_container x.Utils.url x) events in
  let trs = List.map make_selectable_event events in
  let db_table = Utils.make_table ~additional_class:["db_container"] ["Name"; "Owner"; "Location"; "Date"] trs in
  lwt other_links = compute_prev_next_handlers events in
  let new_span = [db_table] @ other_links in
  Html5.Manip.replaceChildren t.db_selected_events_span new_span;
  Lwt.return_unit

let on_db_input_changes t ev _ =
  let queryo =
    match (Js.to_string t.url_input##value) with
      | "" -> None
      | str -> Some str
  in
  t.curr_offset <- 0l;
  get_and_record_events t queryo

let on_user_drop_in_selected_events t ev _ =
  Dom.preventDefault ev;
  Utils.hidde_element t.selected_events_img;
  let data_val = ev##dataTransfer##getData((Js.string "event_id")) in
  let (data_val: string) = Js.to_string data_val in
    (* create the UI elements for new selected elements
       and launch the associated FB requests to compute attending, etc *)
  let to_resolve_lwt = ref None in
  let () = match Events_store.mem t.selected_events data_val with
    | false -> begin
      let event = Events_store.find t.events_in_db_container data_val in
      let spans = Utils.create_spans () in
      let () = Utils.replace_event_spans event spans in
      match Events_store.mem t.resolved_events_cache data_val with
        | false -> let () = to_resolve_lwt:= Some (resolve_event t event spans) in
                   Events_store.add t.selected_events event.Utils.url ((`Resolving event, spans))
        | true -> let resolved_event = Events_store.find t.resolved_events_cache data_val in
                  let () = Events_store.add t.selected_events event.Utils.url ((`Resolved resolved_event, spans)) in
                  display_resolved_event t resolved_event spans

    end
    | true -> () in
    (* display the UI with temporarily non available data *)
  let trs = ref [] in
  let () = Events_store.iter (fun _ (_, s) ->
    trs:= tr (Utils.integrate_spans_in_td s) :: !trs)
    t.selected_events in
  let table = Utils.make_complete_event_table !trs in
  Html5.Manip.replaceChildren t.selected_events_span [table];
    (* wait for the resolution of FB requests *)
  lwt () = match !to_resolve_lwt with
    | None -> Lwt.return_unit
    | Some x -> x
  in
  (* compute differences *)
  let () = refresh_ui t in
  Lwt.return_unit

let on_user_drop_in_ref_event t ev _ =
  Dom.preventDefault ev;
  Utils.hidde_element t.reference_event_img;
  let data_val = ev##dataTransfer##getData((Js.string "event_id")) in
  let (data_val: string) = Js.to_string data_val in
  let to_resolve_lwt = ref None in
  let create_new_ref_event () =
    let event = Events_store.find t.events_in_db_container data_val in
    let spans = Utils.create_spans () in
    let () = Utils.replace_event_spans event spans in
    let resolve_ref_event () =
      let event_ref = ref None in
      let must x = match x with | None -> assert(false) | Some x -> x in
      lwt () = process_event t event spans event_ref in
      let resolved_event = must !event_ref in
      let () = t.ref_event <- `Resolved (resolved_event, spans) in
      let () = Events_store.remove t.resolved_events_cache event.Utils.url in
      let () = Events_store.add t.resolved_events_cache event.Utils.url resolved_event in
      Lwt.return_unit
    in
    match Events_store.mem t.resolved_events_cache data_val with
      | false -> let () = to_resolve_lwt := Some (resolve_ref_event ()) in
                 t.ref_event <- `Resolving (event, spans)
      | true -> let resolved_event = Events_store.find t.resolved_events_cache data_val in
                let () = t.ref_event <- `Resolved (resolved_event, spans) in
                display_resolved_event t resolved_event spans
  in
  let () = match t.ref_event with
    | `Undefined -> create_new_ref_event ()
    | `Resolving (y, _) ->
      if y.Utils.url = data_val then ()
      else create_new_ref_event ()
    | `Resolved (y, _) ->
      if y.Utils.ev_url = data_val then ()
      else create_new_ref_event ()
  in
  (* display the UI with, maybe, temporarily non available data *)
  let trs = match t.ref_event with
    | `Undefined -> assert(false)
    | `Resolved (_, s) | `Resolving (_, s) -> [tr (Utils.integrate_spans_in_td s)]
  in
  let table = Utils.make_complete_event_table trs in
  Html5.Manip.replaceChildren t.reference_event_span [table];
  (* wait for the resolution of FB requests *)
  lwt () = match !to_resolve_lwt with | None -> Lwt.return_unit | Some x -> x in
  (* compute differences *)
  let () = refresh_ui t in
  Lwt.return_unit

let add_users_in_button_id t button_id =
  let corresponding_user = find_user t.user_sets button_id in
  Utils.RsvpSet.union t.all_users_container corresponding_user.users

let update_all_users_basket t new_users =
  let new_basket = make_users_basket_in_div (Utils.RsvpSet.cardinal new_users) in
  Html5.Manip.replaceChildren (Html5.Of_dom.of_element t.all_users_div) new_basket

let update_all_users_basket_from_button_id ?update_all_users:(u=true) t button_id=
  let new_users = add_users_in_button_id t button_id in
  let () = if u then t.all_users_container <- new_users in
  update_all_users_basket t new_users

let on_all_users_div_drop t ev _ =
  Dom.preventDefault ev;
  let data_val = ev##dataTransfer##getData((Js.string "button_id")) in
  let (data_val: string) = Js.to_string data_val in
  let button_id = int_of_string data_val in
  let () = update_all_users_basket_from_button_id t button_id in
  Lwt.return_unit

}}
