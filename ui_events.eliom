let get_query arg =
  let queryo, l, o = arg in
  Db.get_events_from_query ~limit:l ~offset:o queryo

let rpc_get_events = server_function Json.t<(string  option) * int * Int32.t> get_query

{shared{
  open Eliom_content
  open Html5.D
  open Eliom_parameter

  let flatten l = List.fold_left List.append [] l

  let user_type_to_icon_file x =
    match x with
      | `All -> "users.png"
      | `All_events -> "events_all.png"
      | `Attended_ref -> "events_ref_attended.png"
      | `Declined_ref -> "events_ref_declined.png"
      | `Invited_ref -> "events_ref_invited.png"
      | `Not_invited_ref -> "events_ref_not_invited.png"

  let create_img_and_hidden_copy user_type attributes =
    let icon = img ~src:(uri_of_string (fun () -> "imgs/" ^ (user_type_to_icon_file user_type))) ~alt:"users" ~a:attributes () in
    let nattributes =  (a_class ["hidden"]) :: attributes in
    let icon_copy = img ~src:(uri_of_string (fun () -> "imgs/" ^ (user_type_to_icon_file user_type))) ~alt:"users" ~a:nattributes () in
    icon, icon_copy

  let make_icon_and_text ?usert:(u=`All) ?userid:(uid=None) ?draggable:(d=true) ?size:(s=16) text =
    let attributes = [a_height s; a_width s;
                      a_draggable d] in
    let attributes = match uid with
      | None -> attributes
      | Some x -> (a_id (Printf.sprintf "%d" x)) :: attributes
    in
    let icon, icon2 = create_img_and_hidden_copy u attributes in
    let nattributes =  (a_class ["hidden"]) :: attributes in
    let icon2 = img ~src:(uri_of_string (fun () -> "imgs/" ^ (user_type_to_icon_file u))) ~alt:"users" ~a:nattributes () in
    (icon, pcdata text, icon2)

  let make_users_basket_in_div number_of_users =
    let text =
      if number_of_users == 0 then "Users bin"
      else Printf.sprintf "%d users in the bin" number_of_users
    in
    let icon, text, _ = make_icon_and_text ~draggable:false ~size:42 text in
    [icon; text]
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
 let res = tr ~a:[a_draggable true; a_id x.Utils.url] (Utils.print_event x) in
 let res_dom = Html5.To_dom.of_element res in
 let open Lwt_js_events in
 let ondragstarts ev _ =
   let event_id = x.Utils.url in
   ev##dataTransfer##setData((Js.string "event_id"), (Js.string event_id));
   Lwt.return_unit
 in
 Lwt.async (fun () -> dragstarts res_dom ondragstarts);
 res

type attended_type = [`Invited | `Declined | `Attending ]
type relative_type = [`RelInvited | `RelNotInvited | `RelDeclined | `RelAttending| `AllCurrEvent]

type 'a one_button_to_move =  {
  button_elt: 'a Eliom_content.Html5.elt;
  related_event_id: string;
  displayed_information: attended_type * relative_type;
  elt_on_the_right: 'a Eliom_content.Html5.elt;
}

type 'a one_legend_button_move = {
  legend_button: 'a Eliom_content.Html5.elt;
  legend_button_type: relative_type;
  legend_elt_on_the_right: 'a Eliom_content.Html5.elt;
}

type 'a ui_events = {
  events_in_db_container: Utils.event Events_store.t;
  selected_events: (selected_events * Utils.spans) Events_store.t;
  resolved_events_cache: Utils.event_and_users Events_store.t;
  nb_event_per_request: int;
  url_input: Dom_html.inputElement Js.t;
  db_selected_events_div: 'a Eliom_content.Html5.elt;
  all_users_div: Dom_html.element Js.t;
  reference_event_title: string;
  reference_event_div_container: Dom_html.element Js.t;
  reference_event_div: 'a Eliom_content.Html5.elt;
  reference_event_table: Dom_html.element Js.t;
  selected_events_title: string;
  selected_events_div_container: Dom_html.element Js.t;
  selected_events_div: 'a Eliom_content.Html5.elt;
  selected_events_table: Dom_html.element Js.t;
  legend_div: 'a Eliom_content.Html5.elt;
  mutable div_in_legend_div: 'a Eliom_content.Html5.elt;
  mutable legend_displayed: bool;
  mutable ref_event: reference_event;
  mutable user_sets: user_sets;
  mutable curr_offset: Int32.t;
  mutable curr_query: string option;
  mutable buttons_to_move: 'a one_button_to_move list;
  mutable legend_buttons_to_move: 'a one_legend_button_move list;
  mutable all_users_container: Utils.RsvpSet.t;
  demo_text_span: 'a Eliom_content.Html5.elt;
  example_queries: Dom_html.element Js.t;
}

let append_legend_button_move_from_icon_type icon icon_copy icon_type all_buttons =
  let to_append = match icon_type with
      | `All_events | `All -> []
      | `Attended_ref -> [{legend_button=icon;
                           legend_button_type=`RelAttending;
                           legend_elt_on_the_right=icon_copy}]
      | `Declined_ref -> [{legend_button=icon;
                           legend_button_type=`RelDeclined;
                           legend_elt_on_the_right=icon_copy}]
      | `Invited_ref -> [{legend_button=icon;
                          legend_button_type=`RelInvited;
                          legend_elt_on_the_right=icon_copy
                         }]
      | `Not_invited_ref -> [{legend_button=icon;
                              legend_button_type=`RelNotInvited;
                              legend_elt_on_the_right=icon_copy
                             }]
  in
  all_buttons := List.append !all_buttons to_append

let make_users_inequation all_buttons =
  let make_icon u =
    let s = 16 in
    let icon, icon2 = create_img_and_hidden_copy u [a_height s; a_width s] in
    append_legend_button_move_from_icon_type icon icon2 u all_buttons;
    [icon; icon2]
  in
  let flatten l = List.fold_left (fun accum res -> List.append accum res) [] l in
  let overall_inequation =
    flatten [make_icon `All_events;
             [pcdata " != "];
             make_icon `Attended_ref;
             [pcdata " + "];
             make_icon `Declined_ref;
             [pcdata " + "];
             make_icon `Invited_ref;
             [pcdata " + "];
             make_icon `Not_invited_ref;
             [pcdata ", but"]]
  in
  let overall_equation =
    flatten [make_icon `All_events;
             [pcdata " = "];
             make_icon `Invited_ref;
             [pcdata " + "];
             make_icon `Not_invited_ref]
  in
  let list_of_equations = List.rev (List.fold_left (fun a b -> (li b) :: a) [] [overall_inequation; overall_equation]) in
  [ul list_of_equations]

let create_div_map l f all_buttons =
  List.map (fun (x, text) -> f (
    let icon, text, copy = make_icon_and_text ~usert:x ~draggable:false ~size:20 text in
    append_legend_button_move_from_icon_type icon copy x all_buttons;
    [icon; copy; text])) l


let display_legend_div ?force:(f=false) t =
  if not f && t.legend_displayed then ()
  else
    let all_buttons = ref [] in
    let legend_info = [(`Attended_ref,
                        " attended to the reference event,");
                       (`Declined_ref,
                        " declined the reference event's invitation,");
                       (`Invited_ref,
                        " got invited to the reference event,");
                       (`Not_invited_ref,
                        " were not invited to the reference event.")] in
    let in_legend_div =  flatten
      [create_div_map [(`All_events,
                        " Facebook users that attended, declined or were invited to the event. Among these users we have the ones that:")] div all_buttons;
       [ul (create_div_map legend_info li all_buttons)];
       [div [pcdata "To understand more about these sets, note that :"]];
       [div (make_users_inequation all_buttons)]]
    in
    let div_in_legend_div = div in_legend_div in
    let () = Html5.Manip.replaceChildren t.legend_div [div_in_legend_div] in
    t.div_in_legend_div <- div_in_legend_div;
    t.legend_displayed <- true;
    t.legend_buttons_to_move <- !all_buttons

let create
    url_input
    db_selected_events_div
    all_users_div
    reference_event_title reference_event_div_container reference_event_div reference_event_table
    selected_events_title selected_events_div_container selected_events_div selected_events_table
    legend_div demo_text_span example_queries =
  {
    events_in_db_container = Events_store.create 100;
    selected_events = Events_store.create 100;
    resolved_events_cache = Events_store.create 100;
    nb_event_per_request = 5;
    url_input = Html5.To_dom.of_input url_input;
    db_selected_events_div = db_selected_events_div;
    all_users_div = Html5.To_dom.of_element all_users_div;
    reference_event_title = reference_event_title;
    reference_event_div_container = Html5.To_dom.of_element reference_event_div_container;
    reference_event_div = reference_event_div;
    reference_event_table = Html5.To_dom.of_element reference_event_table;
    selected_events_title = selected_events_title;
    selected_events_div_container = Html5.To_dom.of_element selected_events_div;
    selected_events_div = selected_events_div;
    selected_events_table = Html5.To_dom.of_element selected_events_table;
    legend_div = legend_div;
    div_in_legend_div = div [];
    legend_displayed = false;
    ref_event = `Undefined;
    user_sets = create_user_sets ();
    curr_offset = 0l;
    curr_query = None;
    buttons_to_move = [];
    legend_buttons_to_move = [];
    all_users_container = Utils.RsvpSet.empty;
    demo_text_span = demo_text_span;
    example_queries = Html5.To_dom.of_element example_queries;
  }

let make_user_button t user utype text =
  let button, text, copy = make_icon_and_text ~usert:utype ~userid:(Some user.user_id) text in
  let res = div [button; copy; text] in
  let res_dom = Html5.To_dom.of_element res in
  let open Lwt_js_events in
  let ondragstarts ev _ =
    let user_id = Printf.sprintf "%d" user.user_id in
    ev##dataTransfer##setData((Js.string "button_id"), (Js.string user_id));
    Lwt.return_unit
  in
  Lwt.async (fun () -> dragstarts res_dom ondragstarts);
  res, button, text, copy

let display_compared_rsvp t event compared_users compared_span =
  (*let () = check_event_coherence event in*)
  let compared_users_set = Utils.make_rsvp_set compared_users in
  let user_sets, compared_user = append_new_user_set t.user_sets compared_users_set in
  let user_sets, common_attending = append_new_user_set user_sets (common_users event.Utils.attending compared_users) in
  let user_sets, common_declined = append_new_user_set user_sets (common_users event.Utils.declined compared_users) in
  let user_sets, common_invited = append_new_user_set user_sets (common_users event.Utils.invited compared_users) in
  let not_invited_users = Utils.RsvpSet.diff compared_users_set common_invited.users in
  let user_sets, not_invited = append_new_user_set user_sets not_invited_users in
  let map_arg = [(compared_user, `All_events,
                  Printf.sprintf "%d" (List.length compared_users), `AllCurrEvent);
                 (common_attending, `Attended_ref,
                  Printf.sprintf "%d" (Utils.RsvpSet.cardinal common_attending.users), `RelAttending);
                 (common_declined, `Declined_ref ,
                  Printf.sprintf "%d" (Utils.RsvpSet.cardinal common_declined.users), `RelDeclined);
                 (common_invited, `Invited_ref,
                  Printf.sprintf "%d" (Utils.RsvpSet.cardinal common_invited.users), `RelInvited);
                 (not_invited, `Not_invited_ref,
                  Printf.sprintf "%d" (Utils.RsvpSet.cardinal not_invited.users), `RelNotInvited)]
  in
  let buttons = List.map (fun (u, x, y, rel_button_type) -> let div, button, text, copy = make_user_button t u x y in
                                                            div, (button, rel_button_type, text, copy)) map_arg
  in
  Html5.Manip.replaceChildren compared_span (List.map Pervasives.fst buttons);
  t.user_sets <- user_sets;
  List.map Pervasives.snd buttons

let display_rsvp t users span =
  let user_sets, displayed_users = append_new_user_set t.user_sets (Utils.make_rsvp_set users) in
  let under_span, button, text, copy = make_user_button t displayed_users `All_events (Printf.sprintf "%d" (List.length users)) in
  Html5.Manip.replaceChildren span [under_span];
  t.user_sets <- user_sets;
  button, text, copy

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

let display_event ?ref_event:(r=None) ?filter_attending_type:(f_attending=fun _ -> true) t event span =
  let f arg =
    let u, s, rsvp_info_type = arg in
    match r with
    | None ->
      let button, text, copy = display_rsvp t u s in
      [{button_elt = button;
        related_event_id = event.Utils.ev_url;
        displayed_information = (rsvp_info_type, `AllCurrEvent);
        elt_on_the_right = copy;}]
    | Some ref_event -> List.map (fun x -> let b, compared_rsvp_into_type, text, copy = x in
                                           {button_elt = b;
                                            related_event_id = event.Utils.ev_url;
                                            displayed_information = (rsvp_info_type, compared_rsvp_into_type);
                                            elt_on_the_right = copy;
                                           }) (display_compared_rsvp t ref_event u s)
  in
  flatten (List.map f (List.filter f_attending [(event.Utils.attending, span.Utils.attending_span, `Attending);
                                                (event.Utils.declined, span.Utils.declined_span, `Declined);
                                                (event.Utils.invited, span.Utils.invited_span, `Invited)]))

let refresh_compared_buttons_only t button_param =
  let event_url = button_param.related_event_id in
  let button_type, _ = button_param.displayed_information in
  let filter_bt b_elt =
    let _, _, b_elt_button_type = b_elt in
    b_elt_button_type = button_type
  in
  let curr_event, spans = Events_store.find t.selected_events event_url in
  let curr_event = match curr_event with
    | `Resolving _ -> assert(false)
    | `Resolved curr_event -> curr_event in
  let ref_event = match t.ref_event with
    | `Undefined | `Resolving (_, _) -> assert(false)
    | `Resolved (ref_event, _) -> ref_event
  in
  let buttons = display_event ~filter_attending_type:filter_bt ~ref_event:(Some ref_event) t curr_event spans in
  t.buttons_to_move <- List.append (List.filter (fun x ->
    let related_event = x.related_event_id = event_url in
    let rsvp_info_type, bt = x.displayed_information in
    not (related_event && rsvp_info_type = button_type)) t.buttons_to_move) buttons


let refresh_ui t =
 let resolved_events = ref [] in
 let () = Events_store.iter (fun k (event, spans) ->
   match event with
     | `Resolving _ -> ()
     | `Resolved x -> resolved_events:= (x, spans) :: !resolved_events)
   t.selected_events
 in
 let buttons, ref_event_resolved =
 match t.ref_event with
   | `Undefined | `Resolving (_, _) -> begin
     (flatten (List.map (fun (curr_event, span) ->
                         display_event t curr_event span)
               !resolved_events),
      false)
   end
   | `Resolved (ref_event, ref_span) -> begin
     let ref_buttons = display_event t ref_event ref_span in
     let all_other_buttons = flatten (List.map (fun (curr_event, span) ->
                                                display_event ~ref_event:(Some ref_event) t curr_event span)
                                      !resolved_events) in
     (List.append ref_buttons all_other_buttons, true)
   end
 in
 t.buttons_to_move <- buttons;
 Utils.show_element t.all_users_div;
 if List.length !resolved_events > 0 && ref_event_resolved then (
   display_legend_div t)

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
  let db_table = Utils.make_table
 ~additional_class:["db_container"] ["Name"; "Owner"; "Location"; "Date"] trs in
  lwt other_links = compute_prev_next_handlers events in
  let new_div = [db_table] @ other_links in
  Html5.Manip.replaceChildren t.db_selected_events_div new_div;
  Lwt.return_unit

let get_events_in_db t queryo =
  t.curr_offset <- 0l;
  lwt () = get_and_record_events t queryo in
  let () = Utils.show_element t.example_queries in
  Lwt.return_unit

let on_db_input_changes t ev _ =
  let queryo =
    match (Js.to_string t.url_input##value) with
      | "" -> None
      | str -> Some str
  in
  get_events_in_db t queryo

let get_event_data ev data_name =
  let data_val = ev##dataTransfer##getData((Js.string data_name)) in
  Js.to_string data_val

let get_event_id ev = get_event_data ev "event_id"
let get_button_id ev = get_event_data ev "button_id"

let drop_event_id_in_selected_events t event_id =
  (* create the UI elements for new selected elements
     and launch the associated FB requests to compute attending, etc *)
  let to_resolve_lwt = ref None in
  let () = match Events_store.mem t.selected_events event_id with
    | false -> begin
      let event = Events_store.find t.events_in_db_container event_id in
      let spans = Utils.create_spans () in
      let () = Utils.replace_event_spans event spans in
      match Events_store.mem t.resolved_events_cache event_id with
        | false -> let () = to_resolve_lwt:= Some (resolve_event t event spans) in
                   Events_store.add t.selected_events event.Utils.url ((`Resolving event, spans))
        | true -> let resolved_event = Events_store.find t.resolved_events_cache event_id in
                  Events_store.add t.selected_events event.Utils.url ((`Resolved resolved_event, spans))
    end
    | true -> () in
    (* display the UI with temporarily non available data *)
  let trs = ref [] in
  let () = Events_store.iter (fun url (_, s) ->
    trs:= tr ~a:[a_id url] (Utils.integrate_spans_in_td s) :: !trs)
    t.selected_events in
  let table = Utils.make_complete_event_table ~caption:(Some t.selected_events_title) !trs in
  Html5.Manip.replaceChildren t.selected_events_div [table];
    (* wait for the resolution of FB requests *)
  lwt () = match !to_resolve_lwt with
    | None -> Lwt.return_unit
    | Some x -> x
  in
  (* compute differences *)
  let () = refresh_ui t in
  Lwt.return_unit

let on_user_drop_in_selected_events t ev _ =
  Dom.preventDefault ev;
  drop_event_id_in_selected_events t (get_event_id ev)

let drop_event_id_in_reference_event t event_id =
  let to_resolve_lwt = ref None in
  let create_new_ref_event () =
    let event = Events_store.find t.events_in_db_container event_id in
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
    match Events_store.mem t.resolved_events_cache event_id with
      | false -> let () = to_resolve_lwt := Some (resolve_ref_event ()) in
                 t.ref_event <- `Resolving (event, spans)
      | true -> let resolved_event = Events_store.find t.resolved_events_cache event_id in
                t.ref_event <- `Resolved (resolved_event, spans);
  in
  let () = match t.ref_event with
    | `Undefined -> create_new_ref_event ()
    | `Resolving (y, _) ->
      if y.Utils.url = event_id then ()
      else create_new_ref_event ()
    | `Resolved (y, _) ->
      if y.Utils.ev_url = event_id then ()
      else create_new_ref_event ()
  in
  (* display the UI with, maybe, temporarily non available data *)
  let id, span = match t.ref_event with
    | `Undefined -> assert(false)
    | `Resolved (r, s) -> (r.Utils.ev_url, s)
    | `Resolving (r, s) -> (r.Utils.url, s)
  in
  let trs = [tr ~a:[a_id id] (Utils.integrate_spans_in_td span)] in
  let table = Utils.make_complete_event_table ~caption:(Some t.reference_event_title) trs in
  Html5.Manip.replaceChildren t.reference_event_div [table];
  (* wait for the resolution of FB requests *)
  lwt () = match !to_resolve_lwt with | None -> Lwt.return_unit | Some x -> x in
  (* compute differences *)
  let () = refresh_ui t in
  Lwt.return_unit

let on_user_drop_in_ref_event t ev _ =
  Dom.preventDefault ev;
  drop_event_id_in_reference_event t (get_event_id ev)

let add_users_in_button_id t button_id =
  let corresponding_user = find_user t.user_sets button_id in
  Utils.RsvpSet.union t.all_users_container corresponding_user.users

let update_all_users_basket t new_users =
  let new_basket = make_users_basket_in_div (Utils.RsvpSet.cardinal new_users) in
  Html5.Manip.replaceChildren (Html5.Of_dom.of_element t.all_users_div) new_basket

let update_all_users_basket_from_button_id t button_id=
  let new_users = add_users_in_button_id t button_id in
  let () = t.all_users_container <- new_users in
  update_all_users_basket t new_users

let on_all_users_div_drop t ev _ =
  Dom.preventDefault ev;
  let button_id = int_of_string (get_button_id ev) in
  let () = update_all_users_basket_from_button_id t button_id in
  Lwt.return_unit

let set_demo_text t texto =
  let to_set = match texto with
    | None -> []
    | Some (text, class_name) -> [div ~a:[a_class [class_name]] [pcdata text]]
  in
  Html5.Manip.replaceChildren t.demo_text_span to_set
}}
