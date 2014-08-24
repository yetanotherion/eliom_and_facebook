{shared{
  open Eliom_content
  open Html5.D
  open Eliom_parameter

  type user_set = {
    users: Utils.RsvpSet.t;
    user_id: int;
  }

  type user_sets = {
    all_users: user_set list;
    next_id: int;
  }

  let user_type_to_icon_file x =
    match x with
      | `All -> "users.png"
      | `All_events -> "events_all.png"
      | `Attended_ref -> "events_ref_attended.png"
      | `Declined_ref -> "events_ref_declined.png"
      | `Invited_ref -> "events_ref_invited.png"
      | `Not_invited_ref -> "events_ref_not_invited.png"

  let make_users_in_div ?usert:(u=`All) ?draggable:(d=true) ?size:(s=16) text =
    let icon = img ~src:(uri_of_string (fun () -> "imgs/" ^ (user_type_to_icon_file u))) ~alt:"users" ~a:[a_height s; a_width s;
                                                                                                          a_draggable d] () in
    [icon; pcdata text]

  let make_users_basket_in_div number_of_users =
    make_users_in_div (Printf.sprintf "%d fans" number_of_users)
      ~draggable:false ~size:42

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
          (* XXX check whether db value should be updated and update it if so*)
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

 let make_user_button user utype text =
   let res = div (make_users_in_div ~usert:utype text) in
   let res_dom = Html5.To_dom.of_element res in
   let open Lwt_js_events in
   let ondragstarts ev _ =
     Firebug.console##log("on dragstarts");
     let user_id = Printf.sprintf "%d" user.user_id in
     ev##dataTransfer##setData((Js.string "button_id"), (Js.string user_id));
     Firebug.console##log("set user_id:" ^ user_id);
     Lwt.return_unit
   in
   Lwt.async (fun () -> dragstarts res_dom ondragstarts);
   res

 let check_event_coherence event =
   let attending = Utils.make_rsvp_set event.Utils.attending in
   let declined = Utils.make_rsvp_set event.Utils.declined in
   let invited = Utils.make_rsvp_set event.Utils.invited in
   let open Utils.RsvpSet in
   Firebug.console##log(Printf.sprintf "attending&declined:%d" (cardinal (inter attending declined)));
   Firebug.console##log(Printf.sprintf "attending&invited:%d" (cardinal (inter attending invited)));
   Firebug.console##log(Printf.sprintf "declined&invited:%d" (cardinal (inter declined invited)))

 let compare_rsvp user_sets_ref event compared_users compared_span =
   (*let () = check_event_coherence event in*)
   let compared_users_set = Utils.make_rsvp_set compared_users in
   let user_sets, compared_user = append_new_user_set !user_sets_ref compared_users_set in
   let user_sets, common_attending = append_new_user_set user_sets (common_users event.Utils.attending compared_users) in
   let user_sets, common_declined = append_new_user_set user_sets (common_users event.Utils.declined compared_users) in
   let user_sets, common_invited = append_new_user_set user_sets (common_users event.Utils.invited compared_users) in
   let not_invited_users = Utils.RsvpSet.diff compared_users_set common_invited.users in
   let user_sets, not_invited = append_new_user_set user_sets not_invited_users in
   Html5.Manip.replaceChildren compared_span
     (List.map (fun (u, x, y) -> make_user_button u x y) [(compared_user, `All_events,
                                                           Printf.sprintf "%d" (List.length compared_users));
                                                          (common_attending, `Attended_ref,
                                                           Printf.sprintf "%d" (Utils.RsvpSet.cardinal common_attending.users));
                                                          (common_declined, `Declined_ref ,
                                                           Printf.sprintf "%d" (Utils.RsvpSet.cardinal common_declined.users));
                                                          (common_invited, `Invited_ref,
                                                           Printf.sprintf "%d" (Utils.RsvpSet.cardinal common_invited.users));
                                                          (not_invited, `Not_invited_ref,
                                                           Printf.sprintf "%d" (Utils.RsvpSet.cardinal not_invited.users))]);
   user_sets_ref := user_sets

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

 let on_db_input_changes events_in_db_container db_selected_events_span selected_events_span user_sets _ _ =
   lwt () = Utils.lwt_autologin () in
   lwt events = %rpc_get_events () in
   let () = Utils.Events_store.clear events_in_db_container in
   let () = List.iter (fun x -> Utils.Events_store.add events_in_db_container x.Utils.url x) events in
   let trs = List.map make_selectable_event events in
   let db_table = Utils.make_table ["Name"; "Owner"; "Location"; "Date"] trs in
   Html5.Manip.replaceChildren db_selected_events_span [db_table];
   Lwt.return_unit

 let resolve_event selected_events event spans =
   let event_ref = ref None in
   let must x = match x with | None -> assert(false) | Some x -> x in
   lwt () = process_event event spans event_ref in
   let () = Utils.Events_store.remove selected_events event.Utils.url in
   let () = Utils.Events_store.add selected_events event.Utils.url (`Resolved (must !event_ref),
                                                                    spans) in
   Lwt.return_unit

 let display_differences selected_events ref_event user_sets =
   let resolved_events = ref [] in
   let () = Utils.Events_store.iter (fun k (event, spans) ->
     match event with
       | `Resolving _ -> ()
       | `Resolved x -> resolved_events:= (x, spans) :: !resolved_events)
     selected_events
   in
   match !ref_event with
     | `Undefined | `Resolving (_, _) ->
       List.iter (fun (_, span) ->
         List.iter (fun s ->
           Html5.Manip.replaceChildren s [pcdata "the reference is not computed yet"])
           [span.Utils.attending_span; span.Utils.declined_span; span.Utils.invited_span]) !resolved_events
     | `Resolved (x, _) ->
       List.iter (fun (curr_event, span) ->
         List.iter (fun (u, s) -> compare_rsvp user_sets x u s)
           [(curr_event.Utils.attending, span.Utils.attending_span);
            (curr_event.Utils.declined, span.Utils.declined_span);
            (curr_event.Utils.invited, span.Utils.invited_span)]) !resolved_events

 let on_user_drop_in_selected_events events_in_db_container ref_event selected_events user_sets selected_events_span ev _ =
   Dom.preventDefault ev;
   let data_val = ev##dataTransfer##getData((Js.string "event_id")) in
   let (data_val: string) = Js.to_string data_val in
   (* create the UI elements for new selected elements
      and launch the associated FB requests to compute attending, etc *)
   let to_resolve_lwt = ref None in
   let () = match Utils.Events_store.mem selected_events data_val with
     | false -> let event = Utils.Events_store.find events_in_db_container data_val in
                let spans = Utils.create_spans () in
                let () = Utils.replace_event_spans event spans in
                let () = to_resolve_lwt:= Some (resolve_event selected_events event spans) in
                Utils.Events_store.add selected_events event.Utils.url ((`Resolving event, spans))
     | true -> () in
   (* display the UI with temporarily non available data *)
   let trs = ref [] in
   let () = Utils.Events_store.iter (fun _ (_, s) ->
     trs:= tr (Utils.integrate_spans_in_td s) :: !trs)
     selected_events in
   let table = Utils.make_complete_event_table !trs in
   Html5.Manip.replaceChildren selected_events_span [table];
   (* wait for the resolution of FB requests *)
   lwt () = match !to_resolve_lwt with
     | None -> Lwt.return_unit
     | Some x -> x
   in
   (* compute differences *)
   let () = display_differences selected_events ref_event user_sets in
   Lwt.return_unit

 let on_user_drop_in_ref_event events_in_db_container ref_event selected_events user_sets ref_event_span ev _ =
   Dom.preventDefault ev;
   let data_val = ev##dataTransfer##getData((Js.string "event_id")) in
   let (data_val: string) = Js.to_string data_val in
   let to_resolve_lwt = ref None in
   let create_new_ref_event () =
     let event = Utils.Events_store.find events_in_db_container data_val in
     let spans = Utils.create_spans () in
     let () = Utils.replace_event_spans event spans in
     let resolve_ref_event () =
       let event_ref = ref None in
       let must x = match x with | None -> assert(false) | Some x -> x in
       lwt () = process_event event spans event_ref in
       let () = ref_event := `Resolved (must !event_ref, spans) in
       Lwt.return_unit
     in
     let () = to_resolve_lwt := Some (resolve_ref_event ()) in
     ref_event:= `Resolving (event, spans)
   in
   let () = match !ref_event with
     | `Undefined -> create_new_ref_event ()
     | `Resolving (y, _) ->
       if y.Utils.url = data_val then ()
       else create_new_ref_event ()
     | `Resolved (y, _) ->
       (* XXX add the url in correct_event_res, and do as above *)
       create_new_ref_event ()
   in
   (* display the UI with, maybe, temporarily non available data *)
   let trs = match !ref_event with
     | `Undefined -> assert(false)
     | `Resolved (_, s) | `Resolving (_, s) -> [tr (Utils.integrate_spans_in_td s)]
   in
   let table = Utils.make_complete_event_table trs in
   Html5.Manip.replaceChildren ref_event_span [table];
   (* wait for the resolution of FB requests *)
   lwt () = match !to_resolve_lwt with | None -> Lwt.return_unit | Some x -> x in
   (* compute differences *)
   let () = display_differences selected_events ref_event user_sets in
   Lwt.return_unit
 }}


let view_service unused unused2 =
  let db_selected_events_span = span [] in

  let selected_events_span = span [] in
  let selected_events_div = div ~a:[a_class ["container"]] [pcdata "put your selecteds event here"; selected_events_span] in
  let reference_event_span = span [] in
  let reference_event_div = div ~a:[a_class ["container"]] [pcdata "put your reference event here"; reference_event_span] in

  let all_users_div = div (make_users_basket_in_div 0) in
  let legend_info = [(`All_events,
                      "All corresponding fans.");
                     (`Attended_ref,
                      "Subset of corresponding fans that attended to the reference event.");
                     (`Declined_ref,
                      "Subset of corresponding fans that declined the reference event's invitation.");
                     (`Invited_ref,
                      "Subset of corresponding fans that were invited to the reference event.");
                     (`Not_invited_ref,
                      "Subset of corresponding fans that were not invited to the reference event.")] in
  let in_legend_div = List.map (fun (x, text) -> div (make_users_in_div ~usert:x ~draggable:false ~size:20 text)) legend_info in
  let legend_div = div in_legend_div in
  let url_input = string_input ~input_type:`Text () in
  let _ = {unit{
    let selected_events = Utils.Events_store.create 100 in
    let reference_event = ref `Undefined in
    let events_in_db_container = Utils.Events_store.create 100 in
    let all_users_container = ref Utils.RsvpSet.empty in
    let user_sets = ref {
      all_users = [];
      next_id = 0;
    } in
    let open Lwt_js_events in
    async (fun () -> changes (Html5.To_dom.of_element %url_input) (on_db_input_changes events_in_db_container %db_selected_events_span %selected_events_span user_sets));
    let ondrop ev _ =
      Dom.preventDefault ev;
      Firebug.console##log("on drops");
      let data_val = ev##dataTransfer##getData((Js.string "button_id")) in
      let (data_val: string) = Js.to_string data_val in
      let button_id = int_of_string data_val in
      Firebug.console##log((Printf.sprintf "Got button_id %d" button_id));
      let corresponding_user = List.find (fun x -> x.user_id = button_id) (!user_sets).all_users in
      all_users_container := Utils.RsvpSet.union !all_users_container corresponding_user.users;
      Html5.Manip.replaceChildren %all_users_div (make_users_basket_in_div (Utils.RsvpSet.cardinal !all_users_container));
      Lwt.return_unit
    in
    let ondragover ev _ =
      Dom.preventDefault ev;
      Lwt.return_unit
    in
    async (fun () -> dragovers (Html5.To_dom.of_element %all_users_div) ondragover);
    async (fun () -> dragovers (Html5.To_dom.of_element %selected_events_div) ondragover);
    async (fun () -> dragovers (Html5.To_dom.of_element %reference_event_div) ondragover);
    async (fun () -> drops (Html5.To_dom.of_element %all_users_div) ondrop);
    async (fun () -> drops (Html5.To_dom.of_element %reference_event_div)
      (on_user_drop_in_ref_event events_in_db_container reference_event selected_events user_sets %reference_event_span));
    async (fun () -> drops (Html5.To_dom.of_element %selected_events_div)
      (on_user_drop_in_selected_events events_in_db_container reference_event selected_events user_sets %selected_events_span));
  }}
  in
  let all_body = [Utils.fb_root_div;
                  div ~a:[a_class ["container-fluid"]]
                    [div ~a:[a_class ["row-fluid"]]
                        [div ~a:[a_class ["span3"]]
                            [div ~a:[a_class ["well"; "sidebar-nav"]]
                                [ul ~a:[a_class ["nav"; "nav-list"]]
                                    [li ~a:[a_class ["active"]] [url_input];
                                     li [db_selected_events_span]]]];
                         div ~a:[a_class ["span9"]] [all_users_div;
                                                     reference_event_div;
                                                     selected_events_div;
                                                     legend_div]]]]
  in
  let b = all_body @ Utils.bs_scripts in
  let h = Utils.bootstrap_metas @ Utils.bs_icons in
  Lwt.return (Eliom_tools.D.html ~title: "advertise your event"
                ~css:[["css"; "bootstrap.min.css"];
                      ["css"; "mb.css"];
                      ["css"; "mb-view.css"];
                      ["css"; "bootstrap-responsive.css"];
                      ["css"; "signin.css"]]
                ~js:[["js"; "jquery.min.js"];
                     ["js"; "bootstrap.min.js"]]
                (body b)
                ~other_head:h)
