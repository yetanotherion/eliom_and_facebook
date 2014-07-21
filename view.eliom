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
          (* XXX check whether db value should be updated *)
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
}}

let view_service unused unused2 =
  let selected_events_span = span [] in
  let db_selected_events_span = span [] in
  let all_users_container = ref Utils.RsvpSet.empty in
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
  let user_sets = ref {
    all_users = [];
    next_id = 0;
  } in
  let url_input = string_input ~input_type:`Text () in
  let _ = {unit{
    let open Lwt_js_events in
    let onchanges _ _ =
      lwt () = Utils.lwt_autologin () in
      lwt events = %rpc_get_events () in
      let trs = List.map (fun x -> tr (Utils.print_event x)) events in
      let db_table = Utils.make_table ["Name"; "Owner"; "Location"; "Date"] trs in
      Html5.Manip.replaceChildren %db_selected_events_span [db_table];
      let event_and_span = List.map (fun x ->
        let spans = Utils.create_spans () in
        let () = Utils.replace_event_spans x spans in
         (x, spans, ref None)) events in
      let trs = List.map (fun (event, s, _) -> tr (Utils.integrate_spans_in_td s)) event_and_span in
      let table = Utils.make_complete_event_table trs in
      Html5.Manip.replaceChildren %selected_events_span [table];
      lwt () = Lwt.join (List.map (fun (event, span, user_ref) -> process_event event span user_ref) event_and_span) in
      match event_and_span with
        | [] -> Lwt.return_unit
        | hd :: tl -> begin
          let (_, spans, event) = hd in
          let must ev = match !ev with | None -> assert(false) | Some x -> x in
          let event_ref = must event in
          List.iter (fun (_, s, other_event) ->
            let curr_event = must other_event in
            List.iter (fun (u, s) -> compare_rsvp %user_sets event_ref u s)
              [(curr_event.Utils.attending, s.Utils.attending_span);
               (curr_event.Utils.declined, s.Utils.declined_span);
               (curr_event.Utils.invited, s.Utils.invited_span)])
            tl;
          Lwt.return_unit
        end
    in
    async (fun () -> changes (Html5.To_dom.of_element %url_input) onchanges);
    let ondrop ev _ =
      Dom.preventDefault ev;
      Firebug.console##log("on drops");
      let data_val = ev##dataTransfer##getData((Js.string "button_id")) in
      let (data_val: string) = Js.to_string data_val in
      let button_id = int_of_string data_val in
      Firebug.console##log((Printf.sprintf "Got button_id %d" button_id));
      let users = %user_sets in
      let corresponding_user = List.find (fun x -> x.user_id = button_id) (!users).all_users in
      let all_users_container = %all_users_container in
      all_users_container := Utils.RsvpSet.union !all_users_container corresponding_user.users;
      Html5.Manip.replaceChildren %all_users_div (make_users_basket_in_div (Utils.RsvpSet.cardinal !all_users_container));
      Lwt.return_unit
    in
    let ondragover ev _ =
      Dom.preventDefault ev;
      Lwt.return_unit
    in
    async (fun () -> dragovers (Html5.To_dom.of_element %all_users_div) ondragover);
    async (fun () -> drops (Html5.To_dom.of_element %all_users_div) ondrop)
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
                                                     div ~a:[a_class ["container"]] [selected_events_span];
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
