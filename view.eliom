{shared{
open Eliom_content
open Html5.D
open Eliom_parameter
}}


let view_service unused unused2 =
  let db_selected_events_span = span [] in

  let selected_events_span = span [] in
  let selected_events_div = div ~a:[a_class ["container"]] [pcdata "put your selecteds event here"; selected_events_span] in
  let reference_event_span = span [] in
  let reference_event_div = div ~a:[a_class ["container"]] [pcdata "put your reference event here"; reference_event_span] in

  let all_users_div = div (View_events.make_users_basket_in_div 0) in
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
  let in_legend_div = List.map (fun (x, text) -> div (View_events.make_users_in_div ~usert:x ~draggable:false ~size:20 text)) legend_info in
  let legend_div = div in_legend_div in
  let url_input = string_input ~input_type:`Text () in
  let _ = {unit{
    let open View_events in
    let t = create () in
    let all_users_container = ref Utils.RsvpSet.empty in
    let open Lwt_js_events in
    async (fun () -> changes (Html5.To_dom.of_element %url_input) (View_events.on_db_input_changes t %db_selected_events_span %selected_events_span));
    let ondrop ev _ =
      Dom.preventDefault ev;
      Firebug.console##log("on drops");
      let data_val = ev##dataTransfer##getData((Js.string "button_id")) in
      let (data_val: string) = Js.to_string data_val in
      let button_id = int_of_string data_val in
      Firebug.console##log((Printf.sprintf "Got button_id %d" button_id));
      let corresponding_user = find_user t.user_sets button_id in
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
      (View_events.on_user_drop_in_ref_event t %reference_event_span));
    async (fun () -> drops (Html5.To_dom.of_element %selected_events_div)
      (View_events.on_user_drop_in_selected_events t %selected_events_span));
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
