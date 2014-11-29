{shared{
open Eliom_content
open Html5.D
open Eliom_parameter
}}


let view_service unused unused2 =
  let db_selected_events_span = span [] in
  let selected_events_span = span [] in
  let selected_events_img = img ~src:(uri_of_string (fun () -> "imgs/add_additional_events.png")) ~alt:"put your selected events here" () in
  let selected_events_div = div ~a:[a_class ["container"]] [selected_events_img; selected_events_span] in
  let reference_event_span = span [] in
  let reference_event_img = img ~src:(uri_of_string (fun () -> "imgs/set_reference_event.png")) ~alt:"put your reference event here" () in
  let reference_event_div = div ~a:[a_class ["container"]] [reference_event_img; reference_event_span] in

  let all_users_div = div ~a:[a_class ["hidden"]] (Ui_events.make_users_basket_in_div 0) in
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
  let in_legend_div = List.map (fun (x, text) -> div (Ui_events.make_users_in_div ~usert:x ~draggable:false ~size:20 text)) legend_info in
  let legend_div = div ~a:[a_class ["hidden"]] in_legend_div in
  let user_select_ui_div =  div ~a:[a_class ["span9"]] [all_users_div;
                                                        reference_event_div;
                                                        selected_events_div;
                                                        legend_div] in

  let url_input = string_input ~input_type:`Text () in
  let _ = {unit{
    let t = View_events.create
                        %url_input
                        %db_selected_events_span
                        %all_users_div
                        %reference_event_span %reference_event_img %reference_event_div
                        %selected_events_span %selected_events_img %selected_events_div
                        %legend_div in
    View_events.setup t
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
                         user_select_ui_div]]] in
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
