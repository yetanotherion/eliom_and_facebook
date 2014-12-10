{shared{
open Eliom_content
open Html5.D
open Eliom_parameter
}}


let view_service unused unused2 =
  let db_selected_events_div = div [] in
  let six = [1; 2; 3; 4; 5; 6; 7] in
  let create_initial_table header message =
    Utils.make_complete_event_table ~caption:(Some header)
      [tr (List.map (fun d ->
      let text =
        if d = 1 then message
        else ""
      in
      td [pcdata text]) six)]
  in
  let selected_events_title = "Selected events" in
  let reference_event_title = "Reference event" in
  let selected_events_table = create_initial_table selected_events_title "Drag and drop one of the events here" in
  let selected_events_div = div [selected_events_table] in
  let selected_events_div_container = div ~a:[a_class ["container"]] [selected_events_div] in
  let reference_event_table = create_initial_table reference_event_title "Drag and drop a reference event here" in
  let reference_event_div = div [reference_event_table] in
  let reference_event_div_container = div ~a:[a_class ["container"]] [reference_event_div] in
  let demo_text_span = span [] in
  let all_users_div = div ~a:[a_class ["hidden"]] (Ui_events.make_users_basket_in_div 0) in
  let legend_info = [(`Attended_ref,
                      " attended to the reference event,");
                     (`Declined_ref,
                      " declined the reference event's invitation,");
                     (`Invited_ref,
                      " got invited to the reference event,");
                     (`Not_invited_ref,
                      " were not invited to the reference event.")] in
  let create_div_map l f =
    List.map (fun (x, text) -> f (Ui_events.make_users_in_div ~usert:x ~draggable:false ~size:20 text)) l
  in
  let in_legend_div =  List.fold_left List.append []
    [create_div_map [(`All_events,
                      " Facebook users that attended, declined or were invited to the event. Among these users we have the ones that:")] div;
     [ul (create_div_map legend_info li)];
     [div [pcdata "To understand more about these sets, note that :"]];
     [div Ui_events.users_inequation]]
  in
  let legend_div = div ~a:[a_class ["hidden"]] in_legend_div in
  let user_select_ui_div =  div ~a:[a_class ["span9"]] [all_users_div;
                                                        selected_events_div_container;
                                                        reference_event_div_container;
                                                        legend_div] in
  let url_input = string_input ~input_type:`Text () in
  let example_queries = div ~a:[a_class ["hidden"]] [pcdata "Some query examples:";
                                                     ul (List.map (fun x -> li [pcdata x])
                                                           ["nb_invited >= 1000";
                                                            "name != <some_name>";
                                                            "owner = <some_owner>";
                                                            "(nb_declined <= 100 nb_attending > 200 owner:<some_owner>) or (location:<some_city>)"
                                                           ])]
  in
  let _ = {unit{
    let ui_t = Ui_events.create %url_input %db_selected_events_div
                                %all_users_div
                                %reference_event_title %reference_event_div_container %reference_event_div %reference_event_table
                                %selected_events_title %selected_events_div_container %selected_events_div %selected_events_table
                                %legend_div %demo_text_span %example_queries
    in
    let t = View_events.create ui_t in
    View_events.setup t
  }}
  in
  let all_body = [Utils.fb_root_div;
                  demo_text_span;
                  div ~a:[a_class ["container-fluid"]]
                    [div ~a:[a_class ["row-fluid"]]
                        [div ~a:[a_class ["span3"]]
                            [div ~a:[a_class ["well"; "sidebar-nav"]]
                                [ul ~a:[a_class ["nav"; "nav-list"]]
                                    [li ~a:[a_class ["active"]] [url_input];
                                     li [db_selected_events_div]]];
                             example_queries];
                         user_select_ui_div]]] in
  let b = all_body @ Utils.bs_scripts in
  let h = Utils.bootstrap_metas @ Utils.bs_icons in
  Lwt.return (Eliom_tools.D.html ~title: "advertise your event"
                ~css:[["css"; "bootstrap.min.css"];
                      ["css"; "mb.css"];
                      ["css"; "mb-view.css"];
                      ["css"; "bootstrap-responsive.css"];
                      ["css"; "signin.css"];
                      ["css"; "bubbles.css"]]
                ~js:[["js"; "jquery.min.js"];
                     ["js"; "bootstrap.min.js"]]
                (body b)
                ~other_head:h)
