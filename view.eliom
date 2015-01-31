{shared{
open Eliom_content
open Html5.D
open Eliom_parameter
}}

let create_input_header x = h3 [pcdata x]

let view_service unused unused2 =
  let db_selected_events_div = div [] in

  (* XXX need to implement it with an rpc that sets an Eliom_reference accordingly *)
  let logged_userid = (ref None: (Utils.application_user option ref)) in
  let selected_events_title = "Selected events" in
  let reference_event_title = "Reference event" in
  let selected_events_div = div [] in
  let selected_events_div_container = div ~a:[a_class ["container"]] [selected_events_div] in
  let reference_event_div = div [] in
  let reference_event_div_container = div ~a:[a_class ["container"]] [reference_event_div] in
  let demo_text_user_container = div ~a:[a_class ["zoomedPart"]] [] in
  let all_users_div = div ~a:[a_class ["hidden"]] [] in
  let legend_div = div [] in
  let play_demo_button = button ~a:[a_class ["hidden"; "btn"; "btn-lg"; "btn-primary"]] ~button_type:`Button [pcdata "Play demo"] in
  let stop_demo_button = button ~a:[a_class ["hidden"; "btn"; "btn-lg"; "btn-primary"]] ~button_type:`Button [pcdata "Stop demo"] in
  let select_part = [all_users_div;
                     selected_events_div_container;
                     reference_event_div_container;
                     table (tr [td [legend_div]; td [play_demo_button; stop_demo_button]]) []] in
  let url_input = string_input ~input_type:`Text () in
  let example_queries = div ~a:[a_class ["hidden"]] [pcdata "Some search query examples:";
                                                     ul (List.map (fun x -> li [pcdata x])
                                                           ["nb_invited >= 1000";
                                                            "name != <some_name>";
                                                            "owner = <some_owner>";
                                                            "(nb_declined <= 100 nb_attending > 200 owner:<some_owner>) or (location:<some_city>)"
                                                           ])]
  in
  let insert_url = string_input ~input_type:`Text () in
  let insert_button = button ~a:[a_class ["hidden"; "btn"; "btn-lg"; "btn-primary"]] ~button_type:`Button [pcdata "Record"] in
  let insert_user_container = div [] in
  let _ = {unit{
    let ui_t = Ui_events.create %url_input %db_selected_events_div
                                %all_users_div
                                %reference_event_title %reference_event_div_container %reference_event_div
                                %selected_events_title %selected_events_div_container %selected_events_div
                                %legend_div %demo_text_user_container %example_queries %logged_userid %play_demo_button %stop_demo_button
    in
    let insert_t = Insert.create %insert_url %insert_button %insert_user_container %logged_userid in
    View_events.setup (View_events.create ui_t insert_t)
  }}
  in
  let create_li elements =
    let lis = List.map (fun (el, attr) -> li ~a:attr [el]) elements in
    [ul ~a:[a_class ["nav"; "nav-list"]] lis]
  in
  let insert_part = create_li [(create_input_header "Insert new events", []);
                               (insert_url, [a_class ["active"]]);
                               (insert_button, []);
                               (insert_user_container, [])] in
  let search_without_examples = create_li [(create_input_header "Search events", []);
                                           (url_input, [a_class ["active"]]);
                                           (db_selected_events_div, [])] in
  let search_part = List.append search_without_examples [example_queries] in
  let all_body = [Utils.fb_root_div;
                  demo_text_user_container;
                  div ~a:[a_class ["container"]]
                    [div ~a:[a_class ["row"]]
                        [div ~a:[a_class ["col-sm-4"; "zoomedPart"]] [div ~a:[a_class []] (List.append insert_part search_part)];
                         div ~a:[a_class ["col-sm-8"; "zoomedPart"]] select_part]]] in
  let b = all_body in
  let utf8_meta = meta ~a:[a_charset "utf8"] () in
  let viewport_meta = meta ~a:[a_name "viewport";
                               a_content "width=device-width, initial-scale=1"] () in
  let href_link = uri_of_string (fun () -> "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css") in
  let stylesheet = link ~rel:[`Stylesheet] ~href:href_link () in
  let make_script x = script ~a:[a_src (uri_of_string (fun () -> x))] (pcdata "") in
  let js_scripts = List.map make_script ["https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js";
                                         "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"] in
  let h = [utf8_meta;
           viewport_meta;
           stylesheet] @ js_scripts in
  Lwt.return (Eliom_tools.D.html ~title: "advertise your event"
                ~css:[["css"; "mb-view.css"];
                      ["css"; "bubbles.css"]]
                (body b)
                ~other_head:h)
