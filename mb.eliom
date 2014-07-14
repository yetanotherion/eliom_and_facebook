{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

module Mb_app =
  Eliom_registration.App (
    struct
      let application_name = "mb"
    end)


let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let setup_services () =
  Mb_app.register_service ["insert_fb_event"] Eliom_parameter.unit Insert.insert_service,
  Mb_app.register_service ["promote_fb_event"] Eliom_parameter.unit View.view_service

let () =
  let insert, view = setup_services () in
  let home_style = "body {
        padding-top: 60px; /* 60px to make the container go all the way to the bottom of the topbar */
      }" in
  let icon_png_link size = uri_of_string (fun () -> Printf.sprintf "ico/apple-touch-icon-%d-precomposed.png" size)
  in
  let apple_touch_icon_link size =
    let href_link = icon_png_link size in
    link ~rel:[`Other "apple-touch-icon-precomposed"] ~a:[a_sizes [size; size]] ~href:href_link ()
  in
  let touch_icons = List.map apple_touch_icon_link [144; 114; 72] in
  let icons = touch_icons @ [link ~rel:[`Other "apple-touch-icon-precomposed"] ~href:(icon_png_link 57) ();
                             link ~rel:[`Other "shortcut icon"; `Icon] ~href:(uri_of_string (fun () -> "ico/favicon.png")) ()] in
  let js_scripts = ["jquery.js";
                    "bootstrap-transition.js";
                    "bootstrap-alert.js";
                    "bootstrap-modal.js";
                    "bootstrap-dropdown.js";
                    "bootstrap-scrollspy.js";
                    "bootstrap-tab.js";
                    "bootstrap-tooltip.js";
                    "bootstrap-popover.js";
                    "bootstrap-button.js";
                    "bootstrap-collapse.js";
                    "bootstrap-carousel.js";
                    "bootstrap-typeahead.js"] in
  let scripts = List.map (fun x -> script ~a:[a_src (uri_of_string (fun () -> Printf.sprintf "js/%s" x))] (pcdata ""))  js_scripts in
  let collapse_all = div ~a:[a_class ["nav-collapse"; "collapse"]]
    [ul ~a:[a_class ["nav"]]
        [li ~a:[a_class ["active"]] [a ~service:insert [pcdata "Add events"] ()];
         li [a ~service:view [pcdata "Manage custom audiences"] ()]]
    ] in
  let all_body = div ~a:[a_class ["navbar"; "navbar-inverse"; "navbar-fixed-top"]]
    [div ~a:[a_class ["navbar-inner"]]
        [div ~a:[a_class ["container"]]
            [button ~a:[a_class ["btn"; "btn-navbar"]] ~button_type:`Button (*data-toggle="collapse" data-target=".nav-collapse"*)
                [span ~a:[a_class ["icon-bar"]] [];
                 span ~a:[a_class ["icon-bar"]] [];
                 span ~a:[a_class ["icon-bar"]] []];
             a ~service:main_service ~a:[a_class ["brand"]] [] ();
             collapse_all]];
     div ~a:[a_class ["container"]]
                [h1 [pcdata "Improve your custom audiences"];
                 p [pcdata "Tune your facebook custom audiences with Facebook event attenders."]]] in
  Mb_app.register
    ~service:main_service
    (fun () () ->
      let page = Eliom_tools.D.html ~title:"Manage band"
        ~css:[["css"; "bootstrap.css"];
              ["css"; "bootstrap-responsive.css"]]
        (body ([all_body] @ scripts))
        ~other_head:([Utils.utf8_meta;
                      Utils.viewport_meta;
                  (*meta ~a:[a_description "home"] ();
                    meta ~a:[a_author "Ion Alberdi"] ();*)
                      style [pcdata home_style]] @ icons)
      in
      Lwt.return page)
