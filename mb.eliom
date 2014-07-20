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
  Mb_app.register_service ["add_events"] Eliom_parameter.unit Insert.insert_service,
  Mb_app.register_service ["manage_audience"] Eliom_parameter.unit View.view_service

let () =
  let insert, view = setup_services () in
  let links = ul ~a:[a_class ["nav"]]
    [li ~a:[a_class ["active"]] [a ~service:view [pcdata "Manage your audience"] ()];
     li [a ~service:insert [pcdata "Manage pool of events"] ()]]
  in
  let all_body = div ~a:[a_class ["container"]]
    [div ~a:[a_class ["masthead"]]
        [div ~a:[a_class ["navbar"]]
           [div ~a:[a_class ["navbar-inner"]]
               [div ~a:[a_class ["container"]]
                   [links]]]];
     div ~a:[a_class ["jumbotron"]]
                [h1 [pcdata "Improve custom audiences"];
                 p ~a:[a_class ["lead"]] [pcdata "Improve your facebook custom audiences with Facebook event attendees."]]] in
  Mb_app.register
    ~service:main_service
    (fun () () ->
      let page = Eliom_tools.D.html ~title:"Manage band"
        ~css:[["css"; "bootstrap.css"];
              ["css"; "bootstrap-responsive.css"];
              ["css"; "mb-home.css"]]
        (body ([all_body] @ Utils.bs_scripts))
        ~other_head:([Utils.utf8_meta;
                      Utils.viewport_meta] @ Utils.bs_icons)
      in
      Lwt.return page)
