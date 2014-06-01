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
  ignore(Mb_app.register_service ["insert_fb_event"] Eliom_parameter.unit Insert.insert_service);
  ignore(Mb_app.register_service ["promote_fb_event"] Eliom_parameter.unit View.view_service)

let () =
  let () = setup_services () in
  let my_head = (head (title (pcdata "Manage band")) []) in
  let my_body = (body [Utils.fb_root_div]) in
  Mb_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
	(html my_head my_body))
