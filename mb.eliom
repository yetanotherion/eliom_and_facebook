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

let () =
  ignore(Mb_app.register_service ["inserttest"] Eliom_parameter.unit Insert.insert_service);
  let make_div att = div ~a:[a_id att] [] in
  let my_head = (head (title (pcdata "Hello World of Ocsigen")) []) in
  let all_divs = [make_div "fb-root"] in
  let my_body = (body all_divs) in
  Mb_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
	(html my_head my_body))
