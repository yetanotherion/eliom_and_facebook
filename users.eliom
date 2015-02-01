{shared{
  open Eliom_content
  open Html5.D
  open Eliom_parameter

}}

{server{

let my_insert_user user =
  (* we make the conversion now *)
  Db.insert_user (Server_state.from_json user)

let update_user_nb_event arg =
  let id, nb = arg in
  Db.update_user_nb_event id nb

let rpc_insert_user =
  server_function Json.t<Server_state.jsonable_user> my_insert_user

let rpc_user_exists =
  server_function Json.t<string> Db.get_user

let rpc_user_update_nb_event =
  server_function Json.t<string * int> update_user_nb_event

}}
