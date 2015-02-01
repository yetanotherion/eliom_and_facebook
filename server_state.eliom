{shared{

open Eliom_content

 type user = {
    user_id: string;
    user_name: string;
    (* XXX deriving cannot have mutable attributes *)
    mutable nb_event_added: int;
 }


 type jsonable_user = {
   j_user_id: string;
   j_user_name: string;
   j_nb_event_added: int;
 }
 deriving(Json)

let make_jsonable_user user_id username = {
  j_user_id = user_id;
  j_user_name = username;
  j_nb_event_added = 0;
}

let to_json u =
  {j_user_id = u.user_id;
   j_user_name = u.user_name;
   j_nb_event_added = u.nb_event_added}

let from_json u =
  {user_id = u.j_user_id;
   user_name = u.j_user_name;
   nb_event_added = u.j_nb_event_added}

}}

{server{
let user_of_the_session = Eliom_reference.eref ~scope:Eliom_common.default_session_scope ~secure:false None

let get_user_of_the_session () =
  Eliom_reference.get user_of_the_session

let set_user_of_the_session user =
  Eliom_reference.set user_of_the_session (Some (from_json user))

let rpc_get_user_of_the_session =
  server_function Json.t<unit> get_user_of_the_session

let rpc_set_user_of_the_session =
  server_function Json.t<jsonable_user> set_user_of_the_session
}}
