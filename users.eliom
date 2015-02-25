(**************************************************************************)
(*  Copyright 2014, Ion Alberdi <nolaridebi at gmail.com>                 *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)
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
