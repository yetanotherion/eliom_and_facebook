{client{

type t = {
  mutable logged_user: Server_state.user option;
}

let create () = { logged_user = None }

let update_user_with_db_info user =
  match_lwt %Users.rpc_user_exists user.Server_state.user_id with
    | None -> begin
      lwt () = %Users.rpc_insert_user (Server_state.to_json user) in
      Lwt.return user
    end
    | Some x -> begin
      user.Server_state.nb_event_added <- x.Server_state.nb_event_added;
      Lwt.return user
    end

let reset_current_user_session () =
  lwt () = Utils.lwt_autologin () in
  match_lwt Utils.lwt_get_me () with
    | Fb.Nok _ | Fb.EvData _ | Fb.EvOk _ -> Lwt.fail (Failure "error in getting current profile")
    | Fb.ProfileOk res -> begin
      let user = {Server_state.user_id=res.Fb.profile_id;
                  Server_state.user_name=res.Fb.profile_name;
                  Server_state.nb_event_added=0} in
      lwt user = update_user_with_db_info user in
      lwt () = %Server_state.rpc_set_user_of_the_session (Server_state.to_json user) in
      Lwt.return user
    end

let get_user_session_id () =
  lwt user_ref = %Server_state.rpc_get_user_of_the_session () in
  match user_ref with
    | None -> reset_current_user_session ()
    | Some x -> Lwt.return x

let setup_user_in_session t =
  lwt user = get_user_session_id () in

  let () = t.logged_user <- Some user in
  Lwt.return_unit

let make_logged_f f t url =
  try f url
  with _ -> begin
    Utils.log "resetting user";
    lwt user = reset_current_user_session () in
    lwt () = setup_user_in_session t in
    f url
 end

let logged_lwt_api = make_logged_f Utils.lwt_api
let logged_process_all_rsvp = make_logged_f Utils.process_all_rsvp

}}
