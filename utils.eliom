{shared{
  open Eliom_content
  open Html5.D
  open Eliom_parameter
  type event = {
    url: string;
    location: string;
    start_date: Int32.t;
  }

}}

let fb_root_div = div ~a:[a_id "fb-root"] []
let bootstrap_metas = [meta ~a:[a_charset "utf8"] ();
                       meta ~a:[a_http_equiv "X-UA-Compatible";
                                 a_content "IE=edge"] ();
                       meta ~a:[a_name "viewport";
                                a_content "width=device-width, initial-scale=1"] () ]
{client{
  type event_user = String.t * String.t
  let extract_user user = (user.Fb.user_id, user.Fb.name)

  type event_and_users = {
    ev_data: Fb.correct_event_res;
    attending: event_user list;
    declined: event_user list;
    invited: event_user list;
  }

  let make_event_and_users event attending declined invited =
    { ev_data = event;
      attending = attending;
      declined = declined;
      invited = invited;
    }

  module Html = Dom_html
  let offset = (jsnew Js.date_now ())##getTimezoneOffset()
  let () = CalendarLib.Time_Zone.change (CalendarLib.Time_Zone.UTC_Plus ((-offset) / 60))
  let jssdk = Js.string "facebook-jssdk"
  let lwtInstallSdkAlreadyCalled = ref false

  let installSdk () =
    let fjs = List.hd (Dom.list_of_nodeList Html.window##document##getElementsByTagName(Js.string "script")) in
    let js = Html.createScript Html.window##document in
    js##src <- Js.string "connect.facebook.net/en_US/all.js";
    js##id <- jssdk;
    let parentFjs = Js.Opt.get (fjs##parentNode) (fun () -> assert false) in
    Dom.insertBefore parentFjs js (Js.some fjs);
    fjs

  let init () =
    Fb.init { Fb.appId = "534442833338989";
              Fb.cookie = true;
	      Fb.xfbml = false;
              Fb.version = "v2.0"
            }

  let lwt_installSdk () =
    ignore(Js.Opt.get (Html.window##document##getElementById(jssdk))
	   installSdk);
    let window = (Js.Unsafe.coerce Html.window) in
    if !lwtInstallSdkAlreadyCalled then Lwt.return ()
    else begin
      lwtInstallSdkAlreadyCalled := true;
      let fbasyncInitWaiter, fbasyncInitWakener = Lwt.wait () in
      let f () =
        init ();
        Lwt.wakeup fbasyncInitWakener ()
      in
      window##fbAsyncInit <- f;
      fbasyncInitWaiter
    end

  let lwt_login () =
    let flogin, fwakener = Lwt.wait () in
    Fb.login (fun r -> Lwt.wakeup fwakener r);
    flogin

  let lwt_shouldlogin () =
    let flogin, fwakener = Lwt.wait () in
    let f res =
      match res with
	| Fb.LoggedInFbAndApp -> Lwt.wakeup fwakener false
	| Fb.LoggedInFbOnly | Fb.NotLoggedInFb -> Lwt.wakeup fwakener true
    in
    lwt () = lwt_installSdk () in
    Fb.getLoginStatus f;
    flogin

  let lwt_autologin () =
    match_lwt (lwt_shouldlogin ()) with
      | true -> begin
              lwt _ = lwt_login () in
              Lwt.return_unit
        end
      | false -> Lwt.return_unit

  let lwt_api_event url =
    let url = "v2.0/" ^ url in
    let api, fwakener = Lwt.wait () in
    let f res =
      Lwt.wakeup fwakener res
    in
    lwt () = lwt_autologin () in
    Fb.api_event url f;
    api

  let print_event event wait_msg_spans =
    let tds = [td [pcdata event.Fb.name];
               td [pcdata event.Fb.owner.Fb.name];
               td [pcdata event.Fb.venue.Fb.city];
               td [pcdata event.Fb.start_time]]
              @ (List.map (fun x -> td [x]) wait_msg_spans) in
    tr tds

  let hidde_button elt =
    if not (Js.to_bool (elt##classList##contains(Js.string "hidden")))
    then elt##classList##add(Js.string "hidden")

  let show_button elt =
    if (Js.to_bool (elt##classList##contains(Js.string "hidden")))
    then elt##classList##remove(Js.string "hidden")

  let substring_after_char string c =
    let next_char_idx = (String.index string c) + 1 in
    String.sub string next_char_idx ((String.length string) - next_char_idx)

  let rec gather_all_users url res =
    match_lwt (lwt_api_event url) with
      | Fb.Nok error -> begin
        raise_lwt (Failure "error")
      end
      | Fb.Ok _ -> begin
        raise_lwt (Failure "unexpected")
      end
      | Fb.Data (d, cursor) ->
        let current_res = List.map extract_user (Array.to_list d) in
        let new_res = res @ current_res in
        match cursor.Fb.next with
          | None -> Lwt.return new_res
          | Some x ->
            let next_param = substring_after_char x '?' in
            gather_all_users (Printf.sprintf "%s?%s" url next_param) new_res

  let process_rsvp url users_name wait_span =
    lwt all_users = gather_all_users (url ^ "/" ^ users_name) [] in
    Html5.Manip.replaceChildren wait_span [pcdata (Printf.sprintf "%d" (List.length all_users))];
    Lwt.return all_users

  let make_table columns_name data =
    let trs = List.map (fun x -> tr (List.map (fun y -> td [y]) x)) data in
    let curr_tbody = tbody trs in
    let head_columns = tr (List.map (fun x -> th [pcdata x]) columns_name) in
    let curr_thead = thead [head_columns] in
    let curr_table = tablex ~thead:curr_thead ~a:[a_class ["table"; "table-striped"]] [curr_tbody] in
    div ~a:[a_class ["table-responsive"]] [curr_table]

  let make_complete_event_table data = make_table ["Name"; "Owner";
                                                   "Location"; "Start_time";
                                                   "attending"; "declined";
                                                   "invited"] data

  let process_event_answer url res dynamic_param_board =
    lwt to_replace, result =
      match res with
        | Fb.Nok error -> begin
          Lwt.return ([div [pcdata (Printf.sprintf "Invalid event, got error {message:%s, error_type: %s, code: %d}" error.Fb.message error.Fb.error_type error.Fb.code)]], None)
        end
        | Fb.Data _ -> begin
          Lwt.return ([div [pcdata (Printf.sprintf "Invalid event, got data instead of event elements")]], None)
        end
        | Fb.Ok event -> begin
          let make_span () =
            span [pcdata "please wait as we gather data..."]
          in
          let attending_span = make_span () in
          let declined_span = make_span () in
          let invited_span = make_span () in
          let data = [[pcdata event.Fb.name;
                       pcdata event.Fb.owner.Fb.name;
                       pcdata event.Fb.venue.Fb.city;
                       pcdata event.Fb.start_time;
                       attending_span;
                       declined_span;
                       invited_span]] in
          let to_replace = [make_complete_event_table data] in
          Html5.Manip.replaceChildren dynamic_param_board to_replace;
          lwt attending = process_rsvp url "attending" attending_span in
          lwt declined = process_rsvp url "declined" declined_span in
          lwt invited = try_lwt process_rsvp url "invited" invited_span with _ -> Lwt.return [] in
          Lwt.return (to_replace, (Some (make_event_and_users event attending declined invited)))
        end
    in
    Html5.Manip.replaceChildren dynamic_param_board to_replace;
    Lwt.return result

  let epoch_to_tz_date date =
    let open CalendarLib in
        let date_in_float = Int32.to_float date in
        let t = Unix.gmtime date_in_float in
        let curr_time = Calendar.Precise.from_gmt (Calendar.Precise.from_unixtm t) in
        Printer.Precise_Calendar.sprint "%Y-%m-%dT%H:%M:%S%z" curr_time

  let event_to_string event =
    Printf.sprintf "url: %s, location: %s, start_date:%s" event.url event.location (epoch_to_tz_date event.start_date)
}}

let to_epoch date =
  let open CalendarLib in
      let curr_date =
        try
          Calendar.Precise.to_unixfloat (Printer.Precise_Calendar.from_fstring "%Y-%m-%dT%H:%M:%S%z" date)
        with (Invalid_argument _) -> Date.to_unixfloat (Printer.Date.from_fstring "%Y-%m-%d" date)
      in
      Int32.of_float curr_date
