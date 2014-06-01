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

let fb_root_div = div ~a:[a_id "fb_root"] []

{client{
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
    let tds = [td [pcdata (Printf.sprintf "Name: %s" event.Fb.name)];
               td [pcdata (Printf.sprintf "Owner: %s" event.Fb.owner.Fb.name)];
               td [pcdata (Printf.sprintf "Location: %s" event.Fb.venue.Fb.city)];
               td [pcdata (Printf.sprintf "Start_time: %s" event.Fb.start_time)]]
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
        let current_res = List.map (fun x -> (x.Fb.user_id, x.Fb.name)) (Array.to_list d) in
        let new_res = res @ current_res in
        match cursor.Fb.next with
          | None -> Lwt.return new_res
          | Some x ->
            let next_param = substring_after_char x '?' in
            gather_all_users (Printf.sprintf "%s?%s" url next_param) new_res

  let process_rsvp url users_name wait_span =
    lwt all_users = gather_all_users (url ^ "/" ^ users_name) [] in
    Html5.Manip.replaceChildren wait_span [pcdata (Printf.sprintf "#%s:: %d" users_name (List.length all_users))];
    Lwt.return all_users

  let process_event_answer url res dynamic_param_board ~button ~to_insert =
    lwt to_replace =
      match res with
        | Fb.Nok error -> begin
          Lwt.return ([div [pcdata (Printf.sprintf "Invalid event, got error {message:%s, error_type: %s, code: %d}" error.Fb.message error.Fb.error_type error.Fb.code)]])
        end
        | Fb.Data _ -> begin
          Lwt.return ([div [pcdata (Printf.sprintf "Invalid event, got data instead of event elements")]])
        end
        | Fb.Ok api_res -> begin
          let make_span name =
            span [pcdata (Printf.sprintf "wait as we gather %s users..." name)]
          in
          let attending_span = make_span "attending" in
          let declined_span = make_span "declined" in
          let invited_span = make_span "invited" in
          let to_replace = [table (print_event api_res [attending_span; declined_span; invited_span]) []] in
          Html5.Manip.replaceChildren dynamic_param_board to_replace;
          lwt attending = process_rsvp url "attending" attending_span in
          lwt declined = process_rsvp url "declined" declined_span in
          lwt invited = try_lwt process_rsvp url "invited" invited_span with _ -> Lwt.return [] in
          let () = match button with | Some b -> show_button b | None -> () in
          let () = match to_insert with | Some x -> x := Some (url, api_res.Fb.venue.Fb.city, api_res.Fb.start_time) | None -> () in
          Lwt.return to_replace
        end
    in
    Html5.Manip.replaceChildren dynamic_param_board to_replace;
    Lwt.return_unit

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
