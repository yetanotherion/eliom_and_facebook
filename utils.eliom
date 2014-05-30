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

  let handle_correct_api_res api_res new_span =
    [li [pcdata (Printf.sprintf "Name: %s" api_res.Fb.name)];
     li [pcdata (Printf.sprintf "Owner: %s" api_res.Fb.owner.Fb.name)];
     li [pcdata (Printf.sprintf "Location: %s" api_res.Fb.venue.Fb.city)];
     li [pcdata (Printf.sprintf "Start_time: %s" api_res.Fb.start_time)];
     new_span]

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

  let process_rsvp url attending =
    let attending_url = url ^ "/" ^ attending in
    gather_all_users attending_url []

  let process_event_answer url button_dom dynamic_param_board to_insert res =
    lwt to_replace =
      match res with
        | Fb.Nok error -> begin
          Lwt.return ([div [pcdata (Printf.sprintf "Invalid event, got error {message:%s, error_type: %s, code: %d}" error.Fb.message error.Fb.error_type error.Fb.code)]])
        end
        | Fb.Data _ -> begin
          Lwt.return ([div [pcdata (Printf.sprintf "Invalid event, got data instead of event elements")]])
        end
        | Fb.Ok api_res -> begin
          let new_span  = span [pcdata ("wait as we gather attending users...")] in
          let to_replace = handle_correct_api_res api_res new_span in
          Html5.Manip.replaceChildren dynamic_param_board to_replace;
          lwt attending = process_rsvp url "attending" in
          lwt declined = process_rsvp url "declined" in
          lwt invited = try_lwt process_rsvp url "invited" with _ -> Lwt.return [] in
          show_button button_dom;
          to_insert := Some (url, api_res.Fb.venue.Fb.city, api_res.Fb.start_time);
          let res = [
            li [pcdata (Printf.sprintf "#Attending:: %d" (List.length attending))];
            li [pcdata (Printf.sprintf "#Declined:: %d" (List.length declined))];
            li [pcdata (Printf.sprintf "#Invited:: %s" (match invited with | [] -> "Too many" | l -> Printf.sprintf "%d" (List.length invited)))]
          ] in
          Html5.Manip.replaceChildren new_span res;
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
