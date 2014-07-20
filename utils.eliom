{shared{
  open Eliom_content
  open Html5.D
  open Eliom_parameter
  type event = {
    url: string;
    location: string;
    start_date: Int32.t;
    owner: string;
    name: string
  }

  type event_user = String.t * String.t
  module RsvpSet = Set.Make (
    struct
      type t = event_user
      let compare = Pervasives.compare
    end)

  let to_epoch date =
    let open CalendarLib in
        let curr_date =
          try
            Calendar.Precise.to_unixfloat (Printer.Precise_Calendar.from_fstring "%Y-%m-%dT%H:%M:%S%z" date)
          with (Invalid_argument _) -> Date.to_unixfloat (Printer.Date.from_fstring "%Y-%m-%d" date)
        in
        Int32.of_float curr_date

  let epoch_to_fmt_date fmt date =
    let open CalendarLib in
        let date_in_float = Int32.to_float date in
        let t = Unix.gmtime date_in_float in
        let curr_time = Calendar.Precise.from_gmt (Calendar.Precise.from_unixtm t) in
        Printer.Precise_Calendar.sprint fmt curr_time

  let epoch_to_tz_date = epoch_to_fmt_date "%Y-%m-%dT%H:%M:%S%z"
  let epoch_to_light_date = epoch_to_fmt_date "%Y-%m-%d %H:%M"
}}

let fb_root_div = div ~a:[a_id "fb-root"] []
let utf8_meta = meta ~a:[a_charset "utf8"] ()
let viewport_meta = meta ~a:[a_name "viewport";
                             a_content "width=device-width, initial-scale=1"] ()
let bootstrap_metas = [utf8_meta;
                       viewport_meta;
                       meta ~a:[a_http_equiv "X-UA-Compatible";
                                 a_content "IE=edge"] ()]

{client{
  let make_rsvp_set rsvp_list =
    List.fold_left (fun s elt -> RsvpSet.add elt s) RsvpSet.empty rsvp_list

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
               td [pcdata (epoch_to_light_date (to_epoch event.Fb.start_time))]]
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

  let process_rsvp url users_name =
   lwt all_users = gather_all_users (url ^ "/" ^ users_name) [] in
   Lwt.return all_users

  let process_all_rsvp url =
    lwt attending = process_rsvp url "attending" in
    lwt declined = process_rsvp url "declined" in
    lwt invited = try_lwt process_rsvp url "invited" with _ -> Lwt.return [] in
    Lwt.return (attending, declined, invited)

  let display_rsvp users wait_span =
      Html5.Manip.replaceChildren wait_span [pcdata (Printf.sprintf "%d" (List.length users))]

  let display_all_rsvp rsvp_info_display =
    List.iter (fun (rsvp_info, display) -> display_rsvp rsvp_info display) rsvp_info_display

  let make_tds data =
    List.map (fun y -> td [y]) data

  type one_span = [ | `Span] elt

  let make_wait_span () =
    span [pcdata "please wait as we gather data..."]

  type spans = {
    event_name_span: one_span;
    event_owner_span: one_span;
    event_venue_span: one_span;
    event_start_time_span: one_span;
    attending_span: one_span;
    declined_span: one_span;
    invited_span: one_span;
  }

  let create_spans () = {
    event_name_span = make_wait_span ();
    event_owner_span = make_wait_span ();
    event_venue_span = make_wait_span ();
    event_start_time_span = make_wait_span ();
    attending_span = make_wait_span ();
    declined_span = make_wait_span ();
    invited_span = make_wait_span ();
  }

  let integrate_spans_in_td spans =
    make_tds [spans.event_name_span;
              spans.event_owner_span;
              spans.event_venue_span;
              spans.event_start_time_span;
              spans.attending_span;
              spans.declined_span;
              spans.invited_span]

  let make_table columns_name trs =
    let curr_tbody = tbody trs in
    let head_columns = tr (List.map (fun x -> th [pcdata x]) columns_name) in
    let curr_thead = thead [head_columns] in
    let curr_table = tablex ~thead:curr_thead ~a:[a_class ["table"; "table-striped"]] [curr_tbody] in
    div ~a:[a_class ["table-responsive"]] [curr_table]

  let make_complete_event_table data = make_table ["Name"; "Owner";
                                                   "Location"; "Start_time";
                                                   "attending"; "declined";
                                                   "invited"] data

  let process_event_answer url res =
      match res with
        | Fb.Nok error -> begin
          `Err [div [pcdata (Printf.sprintf "Invalid event, got error {message:%s, error_type: %s, code: %d}" error.Fb.message error.Fb.error_type error.Fb.code)]]
        end
        | Fb.Data _ -> begin
          `Err [div [pcdata (Printf.sprintf "Invalid event, got data instead of event elements")]]
        end
        | Fb.Ok event -> `Ok event

  let make_event_display_line event attending_span declined_span invited_span =
   make_tds [pcdata event.Fb.name;
             pcdata event.Fb.owner.Fb.name;
             pcdata event.Fb.venue.Fb.city;
             pcdata (epoch_to_light_date (to_epoch event.Fb.start_time));
             attending_span;
             declined_span;
             invited_span]

  let replace_event_spans event spans =
    List.iter (fun (span, value) ->
      Html5.Manip.replaceChildren span [pcdata value])
      [(spans.event_name_span, event.Fb.name);
       (spans.event_owner_span, event.Fb.owner.Fb.name);
       (spans.event_venue_span, event.Fb.venue.Fb.city);
       (spans.event_start_time_span, event.Fb.start_time)]

  let print_event event =
    [td [pcdata event.name];
     td [pcdata event.owner];
     td [pcdata event.location];
     td [pcdata (epoch_to_light_date event.start_date)]]
}}

let icon_png_link size = uri_of_string (fun () -> Printf.sprintf "ico/apple-touch-icon-%d-precomposed.png" size)

let apple_touch_icon_link size =
    let href_link = icon_png_link size in
    link ~rel:[`Other "apple-touch-icon-precomposed"] ~a:[a_sizes [size; size]] ~href:href_link ()

let touch_icons = List.map apple_touch_icon_link [144; 114; 72]

let bs_icons = touch_icons @ [link ~rel:[`Other "apple-touch-icon-precomposed"] ~href:(icon_png_link 57) ();
                             link ~rel:[`Other "shortcut icon"; `Icon] ~href:(uri_of_string (fun () -> "ico/favicon.png")) ()]

let js_scripts = ["jquery.js";
                  "bootstrap-transition.js";
                  "bootstrap-alert.js";
                  "bootstrap-modal.js";
                  "bootstrap-dropdown.js";
                  "bootstrap-scrollspy.js";
                  "bootstrap-tab.js";
                  "bootstrap-tooltip.js";
                  "bootstrap-popover.js";
                  "bootstrap-button.js";
                  "bootstrap-collapse.js";
                  "bootstrap-carousel.js";
                  "bootstrap-typeahead.js"]
let bs_scripts = List.map (fun x -> script ~a:[a_src (uri_of_string (fun () -> Printf.sprintf "js/%s" x))] (pcdata ""))  js_scripts
