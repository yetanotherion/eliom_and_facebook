{shared{
  open Eliom_content
  open Html5.D
  open Eliom_parameter

  type event = {
    url: string;
    location: string;
    start_date: Int32.t;
    owner: string;
    name: string;
    nb_attending: int;
    nb_declined: int;
    nb_invited: int;
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

  let make_table ?caption:(table_caption=None) ?additional_class:(add_cl=[]) columns_name trs =
    let curr_tbody = tbody trs in
    let head_columns = tr (List.map (fun x -> th [pcdata x]) columns_name) in
    let curr_thead = thead [head_columns] in
    let curr_table =
      match table_caption with
        | None -> tablex ~thead:curr_thead ~a:[a_class ["table"; "table-striped"]] [curr_tbody]
        | Some x -> tablex ~caption:(caption [h3 [pcdata x]]) ~thead:curr_thead ~a:[a_class ["table"; "table-striped"]] [curr_tbody]
    in
    curr_table

  let make_insert_event_table data =
    make_table ~caption:None ["Name"; "Owner";
                              "Location"; "Start_time";
                              "rsvp_info"] data
  let cross_size = [a_height 20; a_width 20]
  let make_cross () =
    img ~src:(uri_of_string (fun () -> "imgs/cross.svg"))
      ~a:cross_size
      ~alt:"close" ()

  let make_cross_hover () =
    img ~src:(uri_of_string (fun () -> "imgs/cross_hover.svg"))
      ~a:cross_size
      ~alt:"close" ()


  let make_complete_event_table ?caption:(c=None) data =
    make_table ~caption:c ["Name"; "Owner";
                           "Location"; "Start_time";
                           "attending"; "declined";
                           "invited"] data

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

  let log s = Firebug.console##log(Js.string s)

  let hidde_element elt =
    if not (Js.to_bool (elt##classList##contains(Js.string "hidden")))
    then elt##classList##add(Js.string "hidden")

  let show_element elt =
    if (Js.to_bool (elt##classList##contains(Js.string "hidden")))
    then elt##classList##remove(Js.string "hidden")

  let show_html_element elt =
    show_element (Html5.To_dom.of_element elt)

  let hidde_html_element elt =
    hidde_element (Html5.To_dom.of_element elt)

  let set_element_as_transparent elt = Html5.Manip.SetCss.opacity elt (Some "0.0")

  let get_element_id element =
    Js.to_string (Js.Opt.get (element##getAttribute (Js.string "id")) (fun () -> assert false))

  let getBoundingClientRect element = element##getBoundingClientRect()

(* XXX
http://javascript.info/tutorial/coordinates
function getOffsetRect(elem) {
  // (1)
  var box = elem.getBoundingClientRect()
  var body = document.body
  var docElem = document.documentElement
  // (2)
  var scrollTop = window.pageYOffset || docElem.scrollTop || body.scrollTop
  var scrollLeft = window.pageXOffset || docElem.scrollLeft || body.scrollLeft

  // (3)
  var clientTop = docElem.clientTop || body.clientTop || 0
  var clientLeft = docElem.clientLeft || body.clientLeft || 0

  // (4)

  var top  = box.top +  scrollTop - clientTop
  var left = box.left + scrollLeft - clientLeft
  return { top: Math.round(top), left: Math.round(left) }
}

*)
  let getBoundingClientRectCoordinates element =
    let html_top = Dom_html.document##documentElement##scrollTop in
    let html_left = Dom_html.document##documentElement##scrollLeft in
    let body_top = Dom_html.document##body##scrollTop in
    let body_left = Dom_html.document##body##scrollLeft in
    let () = log (Printf.sprintf "html_top: %d, html_left: %d body_top:%d body_left: %d"
           html_top html_left body_top body_left) in
    let box = getBoundingClientRect element in
    box##top, box##left, box##right, box##bottom

  let make_rsvp_set rsvp_list =
    List.fold_left (fun s elt -> RsvpSet.add elt s) RsvpSet.empty rsvp_list

  let extract_user user = (user.Fb.user_id, user.Fb.name)

  type event_and_users = {
    ev_url: string;
    ev_data: Fb.correct_event_res;
    attending: event_user list;
    declined: event_user list;
    invited: event_user list;
  }

  let make_event_and_users event_url event attending declined invited =
    {
      ev_url = event_url;
      ev_data = event;
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

  let lwt_get_me () =
    let url = "v2.0/me" in
    let api, fwakener = Lwt.wait () in
    let f res =
      Lwt.wakeup fwakener res
    in
    let () = Fb.api url f in
    api

  let lwt_autologin () =
    match_lwt (lwt_shouldlogin ()) with
      | true -> begin
              lwt _ = lwt_login () in
              Lwt.return_unit
        end
      | false -> Lwt.return_unit

  let lwt_api url =
    let url = "v2.0/" ^ url in
    let api, fwakener = Lwt.wait () in
    let f res =
      Lwt.wakeup fwakener res
    in
    Fb.api url f;
    api

  let get_city event =
    match event.Fb.venue with | None -> "" | Some x -> x.Fb.city

  let print_event event wait_msg_user_containers =
    let tds = [td [pcdata event.Fb.name];
               td [pcdata event.Fb.owner.Fb.name];
               td [pcdata (get_city event)];
               td [pcdata (epoch_to_light_date (to_epoch event.Fb.start_time))]]
              @ (List.map (fun x -> td [x]) wait_msg_user_containers) in
    tr tds

  let substring_after_char string c =
    let next_char_idx = (String.index string c) + 1 in
    String.sub string next_char_idx ((String.length string) - next_char_idx)

  let error_to_str error =
    Printf.sprintf "Invalid event, got error {message:%s, error_type: %s, code: %d}" error.Fb.message error.Fb.error_type error.Fb.code

  let rec gather_all_users url res =
    match_lwt (lwt_api url) with
      | Fb.Nok error ->  begin raise_lwt (Failure ("got error: " ^ (error_to_str error)))
      end
      | Fb.ProfileOk _ | Fb.EvOk _ -> begin raise_lwt (Failure "unexpected answer") end
      | Fb.EvData (d, cursor) ->
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

  let display_rsvp label users user_container =
      Html5.Manip.replaceChildren user_container [pcdata (Printf.sprintf "%s: %d" label (List.length users))]

  let display_all_rsvp rsvp_info_display =
    List.iter (fun (label, rsvp_info, user_container) -> display_rsvp label rsvp_info user_container) rsvp_info_display

  let make_tds data =
    List.map (fun y -> td [y]) data

  type one_user_container = [ | `Div] elt

  let make_wait_user_container ?label:(l="") () =
    div [pcdata (l ^ "please wait as we gather data...")]

  type user_containers = {
    event_name_user_container: one_user_container;
    event_owner_user_container: one_user_container;
    event_venue_user_container: one_user_container;
    event_start_time_user_container: one_user_container;
    attending_user_container: one_user_container;
    declined_user_container: one_user_container;
    invited_user_container: one_user_container;
  }

  let create_user_containers () = {
    event_name_user_container = make_wait_user_container ();
    event_owner_user_container = make_wait_user_container ();
    event_venue_user_container = make_wait_user_container ();
    event_start_time_user_container = make_wait_user_container ();
    attending_user_container = make_wait_user_container ();
    declined_user_container = make_wait_user_container ();
    invited_user_container = make_wait_user_container ();
  }

  let integrate_user_containers_in_td user_containers cross =
    make_tds [user_containers.event_name_user_container;
              user_containers.event_owner_user_container;
              user_containers.event_venue_user_container;
              user_containers.event_start_time_user_container;
              user_containers.attending_user_container;
              user_containers.declined_user_container;
              user_containers.invited_user_container;
              cross]



  let process_event_answer url res =
      match res with
        | Fb.Nok error -> begin
          `Err [div [pcdata (error_to_str error)]]
        end
        | Fb.ProfileOk _ -> begin
          `Err [div [pcdata "Invalid event, got answer related to a profile"]]
        end
        | Fb.EvData _ -> begin
          `Err [div [pcdata "Invalid event, got data instead of event elements"]]
        end
        | Fb.EvOk event -> `Ok event

  let replace_event_user_containers event user_containers =
    List.iter (fun (user_container, value) ->
      Html5.Manip.replaceChildren user_container [pcdata value])
      [(user_containers.event_name_user_container, event.name);
       (user_containers.event_owner_user_container, event.owner);
       (user_containers.event_venue_user_container, event.location);
       (user_containers.event_start_time_user_container, (epoch_to_light_date event.start_date))]

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
