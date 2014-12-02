{client{
open Eliom_content
open Html5.D
type 'a one_move = ('a Eliom_content.Html5.elt * Animation.move list list)

type 'a move_state = [
| `NoAnimation
| `WaitForStart of int
| `DuringMove of 'a one_move
| `WaitForEnd of int
| `WaitForMoveEnd
| `End ]

(* the type checker was lost without that *)
type 'a dom_type = ([> `Img | `PCDATA ] as 'a)

module type MakeMoveType = sig
  type additional_args
  val create_additional_args: unit -> additional_args
  val compute_move: 'a dom_type Ui_events.ui_events -> additional_args -> 'a dom_type one_move option
  val handle_move_end: 'a dom_type Ui_events.ui_events -> additional_args -> 'a dom_type Eliom_content.Html5.elt -> unit
  val handle_wait_end: 'a dom_type Ui_events.ui_events -> additional_args -> unit
  val finished: 'a dom_type Ui_events.ui_events -> additional_args -> bool
  val start_wait: int
  val end_wait : int
end

module MakeMove (M:MakeMoveType) = struct
  type 'a t = { mutable state: 'a move_state;
                additional_args: M.additional_args;
                text: string;
                style: string;
              }
  let create text style = { state = `NoAnimation;
                            additional_args = M.create_additional_args ();
                            text = text;
                            style = style }
  let next_move t ui_t =
    let open Ui_events in
    match t.state with
      | `NoAnimation -> t.state <- `WaitForStart 0
      | `WaitForStart x -> begin
        if x < M.start_wait then t.state <- `WaitForStart (x + 1)
        else
          let () = Ui_events.set_demo_text ui_t (Some (t.text, t.style)) in
          match M.compute_move ui_t t.additional_args with
            | None -> ()
            | Some m -> t.state <- `DuringMove m
      end
      | `DuringMove (x, l) -> begin
        match l with
          | hd :: tl -> begin
            Animation.do_move x hd;
            t.state <- `DuringMove (x, tl)
          end
          | [] -> begin
            M.handle_move_end ui_t t.additional_args x;
            refresh_ui ui_t;
            t.state <- `WaitForEnd 0
          end
      end
      | `WaitForEnd x ->
        if x < M.end_wait then t.state <- `WaitForEnd (x + 1)
        else begin
          M.handle_wait_end ui_t t.additional_args;
          t.state <- `WaitForMoveEnd
        end
      | `WaitForMoveEnd ->
         if M.finished ui_t t.additional_args then t.state <- `End
      | `End -> ()

  let is_demo_finished t =
    match t.state with `End -> true | _ -> false

end

module ButtonsMove =
struct
  let start_wait = 0
  let end_wait = 100
  type additional_args = unit
  let create_additional_args () = ()
  let compute_move t () =
    (* ensure list is not empty *)
    let open Ui_events in
    match t.buttons_to_move with
      | [] -> None
      | _ -> begin
        let random_idx = Random.int (List.length t.buttons_to_move) in
        let hd = List.nth t.buttons_to_move random_idx in
        let top, left, right, bottom = Utils.getBoundingClientRectCoordinates t.all_users_div in
        let curr_top, curr_left, curr_right, curr_bottom = Utils.getBoundingClientRectCoordinates (Html5.To_dom.of_element hd) in
        Some (hd, Animation.compute_funny_move curr_top curr_left top left)
      end

  let handle_move_end t () x =
    let open Ui_events in
    let button = Html5.To_dom.of_element x in
    let button_id = int_of_string (Utils.get_element_id button) in
    update_all_users_basket_from_button_id ~update_all_users:false t button_id

  let handle_wait_end t () = ()
  let finished t () = true
end

module type WaitParam = sig
  val start_wait: int
  val end_wait: int
end

module type SelectEventInDb = sig
  val get_destination_element: 'a dom_type Ui_events.ui_events -> Dom_html.element Js.t
  val drop_in_destination_element: 'a dom_type Ui_events.ui_events -> string -> unit Lwt.t
end

module EventsToAdditionalEvents (S:SelectEventInDb) (M: WaitParam) = struct
  include M
  type additional_args = {
    mutable wait_for_fb_request_completion: unit Lwt.t option;
    mutable set_children_back: (unit -> unit) option;
  }
  let create_additional_args () = {
    wait_for_fb_request_completion = None;
    set_children_back = None
  }
  let dom_node_to_element node =
    match Js.Opt.to_option (Dom_html.CoerceTo.element node) with
    | None -> assert(false)
    | Some x -> x

  let get_tbody_trs span =
    let span = Html5.To_dom.of_element span in
    let selected_events_children = Dom.list_of_nodeList (span##getElementsByTagName (Js.string "tbody")) in
    match selected_events_children with
      | [] -> None
      | tbody_element :: _ -> Some (tbody_element, List.map dom_node_to_element (Dom.list_of_nodeList tbody_element##childNodes))

  let get_trs span =
    match get_tbody_trs span with
      | None -> []
      | Some (_, trs) -> trs

  let select_event t trs =
    let in_selected = get_trs t.Ui_events.selected_events_span in
    let in_reference = get_trs t.Ui_events.reference_event_span in
    let already_selected_events = List.map Utils.get_element_id (in_selected @ in_reference) in
    let available = List.filter (fun x ->
      let x_id = Utils.get_element_id x in
      not (List.exists (fun y -> y = x_id) already_selected_events)) trs in
    let random_idx = Random.int (List.length available) in
    List.nth available random_idx

  let compute_move t additional_args =
    (* ensure list is not empty *)
    let open Ui_events in
    match (get_tbody_trs t.db_selected_events_span) with
      | None -> None
      | Some (tbody_element, trs) -> begin
        match trs with
          | [] -> None
          | trs -> begin
            let top, left, right, bottom = Utils.getBoundingClientRectCoordinates (S.get_destination_element t) in
            let event = select_event t trs in
            let curr_top, curr_left, curr_right, curr_bottom = Utils.getBoundingClientRectCoordinates event in
            let set_back () =
              let event_urls = List.map Utils.get_element_id trs in
              let events = List.map
                (fun x -> Events_store.find t.events_in_db_container x)
                event_urls
              in
              let new_trs = List.map Ui_events.make_selectable_event events in
              let tbody = dom_node_to_element tbody_element in
              Html5.Manip.replaceChildren (Html5.Of_dom.of_element tbody) new_trs
            in
            additional_args.set_children_back <- Some set_back;
            Some (Html5.Of_dom.of_element event,
                  Animation.compute_line_move curr_top curr_left top left)
          end
      end

  let handle_move_end t additional_args element =
    let dom_element = Html5.To_dom.of_element element in
    let event_id = Utils.get_element_id dom_element in
    let () = match additional_args.set_children_back with
      | None -> assert(false)
      | Some x -> x ()
    in
    additional_args.set_children_back <- None;
    additional_args.wait_for_fb_request_completion <- Some (S.drop_in_destination_element t event_id)

  let handle_wait_end t _ = ()
  let finished t additional_args =
    match additional_args.wait_for_fb_request_completion with
      | None -> assert(false)
      | Some lt -> begin
        match Lwt.state lt with
          | Lwt.Return () -> begin
            additional_args.wait_for_fb_request_completion <- None;
            true
          end
          | _ -> false
      end
end

module SelectAdditionalEvents = struct
  let get_destination_element t = t.Ui_events.selected_events_img
  let drop_in_destination_element t event_id = Ui_events.drop_event_id_in_selected_events t event_id
end

module SelectReferenceEvent = struct
  let get_destination_element t = t.Ui_events.reference_event_img
  let drop_in_destination_element t event_id = Ui_events.drop_event_id_in_reference_event t event_id
end

module MB = MakeMove (ButtonsMove)
module EAE = MakeMove(EventsToAdditionalEvents(SelectAdditionalEvents) (struct let start_wait = 100 let end_wait = 0 end))
module ERE = MakeMove(EventsToAdditionalEvents(SelectReferenceEvent) (struct let start_wait = 30 let end_wait = 0 end))

type 'a demo_move = [
| `SelectedEventMove of 'a EAE.t
| `ButtonMove of 'a MB.t
| `SelectedEventMoveToReferenceEvent of 'a ERE.t
| `ButtonMoveInReferenceEvent of 'a MB.t
| `LastSelectedEventMove of 'a EAE.t
| `LastButtonMove of 'a MB.t
| `Done
]

type 'a ui_with_demo = {
  ui_events: 'a Ui_events.ui_events;
  mutable demo: 'a demo_move;
}

let play_demo t = fun () ->
  match t.demo with
    | `SelectedEventMove arg -> begin
      EAE.next_move arg t.ui_events;

      if EAE.is_demo_finished arg then t.demo <- `ButtonMove (MB.create "You can select a group of users and drop it in the set of all users"
                                                                "triangle-obtuse")
    end
    | `ButtonMove arg -> begin
      MB.next_move arg t.ui_events;
      if MB.is_demo_finished arg then t.demo <- `SelectedEventMoveToReferenceEvent (ERE.create
                                                                                      "To compare users relative to different events, you can set a reference event"
                                                                                      "triangle-obtuse-other")
    end
    | `SelectedEventMoveToReferenceEvent arg -> begin
      ERE.next_move arg t.ui_events;
      if ERE.is_demo_finished arg then t.demo <- `ButtonMoveInReferenceEvent (MB.create
                                                                                "You can choose another group of users too"
                                                                                "triangle-obtuse")
    end
    | `ButtonMoveInReferenceEvent arg -> begin
      MB.next_move arg t.ui_events;
      if MB.is_demo_finished arg then t.demo <- `LastSelectedEventMove (EAE.create
                                                                          "You can add other events to the list of compared ones"
                                                                          "triangle-obtuse-other")
    end
    | `LastSelectedEventMove arg -> begin
      EAE.next_move arg t.ui_events;
      if EAE.is_demo_finished arg then t.demo <- `LastButtonMove (MB.create
                                                                    "And pick another set of users"
                                                                    "triangle-obtuse-other")
    end
    | `LastButtonMove arg -> begin
      MB.next_move arg t.ui_events;
      if MB.is_demo_finished arg then t.demo <- `Done
    end
    | `Done -> ()

let stop_demo t = t.demo <- `Done

let create
    url_input
    db_selected_events_span
    all_users_div
    reference_event_span reference_event_img reference_event_div
    selected_events_span selected_events_img selected_events_div
    legend_div demo_span =
  let ui_events = Ui_events.create
    url_input
    db_selected_events_span
    all_users_div
    reference_event_span reference_event_img reference_event_div
    selected_events_span selected_events_img selected_events_div
    legend_div demo_span in
  let () = Random.self_init () in
  let ui_with_demo =
    {
      ui_events = ui_events;
      demo = `SelectedEventMove (EAE.create "You can first choose an event" "triangle-obtuse-other")
    }
  in
  ignore (Dom_html.window##setInterval(Js.wrap_callback (play_demo ui_with_demo),
                                       0.05 *. 1000.));
  ui_with_demo

let ondragover ev _ =
   Dom.preventDefault ev;
   Lwt.return_unit

let on_all_users_div_drop t ev ev_arg =
  lwt () = Ui_events.on_all_users_div_drop t.ui_events ev ev_arg in
  stop_demo t;
  Lwt.return_unit

let setup t =
  let open Lwt_js_events in
  let ui_events = t.ui_events in
  async (fun () ->
         lwt () = Utils.lwt_autologin () in
         Ui_events.get_and_record_events ui_events None);
  async (fun () ->
         changes ui_events.Ui_events.url_input (Ui_events.on_db_input_changes ui_events));
  async (fun () ->
         dragovers ui_events.Ui_events.all_users_div ondragover);
  async (fun () ->
         dragovers ui_events.Ui_events.selected_events_div ondragover);
  async (fun () ->
         dragovers ui_events.Ui_events.reference_event_div ondragover);
  async (fun () ->
         drops ui_events.Ui_events.all_users_div (on_all_users_div_drop t));
  async (fun () ->
         drops ui_events.Ui_events.reference_event_div (Ui_events.on_user_drop_in_ref_event ui_events));
  async (fun () ->
         drops ui_events.Ui_events.selected_events_div (Ui_events.on_user_drop_in_selected_events ui_events))

}}
