{client{
open Eliom_content
open Html5.D
type 'a one_move = ('a Eliom_content.Html5.elt * Animation.move list list)

type 'a move_state = [
| `NoAnimation
| `WaitForStart of int
| `DuringMove of 'a one_move
| `WaitForEnd of int
| `End ]

module type MakeMoveType = sig
  val compute_move: 'a Ui_events.ui_events -> 'a one_move option
  val handle_move_end: 'a Ui_events.ui_events -> 'a Eliom_content.Html5.elt -> unit
  val handle_wait_end: 'a Ui_events.ui_events -> unit
end

module ButtonsMove = struct
  let compute_move t =
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

  let handle_move_end t x =
    let open Ui_events in
    let button = Html5.To_dom.of_element x in
    let id = Js.Opt.get (button##getAttribute (Js.string "id")) (fun () -> assert false) in
    let button_id = int_of_string (Js.to_string id) in
    update_all_users_basket_from_button_id ~update_all_users:false t button_id

  let handle_wait_end t =
    let open Ui_events in
    update_all_users_basket t t.all_users_container
end

module MakeMove (M:MakeMoveType) = struct
  type 'a t = { mutable state: 'a move_state }
  let create () = { state = `NoAnimation }
  let next_move t ui_t =
    let open Ui_events in
    match t.state with
      | `NoAnimation -> t.state <- `WaitForStart 0
      | `WaitForStart x -> begin
        if x < 100 then t.state <- `WaitForStart (x + 1)
        else
          match M.compute_move ui_t with
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
            M.handle_move_end ui_t x;
            refresh_ui ui_t;
            t.state <- `WaitForEnd 0
          end
      end
      | `WaitForEnd x ->
        if x < 100 then t.state <- `WaitForEnd (x + 1)
        else begin
          M.handle_wait_end ui_t;
          t.state <- `End
        end
      | `End -> ()

  let is_demo_finished t =
    match t.state with `End -> true | _ -> false

end
module MB = MakeMove(ButtonsMove)

type 'a demo_move = [
| `ButtonMove of 'a MB.t
| `Done
]

type 'a ui_with_demo = {
  ui_events: 'a Ui_events.ui_events;
  mutable demo: 'a demo_move;
}

let play_demo t = fun () ->
  match t.demo with
    | `ButtonMove dt -> begin
      MB.next_move dt t.ui_events;
      if MB.is_demo_finished dt then t.demo <- `Done
    end
    | `Done -> ()

let stop_demo t = t.demo <- `Done

let create
    url_input
    db_selected_events_span
    all_users_div
    reference_event_span reference_event_img reference_event_div
    selected_events_span selected_events_img selected_events_div
    legend_div =
  let ui_events = Ui_events.create
    url_input
    db_selected_events_span
    all_users_div
    reference_event_span reference_event_img reference_event_div
    selected_events_span selected_events_img selected_events_div
    legend_div in
  let () = Random.self_init () in
  let ui_with_demo =
    {
      ui_events = ui_events;
      demo = `ButtonMove (MB.create())
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
