{client{
open Eliom_content
open Html5.D


let hidde_elt elt =
  let () = Utils.show_element (Html5.To_dom.of_element elt) in
  Utils.set_element_as_transparent elt

type 'a dom_type = 'a constraint [>`Div | `Img | `PCDATA] = 'a
type 'a one_move = ('a dom_type elt * Animation.move list list)

type 'a move_state =
  | NoAnimation
  | WaitForStart of int
  | DuringMove of 'a one_move
  | WaitForEnd of int
  | WaitForMoveEnd
  | End

module type MakeMoveType = sig
  type additional_args
  val create_additional_args: unit -> additional_args
  val compute_move: 'a dom_type Ui_events.ui_events -> additional_args -> 'a one_move option
  val handle_move_end: 'a dom_type Ui_events.ui_events -> additional_args -> 'a dom_type elt -> unit
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

  let create text style = { state = NoAnimation;
                            additional_args = M.create_additional_args ();
                            text = text;
                            style = style }

  let next_move t ui_t =
    let open Ui_events in
    match t.state with
      | NoAnimation -> t.state <- WaitForStart 0
      | WaitForStart x -> begin
        if x < M.start_wait then t.state <- WaitForStart (x + 1)
        else
          let () = Ui_events.set_demo_text ui_t (Some (t.text, t.style)) in
          match M.compute_move ui_t t.additional_args with
            | None -> ()
            | Some m -> t.state <- DuringMove m
      end
      | DuringMove (x, l) -> begin
        match l with
          | hd :: tl -> begin
            Animation.do_move x hd;
            t.state <- DuringMove (x, tl)
          end
          | [] -> begin
            M.handle_move_end ui_t t.additional_args x;
            refresh_ui ui_t;
            t.state <- WaitForEnd 0
          end
      end
      | WaitForEnd x ->
        if x < M.end_wait then t.state <- WaitForEnd (x + 1)
        else begin
          M.handle_wait_end ui_t t.additional_args;
          t.state <- WaitForMoveEnd
        end
      | WaitForMoveEnd ->
         if M.finished ui_t t.additional_args then t.state <- End
      | End -> ()

  let is_demo_finished t =
    match t.state with End -> true | _ -> false

end

module MakeEventLanguageDemo = struct
  let wait_to_type = 1

  type write_string_and_wait = {
    mutable wait_for_typping: int;
    mutable string_to_be_typed: string option;
  }

  type state = [
    | `Init
    | `WriteString of write_string_and_wait
    | `DoDbQuery of unit Lwt.t
    | `Done ]

  type t = {
    mutable state: state;
    text: string;
    style: string;
  }
  let create_ws string = {
    wait_for_typping = wait_to_type;
    string_to_be_typed = Some string;
  }
  let create text style =
    { state = `Init;
      text = text;
      style = style; }

  let write_string ui ws =
    if (ws.wait_for_typping > 0) then begin
      ws.wait_for_typping <- ws.wait_for_typping - 1;
      false
    end
    else begin
      match ws.string_to_be_typed with
        | None -> true
        | Some s -> begin
          let older_value = ui.Ui_events.url_input##value in
          let next_char = s.[0] in
          ui.Ui_events.url_input##value <- Js.string ((Js.to_string older_value) ^ (String.make 1 next_char));
          let s_len = String.length s in
          let () = if s_len > 1 then begin
            let remaining = String.sub s 1 (s_len - 1) in
            ws.string_to_be_typed <- Some remaining;
            ws.wait_for_typping <- wait_to_type
          end
            else begin
              ws.string_to_be_typed <- None
            end
          in
          false
        end
    end

  let next_move t ui_t =
    match t.state with
      | `Init -> begin
        let () = Ui_events.set_demo_text ui_t (Some (t.text, t.style)) in
        let () = ui_t.Ui_events.url_input##value <- Js.string "" in
        t.state <- `WriteString (create_ws "nb_attending > 200")
      end
      | `WriteString ws -> begin
        if write_string ui_t ws then
          let input = (Js.to_string ui_t.Ui_events.url_input##value) in
          t.state <- `DoDbQuery (Ui_events.get_events_in_db ui_t (Some input))
      end
      | `DoDbQuery x -> begin
        match Lwt.state x with
          | Lwt.Return () -> t.state <- `Done
          | _ -> ()
      end
      | `Done -> ()


  let is_demo_finished t =
    match t.state with
      | `Done -> true
      | _ -> false
end

module ButtonsMove (M: sig val do_middle:bool end) =
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
        let button_elt = hd.Ui_events.button_elt in
        let () = hidde_elt hd.Ui_events.elt_on_the_right in
        let top, left, right, bottom = Utils.getBoundingClientRectCoordinates t.all_users_div in
        let curr_top, curr_left, curr_right, curr_bottom = Utils.getBoundingClientRectCoordinates (Html5.To_dom.of_element button_elt) in
        let source = Animation.create_point curr_left curr_top in
        let dest = Animation.create_point left top in
        let move = match M.do_middle with
          | false -> Animation.compute_funny_move source dest
          | true -> begin
            let mtop, mleft, mright, _ = Utils.getBoundingClientRectCoordinates (Html5.To_dom.of_element t.Ui_events.legend_div) in
            Animation.compute_rebound_on_middle_move source dest mleft mright mtop
          end
        in
        Some (button_elt, move)
      end

  let handle_move_end t () x =
    let open Ui_events in
    let button = Html5.To_dom.of_element x in
    let button_id = int_of_string (Utils.get_element_id button) in
    update_all_users_basket_from_button_id t button_id

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

  let get_tbody_trs div =
    let div = Html5.To_dom.of_element div in
    let selected_events_children = Dom.list_of_nodeList (div##getElementsByTagName (Js.string "tbody")) in
    match selected_events_children with
      | [] -> None
      | tbody_element :: _ -> Some (tbody_element, List.map dom_node_to_element (Dom.list_of_nodeList tbody_element##childNodes))

  let get_trs div =
    match get_tbody_trs div with
      | None -> []
      | Some (_, trs) -> trs

  let select_event t trs =
    let get_all_url event_store =
      let res = ref [] in
      let () = Ui_events.Events_store.iter (fun url _ ->
        res := url :: !res) event_store
      in
      !res
    in
    let in_selected = get_all_url t.Ui_events.selected_events in
    let already_selected_events =
      match t.Ui_events.ref_event with
        | `Undefined -> in_selected
        | `Resolving (y, _) -> y.Utils.url :: in_selected
        | `Resolved (y, _) -> y.Utils.ev_url :: in_selected
    in
    let available = List.filter (fun x ->
      let x_id = Utils.get_element_id x in
      not (List.exists (fun y -> y = x_id) already_selected_events)) trs in
    let random_idx = Random.int (List.length available) in
    List.nth available random_idx

  let compute_move t additional_args =
    (* ensure list is not empty *)
    let open Ui_events in
    match (get_tbody_trs t.db_selected_events_div) with
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
                  Animation.compute_line_move
                    (Animation.create_point curr_left curr_top)
                    (Animation.create_point left top))
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
  let get_destination_element t = Html5.To_dom.of_element t.Ui_events.selected_events_div
  let drop_in_destination_element t event_id = Ui_events.drop_event_id_in_selected_events t event_id
end

module SelectReferenceEvent = struct
  let get_destination_element t = Html5.To_dom.of_element t.Ui_events.reference_event_div
  let drop_in_destination_element t event_id = Ui_events.drop_event_id_in_reference_event t event_id
end

module ButtonsMoveNoMiddle = ButtonsMove (struct let do_middle = false end)
module ButtonsMoveMiddle = ButtonsMove (struct let do_middle = true end)
module MBN = MakeMove (ButtonsMoveNoMiddle)
module MBM = MakeMove (ButtonsMoveNoMiddle)
module EAE = MakeMove(EventsToAdditionalEvents(SelectAdditionalEvents) (struct let start_wait = 100 let end_wait = 0 end))
module ERE = MakeMove(EventsToAdditionalEvents(SelectReferenceEvent) (struct let start_wait = 30 let end_wait = 0 end))

class type ['a] demo_move_type =
object
  method next_move: 'a dom_type Ui_events.ui_events -> unit
  method is_demo_finished: unit -> bool
end

class ['a] event_language_demo init =
object
  val arg = init
  method next_move (t:'a dom_type Ui_events.ui_events) = MakeEventLanguageDemo.next_move arg t
  method is_demo_finished () = MakeEventLanguageDemo.is_demo_finished arg
end

class ['a] eae init =
object
  val arg = init
  method next_move (t:'a dom_type Ui_events.ui_events) = EAE.next_move arg t
  method is_demo_finished () = EAE.is_demo_finished arg
end

class ['a] ere init =
object
  val arg = init
  method next_move (t: 'a dom_type Ui_events.ui_events) = ERE.next_move arg t
  method is_demo_finished () = ERE.is_demo_finished arg
end

class ['a] eab init =
object (self)
  val initial_style = init
  val mutable state = `NotStarted

  method filter_buttons filter_bt filter_reltype l =
    List.filter (fun x -> let bt, relative_type = x.Ui_events.displayed_information in
                          if filter_bt bt then filter_reltype relative_type
                          else false) l
  method compute_move fbt frbt (t:'a dom_type Ui_events.ui_events): ('a dom_type Ui_events.one_button_to_move * Animation.move list list * ('a dom_type elt * Animation.move list list) list) =
    let open Ui_events in
    let buttons_to_pick = self#filter_buttons fbt frbt t.buttons_to_move in
    let () = assert (List.length buttons_to_pick > 0) in
    let random_idx = Random.int (List.length buttons_to_pick) in
    let nth = List.nth buttons_to_pick random_idx in
    let button_elt = nth.button_elt in
    let legend_elements = List.filter (fun x -> frbt x.legend_button_type) t.legend_buttons_to_move in
    let legend_buttons = List.map (fun x -> x.legend_button) legend_elements in
    let button_to_hidde = List.map (fun x -> x.legend_elt_on_the_right) legend_elements in
    let () = List.iter hidde_elt (nth.elt_on_the_right :: button_to_hidde) in
    let compute_vertical_move b =
      let curr_top, curr_left, curr_right, curr_bottom = Utils.getBoundingClientRectCoordinates (Html5.To_dom.of_element b) in
      let point = Animation.create_point curr_left curr_top in
      Animation.compute_vertical_move point
    in
    (nth, compute_vertical_move button_elt, List.map (fun x -> x, compute_vertical_move x) legend_buttons)

  method compute_all_moves (t:'a dom_type Ui_events.ui_events) =
    List.map (fun x ->
      let msg, f1, f2 = x in
      `Init (msg, fun () -> self#compute_move f1 f2 t))
      [("These are the people that attended to the selected event and that attended to the reference event too", (fun x -> x = `Attending), (fun x -> x = `RelAttending));
       ("Those are the ones that attended to the selected event and that declined the reference event's invitation", (fun x -> x = `Attending), (fun x -> x = `RelDeclined));
       ("Those are the ones that declined the selected event's invitation, but that were not invited to the reference event", (fun x -> x = `Declined), (fun x -> x = `RelNotInvited));
       ("Those are the ones that were invited to the selected event, and that were invited to the reference event too", (fun x -> x = `Invited), (fun x -> x = `RelInvited))]


  method next_move (t:'a dom_type Ui_events.ui_events) =
    match state with
      | `NotStarted -> begin
        Ui_events.set_demo_text t (Some ("Let's take some time to explain the new group of users that appeared", initial_style));
        state <- `WaitForFirstMsgRead 0
      end
      | `WaitForFirstMsgRead x -> begin
        if x = 50 then state <- `Moving (self#compute_all_moves t)
        else state <- `WaitForFirstMsgRead (x + 1)
      end
      | `Moving l -> begin
        match l with
          | hd :: tl -> begin
            match hd with
              | `Init (msg, f) -> begin
                Ui_events.set_demo_text t (Some (msg, initial_style));
                let elt, move_l, legend_moves = f () in
                state <- `Moving (`DoMove (elt, move_l, legend_moves) :: tl)
              end
              | `DoMove (elt_to_move, move_l, legend_moves) -> begin
                match move_l with
                  | [] -> begin
                    Ui_events.refresh_compared_buttons_only t elt_to_move;
                    Ui_events.display_legend_div ~force:true t;
                    state <- `Moving tl
                  end
                  | curr_move :: other_move -> begin
                    Animation.do_move elt_to_move.Ui_events.button_elt curr_move;
                    let legend_moves = List.map (fun (x, l) ->
                      let hd, tl = List.hd l, List.tl l in
                      let () = Animation.do_move x hd in
                      x, tl) legend_moves in
                    state <- `Moving ((`DoMove (elt_to_move, other_move, legend_moves)) :: tl)
                  end
              end
          end
          | [] -> begin
            let curr_top, curr_left, _, _ = Utils.getBoundingClientRectCoordinates (Html5.To_dom.of_element t.Ui_events.div_in_legend_div) in
            let point = Animation.create_point curr_left curr_top in
            Ui_events.set_demo_text t (Some ("Do you get the idea ? If you forgot the meaning of different user sets, don't worry, the legend is here to help you", initial_style));
            state <- `Last (Animation.compute_vertical_move point)
          end
      end
      | `Last l -> begin
        match l with
          | [] -> begin
            Ui_events.display_legend_div ~force:true t;
            state <- `Stop
          end
          | hd :: tl -> begin
            Animation.do_move t.Ui_events.div_in_legend_div hd;
            state <- `Last tl
          end
      end
      | `Stop -> ()

  method is_demo_finished () =
    match state with
      | `Stop -> true
      | _  -> false
end

class ['a] mbn init =
object
  val arg = init
  method next_move (t: 'a dom_type Ui_events.ui_events) = MBN.next_move arg t
  method is_demo_finished () = MBN.is_demo_finished arg
end

class ['a] mbm init =
object
  val arg = init
  method next_move (t: 'a dom_type Ui_events.ui_events) = MBM.next_move arg t
  method is_demo_finished () = MBM.is_demo_finished arg
end

type 'a demo_state = [
| `Start
| `MakeUserLog of unit Lwt.t
| `DisplayDb of unit Lwt.t
| `CheckUserExists of Utils.application_user option Lwt.t
| `RegisterUser of unit Lwt.t
| `DoDemo of 'a demo_move_type list
| `Done
]

type 'a ui_with_demo = {
  ui_events: 'a Ui_events.ui_events;
  mutable demo: 'a demo_state;
}


let make_jsonable_user user_id username = {
  Users.user_id = user_id;
  Users.user_name = username;
  Users.nb_event_added = 0;
}

let must_get_user t =
  match !(t.ui_events.Ui_events.logged_user_ref) with
    | None -> assert(false)
    | Some x -> x

let create_moves () =
  let () = Random.self_init () in
  let constructors =
    [(fun x -> (new eae (EAE.create "First choose an event the audience you're looking for might have liked" x) :> 'a demo_move_type));
     (fun x -> (new mbn (MBN.create "You can drag a group of users and drop it in the users bin" x) :> 'a demo_move_type));
     (fun x -> (new ere (ERE.create "To compare users relative to different events, you can set a reference event" x) :> 'a demo_move_type));
     (fun x -> (new mbm (MBM.create "You can choose another group of facebook users (if a user is already in the bin he won't be appended twice)" x) :> 'a demo_move_type));
     (fun x -> ((new eab x) :> 'a demo_move_type));
     (fun x -> (new event_language_demo (MakeEventLanguageDemo.create "Let's look for events that got at least 200 attending users" x) :> 'a demo_move_type));
     (fun x -> (new eae (EAE.create "Let's add one of the events with more than 200 attending users into the events bin" x) :> 'a demo_move_type));
     (fun x -> (new mbm (MBM.create "Drag and drop another set of users (here again, if a user is already in the bin he won't be appended twice)" x) :> 'a demo_move_type))]
  in
  let moves = List.fold_left (fun accum x ->
    let curr_style, res = accum in
    let new_tyle =
      if curr_style = "triangle-obtuse" then "triangle-obtuse-other"
      else "triangle-obtuse"
    in
    new_tyle, (x curr_style :: res))
    ("triangle-obtuse", []) constructors
  in
  let _, demo = moves in
  List.rev demo

let play_demo t = t.demo <- `DoDemo (create_moves ())

let run_demo t = fun () ->
  match t.demo with
    | `Start -> begin t.demo <- `MakeUserLog (Utils.get_user_id t.ui_events.Ui_events.logged_user_ref) end
    | `MakeUserLog lt -> begin
      match Lwt.state lt with
        | Lwt.Return () -> begin t.demo <- `DisplayDb (Ui_events.get_events_in_db t.ui_events None) end
        | _ -> ()
    end
    | `DisplayDb lt -> begin
      match Lwt.state lt with
        | Lwt.Return () -> begin
          let user = must_get_user t in
          t.demo <- `CheckUserExists (%Users.rpc_user_exists user.Utils.user_id)
        end
        | _ -> ()
    end
    | `CheckUserExists lt -> begin
      match Lwt.state lt with
        | Lwt.Return x -> begin
          match x with
            | None -> begin
              let user = must_get_user t in
              t.demo <- `RegisterUser (%Users.rpc_insert_user (make_jsonable_user user.Utils.user_id user.Utils.user_name))
            end
            | Some _ -> begin t.demo <- `Done end
        end
        | _ -> ()
    end
    | `RegisterUser lt -> begin
      match Lwt.state lt with
        | Lwt.Return _ -> begin play_demo t end
        | _ -> ()
    end
    | `DoDemo l -> begin
      match l with
        | [] -> begin t.demo <- `Done end
        | hd :: tl -> begin
          let () = hd#next_move t.ui_events in
          if hd#is_demo_finished () then t.demo <- `DoDemo tl
        end
    end
    | `Done -> ()

let stop_demo t = t.demo <- `Done

let create ui_events =
  let ui_with_demo =
    {
      ui_events = ui_events;
      demo = `Start;
    }
  in
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
         changes ui_events.Ui_events.url_input (Ui_events.on_db_input_changes ui_events));
  async (fun () ->
         dragovers ui_events.Ui_events.all_users_div ondragover);
  async (fun () ->
         dragovers ui_events.Ui_events.selected_events_div_container ondragover);
  async (fun () ->
         dragovers ui_events.Ui_events.reference_event_div_container ondragover);
  async (fun () ->
         drops ui_events.Ui_events.all_users_div (on_all_users_div_drop t));
  async (fun () ->
         drops ui_events.Ui_events.reference_event_div_container (Ui_events.on_user_drop_in_ref_event ui_events));
  async (fun () ->
         drops ui_events.Ui_events.selected_events_div_container (Ui_events.on_user_drop_in_selected_events ui_events));
  ignore (Dom_html.window##setInterval(Js.wrap_callback (run_demo t),
                                       0.05 *. 1000.));


}}
