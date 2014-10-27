{client{
open Eliom_content
open Html5.D

let range ?step:(s=1) start_idx end_idx =
  let rec _range cidx eidx accum =
    if cidx + s >= eidx then List.rev (eidx :: accum)
    else _range (cidx + s) eidx (cidx :: accum)
  in
  _range start_idx end_idx []

module ProjectedY : sig
  type t
  val of_float: float -> t
  val project_in_x: t -> float
  val to_float: t -> float
  val to_string: t -> string
  val add: t -> t -> t
  val sub: t -> t -> t
  val mult: t -> t -> t
  val div: t -> t -> t
  val add_float: t -> float -> t
  val sub_float: t -> float -> t
  val float_sub: float -> t -> t
  val mult_float: t -> float -> t
  val div_float: t -> float -> t
  val zero: t
end = struct
  type t = float
  let zero = 0.0
  let of_float x = 0.0 -. x
  let project_in_x x = x
  let to_float x = 0.0 -. x
  let to_string x = Printf.sprintf "%f" x
  let add x y = x +. y
  let sub x y = x -. y
  let mult x y = x *. y
  let div x y = x /. y
  let add_float = add
  let sub_float = sub
  let float_sub = sub
  let mult_float = mult
  let div_float = div
end

let rec compute_x_range ?step:(s=20) sx dx =
  let to_int x = int_of_float x in
  if sx > dx then List.rev (compute_x_range ~step:s dx sx)
  else range ~step:s (to_int sx) (to_int dx)

let compute_between ?coeff:(c=1.0 /. 2.0) xO xD = xO +. (xD -. xO) *. c

let line_func_param yO xO yD xD =
  (* y = slope * x + y_src *)
  let open ProjectedY in
  let slope = div_float (sub yD yO) (xD -. xO) in
  let origin = sub yO (mult_float slope xO) in
  slope, origin

let line_func yO xO yD xD =
  let slope, origin = line_func_param yO xO yD xD in
  let open ProjectedY in
  fun x -> add (mult_float slope x) origin

let parabolic_func_param ?coeff:(c=0.1) yO xO yD xD =
  assert(c <> 0.5);
  (* move from O to D *)
  (* y = a * x * x + b * x + c *)
  (* lets choose the max abscisse *)
  let maxX = compute_between ~coeff:c xO xD in
  (* yO = a *. xO *. xO + b *. XO + c *)
  (* yD = a *. xD *. xD + b *. XD + c *)
  (* replace c in yD with expression in yO *)
  (* a * (xO *. xO - xD *. xD) = (yO -. yD) - b. *. (xO -. xD) *)
  (* maxX = - b /. (2 *. a) *)
  (* -c *. 2 *. a = b *)
  (* a *. (xO *. xO - xD *. xD) - maxX *. 2 *. a * (xO -. xD) = yO - yD *)
  (* a = (yO -. yD) / ((xO *. xO - xD *. xD) -maxX *. 2 *. (xO -. xD) *)
  let open ProjectedY in
  let a = div_float (sub yO yD) ((xO *. xO -. xD *. xD) -. maxX *. 2.0 *. (xO -. xD)) in
  let b = float_sub 0.0 (mult_float a (2.0 *.maxX)) in
  let c = sub yO (add (mult_float b xO) (mult_float a (xO *. xO))) in
  (a, b, c)

let parabolic_func ?coeff:(c=0.1) yO xO yD xD =
  let open ProjectedY in
  let a, b, c = parabolic_func_param ~coeff:c yO xO yD xD in
  fun x -> add (add (mult_float a (x *. x)) (mult_float b x)) c

let compute_one_basketball_traj ?coeff:(coeff=0.1) yO xO yD xD =
  let a, b, c = parabolic_func_param ~coeff:coeff yO xO yD xD in
  let f = parabolic_func ~coeff:coeff yO xO yD xD in
  let open ProjectedY in
  if a <= zero then (f, f (project_in_x (div (sub zero b) (mult_float a 2.0))))
  else
    (* make the curve look good, by computing
       the symmetry from the line between the two points
    *)
    let s, _ = line_func_param yO xO yD xD in
    let line_f = line_func yO xO yD xD in
    let f x =
      let yparabol = f x in
      let yline = line_f x in
      add yline (sub yline yparabol)
    in
    (* yparabol = a * x2 + b * x + c *)
    (* yline = s * x + r *)
    (* y = 2 * yline - yparabol *)
    (* y' = 2 * s - 2 * a * x - b *)
    (* 0 = 2 * s - 2 * a * max - b *)
    (* max = (2 * s - b)  / (2 * a) *)
    (f, f (project_in_x (div (sub (mult_float s 2.0) b) (mult_float a 2.0))))

let basketball_func yO xO yD xD =
 (* compute the one that goes the highest, but still fits into the screen *)
 let get_max coeff =
   let _, max = compute_one_basketball_traj ~coeff:coeff yO xO yD xD in
   max
 in
 (* we browse for 100 trajectories the one that best suits our needs *)
 let r = List.map (fun x -> let coeff = 0.01 *. (float_of_int x) in
                             (coeff, get_max coeff)) (List.filter (fun x -> x <> 50) (range 1 100)) in
 let filtered = List.filter (fun (coeff, max) -> max < (ProjectedY.of_float 0.0)) r  in
 let (c, _) = List.hd (List.sort (fun (c1, max1) (c2, max2) -> Pervasives.compare max2 max1) filtered) in
 let f, _ = compute_one_basketball_traj ~coeff:c yO xO yD xD in
 f

let rec compute_x_range ?step:(s=20) sx dx =
  let to_int x = int_of_float x in
  if sx > dx then List.rev (compute_x_range ~step:s dx sx)
  else range ~step:s (to_int sx) (to_int dx)

type move = [
| `Left of int
| `Top of int
| `PaddingBottom of int
| `PaddingTop of int
| `Skip ]

let do_one_move x move =
  match move with
    | `Top d -> Html5.Manip.SetCss.topPx x d
    | `Left d -> Html5.Manip.SetCss.leftPx x d
    | `PaddingBottom d -> Html5.Manip.SetCss.paddingBottomPx x d
    | `PaddingTop d -> Html5.Manip.SetCss.paddingTopPx x d
    | `Skip -> ()

let move_done = ref false

let do_move x move =
  Html5.Manip.SetCss.position x "absolute";
  List.iter (fun m -> do_one_move x m) move

let compute_move ?step:(s=20) sx dx f =
  let range_of_x = compute_x_range ~step:s sx dx in
  List.map (fun x -> let y = int_of_float (ProjectedY.to_float (f (float_of_int x))) in
                     if not !move_done then (
                       Firebug.console##log(Js.string (Printf.sprintf "x:%d y:%d" x y)));
                     [`Left x; `Top y]) range_of_x

let compute_basketball_move yO xO yD xD =
  compute_move xO xD (basketball_func yO xO yD xD)

let compute_funny_move yO xO yD xD =
  let open ProjectedY in
  let yO = of_float yO in
  let yD = of_float yD in
  let xMiddle = compute_between xO xD in
  let yMiddle = div_float (add yD yO) 3.0 in
  let move_1 = compute_basketball_move yO xO yMiddle xMiddle in
  let move_2 = compute_basketball_move yMiddle xMiddle yO xO in
  let yMiddle = div_float (add yD yO) 1.5 in
  let move_3 = compute_basketball_move yO xO yMiddle xMiddle in
  let move_4 = compute_basketball_move yMiddle xMiddle yD xD in
  move_1 @ move_2 @ move_3 @ move_4

}}
