(**************************************************************************)
(*  Copyright 2014, Ion Alberdi <nolaridebi at gmail.com>                 *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)
{client{
open Eliom_content
open Html5.D

let range ?step:(s=1) start_idx end_idx =
  let rec _range cidx eidx accum =
    if cidx + s >= eidx then List.rev (cidx :: accum)
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

let vertical_fire_func x yO yD =
  let g = 9.91 in
  let open ProjectedY in
  (* y = yO + b * t - 1/2 * g * t * t *)
  (* max in b - g * t = 0 *)
  (* i.e. b / g = t *)
  (* we want that the max is in yD *)
  (* yD = yO + b * b / g - g / 2 * (b / g) * (b / g) *)
  (* yD = yO + b^2 / g - (b^2 / (g * 2) *)
  (* yD = yO + b^2 / (g * 2) *)
  (* b = sqrt ((yD - yO) * (g * 2)) *)
  let yd_yo = project_in_x (sub yD yO) in
  let yd_yo = if yd_yo < 0.0 then 0.0 -. yd_yo else yd_yo in
  let b = 0.0 -. sqrt (yd_yo *. g *. 2.0) in
  let f t = sub_float (add_float yO (b *. t)) ((g /. 2.0) *. t *. t) in
  (* 0 = t * (b - g * t / 2*)
  let t_end = 2.0 *. b /. g in
  let number_points = 25.0 in
  let t_range = List.map
    (fun x -> (float_of_int x) *. t_end /. number_points)
    (range 0 (int_of_float number_points))
  in
  Utils.log (Printf.sprintf "%f->%d" x (int_of_float x));
  List.map (fun t -> (int_of_float x), f t) t_range

let parabolic_func_param ?coeff:(c=0.1) yO xO yD xD =
  (* move from O to D *)
  (* y = a * x * x + b * x + c *)
  (* lets choose the max abscisse *)
  let c =
    if yO = yD then 0.5
    else c
  in
  let maxX = compute_between ~coeff:c xO xD in
  (* yO = a *. xO *. xO + b *. XO + c *)
  (* yD = a *. xD *. xD + b *. XD + c *)
  (* replace c in yD with expression in yO *)
  (* a * (xO *. xO - xD *. xD) = (yO -. yD) - b. *. (xO -. xD) *)
  (* maxX = - b /. (2 *. a) *)
  (* -maxX *. 2 *. a = b *)
  let open ProjectedY in
  let a =
    if c <> 0.5 then begin
      (* a *. (xO *. xO - xD *. xD) - maxX *. 2 *. a * (xO -. xD) = yO - yD *)
      (* a = (yO -. yD) / ((xO *. xO - xD *. xD) -maxX *. 2 *. (xO -. xD) *)
      div_float (sub yO yD) ((xO *. xO -. xD *. xD) -. maxX *. 2.0 *. (xO -. xD))
    end
    else begin
        (*we have an infinty of solutions so we choose yMax to be
          yMax = yD + upper
          using maxX on the general equation we have
          yMax = a *. maxX *. maxX +. (-2 *.maxX *.a) *. maxX +. c
          yMax = -a *. maxX^2 + c,
          uxing xD on the equation:
          c = yD - a * xD^2 + 2maxX * a * xD,
          we replace c,
          yMax = -a *. maxX^2 + yD - a * xD^2 + 2maxX * a * xD
          yMax - yD = a (2maxX * XD - maxX^2 - XD^2)
          let yMax = yD + upper *)
      let upper = if xO < xD then (xO -. xD) else (xD -. xO) in
      let yMax = add_float yD (upper /. 5.0) in
      div (sub yMax yD) (of_float ((2.0 *. maxX *. xD -. maxX *. maxX -. xD *. xD)))
    end
  in
  let b = float_sub 0.0 (mult_float a (2.0 *.maxX)) in
  let c = sub yO (add (mult_float b xO) (mult_float a (xO *. xO))) in
  a, b, c

let parabolic_func ?coeff:(c=0.1) yO xO yD xD =
  let open ProjectedY in
  let a, b, c = parabolic_func_param ~coeff:c yO xO yD xD in
  a, b, c, fun x -> add (add (mult_float a (x *. x)) (mult_float b x)) c

let compute_one_basketball_traj ?coeff:(coeff=0.1) yO xO yD xD =
  let open ProjectedY in
  let a, b, c, f =
    if yO <> yD then parabolic_func ~coeff:coeff yO xO yD xD
    else parabolic_func yO xO yD xD
  in
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
  let f, _ =
    if yO <> yD then begin
     let get_max coeff =
       let _, max = compute_one_basketball_traj ~coeff:coeff yO xO yD xD in
       max
     in
     (* we browse for 100 trajectories the one that best suits our needs *)
     let r = List.map (fun x -> let coeff = 0.01 *. (float_of_int x) in
                                 (coeff, get_max coeff)) (List.filter (fun x -> x <> 50) (range 1 100)) in
     let filtered = List.filter (fun (coeff, max) -> max < (ProjectedY.of_float 0.0)) r  in
     let (c, _) = List.hd (List.sort (fun (c1, max1) (c2, max2) -> Pervasives.compare max2 max1) filtered) in
     compute_one_basketball_traj ~coeff:c yO xO yD xD
    end
  else
    compute_one_basketball_traj yO xO yD xD
 in f

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

let do_move x move =
  Html5.Manip.SetCss.position x "fixed";
  List.iter (fun m -> do_one_move x m) move

let translate_coordinates_to_move l =
  List.map (fun (x, y) -> let y = int_of_float (ProjectedY.to_float y) in
                          [`Left x; `Top y]) l

let compute_move ?step:(s=20) sx dx f =
  let range_of_x = compute_x_range ~step:s sx dx in
  translate_coordinates_to_move (List.map
                                   (fun x -> x, f (float_of_int x))
                                   range_of_x)
type point = {
  x: float;
  y: float;
}

let create_point x y = {x=x; y=y;}

let compute_basketball_move ?step:(s=20) yO xO yD xD =
  compute_move ~step:s xO xD (basketball_func yO xO yD xD)

let compute_line_move source dest =
  let open ProjectedY in
  let yO, yD = of_float source.y, of_float dest.y in
  compute_move source.x dest.x (line_func yO source.x yD dest.x)

let compute_vertical_move point =
  let open ProjectedY in
  let x = point.x in
  let y = of_float point.y in
  let ydest = add_float y 100.0 in
  let r = translate_coordinates_to_move (vertical_fire_func x y ydest) in
  List.fold_left (fun res _ ->
    res @ r) [] (range 0 10)


let compute_funny_move source dest =
  let open ProjectedY in
  let yO = of_float source.y in
  let yD = of_float dest.y in
  let xO = source.x in
  let xD = dest.x in
  let xMiddle = compute_between xO xD in
  let yMiddle = div_float (add yD yO) 3.0 in
  let move_1 = compute_basketball_move yO xO yMiddle xMiddle in
  let move_2 = compute_basketball_move yMiddle xMiddle yO xO in
  let yMiddle = div_float (add yD yO) 1.5 in
  let move_3 = compute_basketball_move yO xO yMiddle xMiddle in
  let move_4 = compute_basketball_move yMiddle xMiddle yD xD in
  move_1 @ move_2 @ move_3 @ move_4

let compute_rebound_on_middle_move source dest mleft mright mtop =
  Utils.log (Printf.sprintf "mleft: %f, mright: %f mtop: %f" mleft mright mtop);
  let open ProjectedY in
  let mtop = of_float mtop in
  let yD = of_float dest.y in
  let yO = of_float source.y in
  let xO = source.x in
  let xD = dest.x in
  let number_rebounds = 6 in
  let coeff = ((Pervasives.min xO mright) -. mleft) /. (float_of_int number_rebounds) in
  Utils.log (Printf.sprintf "coeff: %f" coeff);
  let compute_y i =
    if i = number_rebounds - 1 then div_float (add yO mtop) 2.0
    else if i = 1 then div_float (add yD mtop) 2.0
    else mtop
  in
  let middle_points = List.map (fun x ->
    (mleft +. coeff *. (float_of_int x), compute_y x))
    (List.rev (range 1 number_rebounds))
  in
  let () =
    Utils.log (Printf.sprintf "xO: %f" xO);
    List.iter (fun (x, _) ->
      Utils.log (Printf.sprintf "middle_point: %f" x))
      middle_points;
    Utils.log (Printf.sprintf "xD: %f" xD);
  in
  let _, res = List.fold_left (fun a b ->
    let xD, yD = b in
    let (xO, yO), previous_moves = a in
    let curr_move =
      try compute_basketball_move yO xO yD xD
      with (Failure _) -> (
        Utils.log (Printf.sprintf "failure xO: %f xD:%f" xO xD);
        [])
    in
    (b, previous_moves @ curr_move))
    ((xO, yO), [])
    (middle_points @ [(xD, yD)])
  in
  res

}}
