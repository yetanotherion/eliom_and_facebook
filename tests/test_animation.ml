(* XXX: find a more conveniant way than:
   ocaml -I ../_server test_animation.ml
   to run it *)

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
          yMax = yO + ((xO - xD) / 2)
          using maxX on the general equation we have
          yMax = a *. maxX *. maxX +. (-2 *.maxX *.a) *. maxX +. c
          yMax = -a *. maxX^2 + c,
          uxing xD on the equation:
          c = yD - a * xD^2 + 2maxX * a * xD,
          we replace c,
          yMax = -a *. maxX^2 + yD - a * xD^2 + 2maxX * a * xD
          yMax - yD = a (2maxX * XD - maxX^2 - XD^2)
          let yMax = yD + upper *)
        let upper = if xO < xD then (xO -. xD) /. 2.0 else (xD -. xO) /. 2.0 in
        of_float (upper /. (2.0 *. maxX *. xD -. maxX *. maxX -. xD *. xD))
        end
  in
  let b = float_sub 0.0 (mult_float a (2.0 *.maxX)) in
  let c = sub yO (add (mult_float b xO) (mult_float a (xO *. xO))) in
  a, b, c

let parabolic_func ?coeff:(c=0.1) yO xO yD xD =
  let open ProjectedY in
  let a, b, c = parabolic_func_param ~coeff:c yO xO yD xD in
  let () = Printf.printf "a:%f, b:%f, c:%f" (to_float a) (to_float b) (to_float c) in
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

let make_test1 () =
  let (yD, xD) = (ProjectedY.of_float 40.0, 400.0) in
  let (yO, xO) = (ProjectedY.of_float 140.0, 1500.0)  in
  let f = parabolic_func yO xO yD xD in
  let assert_eq a b =
    let open ProjectedY in
    let diff = (to_float b) -. (to_float a) in
    let diff = if diff < 0.0 then 0.0 -. diff else diff in
    if diff > 0.001 then raise(Failure(Printf.sprintf "%s <> %s" (ProjectedY.to_string a) (ProjectedY.to_string b)))
  in
  let () = assert_eq (f xD) yD in
  let () = assert_eq (f xO) yO in
  ();;

let write_lines f l =
  let open Printf in
  let fd = open_out f in
  let () = List.iter (fun x -> fprintf fd "%s\n" x) l in
  close_out fd;;

let make_gnuplot_script gnuplot_f data_f =
  let ps = Printf.sprintf in
  let r = ["set terminal postscript portrait enhanced mono dashed lw 1 'Helvetica' 14";
           "set size 1.0, 0.6";
           ps "plot '%s' using 1:2" data_f] in
  write_lines gnuplot_f r

let make_f_test f name =
  let ps = Printf.sprintf in
  let (yD, xD) = (ProjectedY.of_float 40.0, 400.0) in
  let (yO, xO) = (ProjectedY.of_float 40.0, 1500.0)  in
  let f = f yO xO yD xD in
  let r = List.map (fun x -> float_of_int x) (compute_x_range xO xD) in
  let () = write_lines (ps "%s.dat" name) (List.map (fun x -> Printf.sprintf "%f %s" x (ProjectedY.to_string (f x))) r) in
  let () = make_gnuplot_script (ps "%s.gplt" name) (ps "%s.dat" name) in
  let _ = Sys.command (ps "gnuplot < %s.gplt > %s.ps" name name) in
  let _ = Sys.command (ps "open %s.ps" name) in
  ();;

let make_test2 () =
  make_f_test (fun yO xO yD xD ->
    parabolic_func ~coeff:0.5 yO xO yD xD) "parabolic";;

let make_test3 () =
  make_f_test line_func "line";;

let make_test4 () =
  make_f_test basketball_func "basketball";;


make_test1 ();;
make_test2 ();;
make_test3 ();;
make_test4 ();;
