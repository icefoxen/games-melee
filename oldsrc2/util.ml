(* util.ml
   Basic utility junk and global vars.
   
   Simon Heath
*)

open Sdlvideo;;

let screenx = ref 800;;
let screeny = ref 600;;
let logscreenx = ref 0.;;
let logscreeny = ref 0.;;
let logscreenw = ref 0.;;
let logscreenh = ref 0.;;



let timecompress = ref 1;;
let gravity = ref (-5.);;
let gravlimit = ref (-300.);;



let print_bool = function
    true -> print_string "true"
  | false -> print_string "false"
;;

let error x y = 
  Printf.eprintf x y;
  exit 1;
;;


let pi = acos (-1.0);;
let d2r x = x *. (pi /. 180.);;
let r2d x = x *. (180. /. pi);;
let absf x =
  if x < 0. then
    -.x
  else
    x
;;

(* This kinda sucks, but it works. *)
let round x =
  let a, b = modf x in
    if a < 0.5 then
      b
    else
      ceil x
;;


let absmod x y = 
  let n = x mod y in
    abs n
;;

let incf x =
  x := !x +. 1.;;

let decf x = 
  x := !x -. 1.;;


let x2screen gx scrx objw =
  (int_of_float ((gx -. scrx +. (float_of_int (!screenx / 2))) -. (objw /. 2.)))
;;

let y2screen gy scry objh =
  (int_of_float ((-1. *. gy +. scry +. (float_of_int (!screeny / 2))) -. (objh /. 2.)))
;;


let removeNth lst n =
  let rec loop n lst = 
    if n = 0 then
      List.tl lst
    else 
      (List.hd lst) :: (loop (n - 1) (List.tl lst))
  in
    if List.length lst > n then
      raise (Failure "removeNth: list too long")
    else
      loop n lst
;;


let square x =
  x *. x
;;

(* Gives the max distance covered at warp x
   Distance is measured in 10th's of light-years.
 *)
let warpdist x =
  x * x * 10
;;

(* Return true if a is equal to b within the given delta *)
let within a b delta =
  absf (a -. b) > delta
;;



(* Why can't I just chop the first or last x characters from a string,
   easily?
*)
let chop_left s i =
  let ns = String.create ((String.length s) - i) in
    for x = i to ((String.length s) - 1) do
      ns.[x - i] <- s.[x]
    done;
    ns
;;

let chop_right s i =
  String.sub s 0 ((String.length s) - i)
;;

(* Trims whitespace from the beginning and end of a string *)
let trim s =
  let stptr = ref 0 
  and endptr = ref ((String.length s) - 1)
  in
    while s.[!stptr] = ' ' || s.[!stptr] = '\t' || s.[!stptr] = '\n' do
      incr stptr;
    done;
    while s.[!endptr] = ' ' || s.[!endptr] = '\t' || s.[!endptr] = '\n' do
      decr endptr;
    done;
    let strlen = !endptr - !stptr + 1 in
      String.sub s !stptr strlen
;;

(* Returns true if the two given rects overlap on the screen *)
let rectsOverlap r1 r2 =
  let left1 = r1.r_x
  and left2 = r2.r_x
  and top1 = r1.r_y
  and top2 = r2.r_y
  and right1 = r1.r_x + r1.r_w
  and right2 = r2.r_x + r2.r_w
  and bottom1 = r1.r_y + r1.r_h
  and bottom2 = r2.r_y + r2.r_h in
    if bottom1 < top2 then
      false
    else if top1 > bottom2 then
      false
    else if right1 < left2 then
      false
    else if left1 > right2 then
      false
    else
      true
;;


(* Takes four points denoting the ends of two lines.
   Returns true if the lines intersect.
*)
let doLinesIntersect p1x p1y p2x p2y p3x p3y p4x p4y =
  (* Takes two points denoting the ends of a line.
     Returns a tuple of (slope, intercept)
  *) (*
  let getLineEquation p1x p1y p2x p2y =
    let slope = 
      if p1x <> p2x then ((p2y -. p1y) /. (p2x -. p1x))
      else 10E10 in
    let intercept = p1y -. (slope *. p1x) in
      (slope, intercept)
  in *)
    (*
      Since we have a real infinity, we can use that... *)
  let getLineEquation p1x p1y p2x p2y =
    let slope = (p2y -. p1y) /. (p2x -. p1x) in
      let intercept = p1y -. (slope *. p1x) in
	(slope, intercept)
  in
    
  let m1, b1 = getLineEquation p1x p1y p2x p2y
  and m2, b2 = getLineEquation p3x p3y p4x p4y in
  let x = round ((b2 -. b1) /. (m1 -. m2)) in
  let y = m1 *. x +. b1 in
    x >= (min p1x p2x) && x <= (max p1x p2x) 
    && x >= (min p3x p4x) && x <= (max p3x p4x)
    && y >= (min p1y p2y) && y <= (max p1y p2y)
    && y >= (min p3y p4y) && y <= (max p3y p4y)				  
;;



let arrayAdd arr itm =
  Array.append arr [|itm|]
;;

let arrayMoveTo arr fromidx toidx =
  let fromidx = fromidx mod (Array.length arr) in
  let toidx = toidx mod (Array.length arr) in
  let itm = arr.(fromidx) in
    if fromidx < toidx then (
      for x = fromidx to toidx do
	arr.(x) <- arr.(x + 1);
      done;
      arr.(toidx) <- itm;
    )
    else (
      for x = fromidx downto toidx do
	arr.(x) <- arr.(x - 1);
      done;
      arr.(toidx) <- itm;
    )
;;

let arrayEnd arr =
  Array.length arr - 1
;;
