(* area.ml
   A class for an "area", which is anything that takes up space.

   Simon Heath
*)

open Config;;
open Resources;;

let notColliding = 0
and top = 1
and right = 2
and bottom = 3
and left = 4;;


class area cfgfile =
  let cfg = Resources.getConfig cfgfile  in
object (self)

  val mutable x = cfg#getFloat "area" "x"
  val mutable y = cfg#getFloat "area" "y"
  val mutable w = cfg#getFloat "area" "w"
  val mutable h = cfg#getFloat "area" "h"
  val mutable tangible = cfg#getBool "area" "tangible"


  method getx = x
  method gety = y
  method getw = w
  method geth = h
  method istangible = tangible

  method setx n = x <- n
  method sety n = y <- n
  method setw n = w <- n
  method seth n = h <- n
  method settangible n = tangible <- n
  method moveto nx ny =
    x <- nx;
    y <- ny

  method setsize nw nh =
    w <- nw;
    h <- nh

(* Old collision detection algorithm using bounding-boxes.
   Not quite right...
*)

  method isColliding (ar : area) =
    if tangible && ar#istangible then
      let left1 = x
      and left2 = ar#getx
      and bottom1 = y
      and bottom2 = ar#gety
      and right1 = x +. w
      and right2 = ar#getx +. ar#getw
      and top1 = y +. h
      and top2 = ar#gety +. ar#geth in
	if bottom1 > top2 then
	  false
	else if top1 < bottom2 then
	  false
	else if right1 < left2 then
	  false
	else if left1 > right2 then
	  false
	else
	  true
    else
      false


  (* Randomizes an area within certain constraints *)
  method randomize conx cony =
    let rndx = Random.float conx
    and rndy = Random.float cony
    in
      x <- x +. rndx -. (rndx /. 2.);
      y <- y +. rndy -. (rndy /. 2.)


(* The following two methods are experimental, involving the collision
   detection system.  The idea is that after an object has hit another object,
   it is moved so it does NOT OVERLAP with the second object.  This hopefully
   lets you prevent things from "pushing through" other things...

   Though you can really just prevent it by capping max acceleration...
*)
  (* Returns where on the arg the current object is hitting *)
  method collidingWhere (ar : area) =
    if self#isColliding ar then
      let left1 = x +. (w *. 0.1)
      and left2 = ar#getx +. (ar#getw *. 0.1)
      and bottom1 = y +. (h *. 0.1)
      and bottom2 = ar#gety +. (ar#geth *. 0.1)
      and right1 = x +. (w *. 0.9)
      and right2 = ar#getx +. (ar#getw *. 0.9)
      and top1 = y +. (h *. 0.9)
      and top2 = ar#gety +. (ar#geth *. 0.9) in
	if bottom1 < top2 then
	  top
	else if top1 > bottom2 then
	 bottom
	else if right1 > left2 then
	  left
	else if left1 < right2 then
	  right
	else
	  notColliding
    else
      notColliding

  method moveAwayFrom (ar : area) =
    let l = self#collidingWhere ar in
      if l = notColliding then
	()
      else if l = top then
	y <- ar#gety +. ar#geth *. 0.9
      else if l = bottom then
	y <- ar#gety +. ar#geth *. 0.1
      else if l = left then
	x <- ar#getx +. ar#getw *. 0.1
      else if l = right then
	x <- ar#getx +. ar#getw *. 0.9
      else
	()


  method print =
    Printf.printf "Area: x: %f y: %f w: %f h: %f: tangible: %b\n"
      x y w h tangible




end;;
