(* drawing.ml
   Line, circle and polygon drawing functions.
   Vector graphics, basically.

   TODO: 
     Draw with alpha
     Draw circles
     Draw filled circles?
     Draw polygons/triangles?
     Draw filled polygons/triangles?


*)

open Sdlvideo


(* Check to make sure we're in bounds of the edges of the screen.
   XXX: This is imperfect.  Check edge-cases, literally...
   It's not too bad though.

   But basically, if you try to draw a shape that's off the screen,
   it'll draw a line along the edge of the screen.  Not exactly ideal.
*)

let correctBoundsX scr x =
  let maxx, _, _ = surface_dims scr in
    if x < 0 then
      0
    else if x > maxx then
      maxx
    else
      x
;;

let correctBoundsY scr y =
  let _, maxy, _ = surface_dims scr in
    if y < 0 then
      0
    else if y > maxy then
      maxy
    else
      y
;;


let drawVLine surf x y1 y2 col =
  let x = ref (correctBoundsX surf x)
  and y1 = ref (correctBoundsY surf y1)
  and y2 = ref (correctBoundsY surf y2) in

  if must_lock surf then
    lock surf;

  (* A bit of wonkiness to account for possible negative values *)
  let dy = abs (!y1 - !y2) in
  let sy = (min !y1 !y2) in  (* Starting y *)
  let ey = sy + dy in        (* Ending y   *)
    for n = sy to ey do
      put_pixel_color surf ~x: !x ~y: n col;
    done;

  if must_lock surf then
    unlock surf
;;

let drawHLine surf x1 x2 y1 col =
  let x1 = ref (correctBoundsX surf x1)
  and x2 = ref (correctBoundsX surf x2)
  and y1 = ref (correctBoundsY surf y1) in

  if must_lock surf then
    lock surf;

  (* A bit of wonkiness to account for possible negative values *)
  let dx = abs (!x1 - !x2) in
  let sx = (min !x1 !x2) in  (* Starting x *)
  let ex = sx + dx in        (* Ending x   *)
    for n = sx to ex do
      put_pixel_color surf ~x: n ~y: !y1 col;
    done;

  if must_lock surf then
    unlock surf
;;

(* Draws an outline of a rectangle *)
let drawRect surf x y w h col =
  drawHLine surf x (x + w) y col;
  drawHLine surf x (x + w) (y + h) col;
  drawVLine surf x y (y + h) col;
  drawVLine surf (x + w) y (y + h) col;
;;


let drawFilledRect surf x y w h col =
  let r = {r_x = x; r_y = y; r_w = w; r_h = h} in
   fill_rect ~rect: r surf (map_RGB surf col)
;;


(* This is a much-tuned (much-hacked) line-drawing algorithm.
   It's probably not the fastest, but it draws lines EQUALLY WELL no matter
   what orientation they're in.
*)
let drawLine surf x1 y1 x2 y2 col =
  let x1 = correctBoundsX surf x1
  and x2 = correctBoundsX surf x2
  and y1 = correctBoundsY surf y1
  and y2 = correctBoundsY surf y2 in
  let dx = x1 - x2
  and dy = y1 - y2 in
  let m = (float_of_int dy) /. (float_of_int dx)
  and xc = ref (float_of_int x1)
  and yc = ref (float_of_int y1) in

    (* Check for easy cases: Vertical and horizontal lines *)
    if m = infinity or m = -.infinity then
      drawVLine surf x1 y1 y2 col
    else if m = 0. then
      drawHLine surf x1 x2 y1 col;


  if must_lock surf then
    lock surf;

  (* Check for slopes greater than 45 degrees and compensate to prevent
     dotty lines.  If slope < 45, we iterate along x and calculate the y 
     points.  Else, we iterate along y and calculate the x points. *)
  if m > 1. or m < -.1. then
    (* This recalculates the slope as x/y.  This isn't something you'll find
       in any geometry class, but it works. *)
    let m = (float_of_int dx) /. (float_of_int dy) in
      (* These inner if's check whether dy is less than 0; if it is, we
	 need to basically run the loop backwards for it to work.  *)
      if dy < 0 then
	(* Each of these while loops just runs slightly differently.  Lots
	   of trial and error and figuring went into them.
	   This one counts upward, calculating x from y *)
	while (int_of_float !yc) < y2 do
	  put_pixel_color surf 
	    ~x: (int_of_float !xc) ~y: (int_of_float !yc) col;
	  Util.incf yc;
	  xc := !xc +. m;
	done
      else
	(* This one counts downward, calculating x from y *)
	while (int_of_float !yc) > y2 do
	  put_pixel_color surf 
	    ~x: (int_of_float !xc) ~y: (int_of_float !yc) col;
	  Util.decf yc;
	  xc := !xc -. m;
	done
  else (* if m > 1. or m < -.-1 *)  
    (* This is exactly the same as above, calculating y from x though. *)
    if dx < 0 then
      (* This loop counts upward, calculating y from x *)
      while (int_of_float !xc) < x2 do
	put_pixel_color surf 
	  ~x: (int_of_float !xc) ~y: (int_of_float !yc) col;
	Util.incf xc;
	yc := !yc +. m;
      done
    else
      (* This loop counts downward, calculating y from x *)
      while (int_of_float !xc) > x2 do
	put_pixel_color surf 
	  ~x: (int_of_float !xc) ~y: (int_of_float !yc) col;
	Util.decf xc;
	yc := !yc -. m;
      done;
  
  if must_lock surf then
    unlock surf;
;;



