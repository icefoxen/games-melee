(* vector.ml
   A class for a "vector", which is anything that moves.

   Simon Heath
*)

open Util

class vector =
object (self)
  val mutable x = 0.
  val mutable y = 0.
  val mutable friction = 1.0
		    
  (* Get the direction or magnitude of the vector *)
  method get_d = 
    if y = 0. then
      0.
    else 
      let r = r2d (atan (x /. y)) in
	if r > 0. then
	  r
	else
	  360. -. r
	    
  method get_m = sqrt ((x *. x) +. (y *. y))
		   
  (* We sometimes need to set a vector's direction by direction and
     magnitude...  Hm. 
     This is kinda ugly, but since actors only know their heading and
     acceleration, it's the best way.
  *)

  method reverse =
    x <- -. x;
    y <- -. y;

  method set_by_dirmag d m = 
    Printf.printf "x: %f y: %f d: %f m: %f\n" x y d m;
    x <- m *. (sin (d2r d));
    y <- m *. (cos (d2r d));
    Printf.printf "New: x: %f y: %f d: %f m: %f\n" x y self#get_d self#get_m

  method accel_by_dirmag d m =
    x <- x +. (m *. (sin (d2r d)));
    y <- y +. (m *. (cos (d2r d)))

  method getx = x
  method gety = y
  method setx x' = x <- x'
  method sety y' = y <- y'

  (* Adds another vector to this one --acceleration, for instance *)
  method add (v : vector) =
    x <- x +. v#getx;
    y <- y +. v#gety

  method addxy x' y' =
    x <- x +. x';
    y <- y +. y'

  method reverse =
    x <- -. x;
    y <- -. y

  (* These add the vector to a basic offset *)
  (* nx is the base X, x is the offset of the vector, and t is TIME in ms. 
     Yay, this more or less works.  *)
  method movex nx time =
    let n =  nx +. (x *. ((float_of_int time) *. 0.01) *. friction) in
      (*Printf.printf "(vector) Time: %d\n" t; *)
      n

  method movey ny time =
    ny +. y *. ((float_of_int time) *. 0.01) *. friction

  method getFriction = friction
  method setFriction f =
    friction <- f

  (* Sets it to a random direction and magnitude within the given limit. 
     The randomizer MUST have been initialized first!!!
  *)
  method randomize n =
    x <- (Random.float n) -. n /. 2.;
    y <- (Random.float n) -. n /. 2.

  method print =
    Printf.printf "Vector: x: %f y: %f\n" x y
    

end;;


