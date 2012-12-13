(* particles.ml
   Particle system.
   Particles would be: smoke, flame, shots maybe, snow, rain, clouds/fog?,
   drifting/floating anything...
   They generally don't interact.  In fact, making them interact would be
   damn hard.  Just there to look pretty.

   Simon Heath
   13/4/2005
*)

open Gameobj;;


type particle = {
  obj : gameobj;
  behavior : (gameobj -> int -> unit);
}


class particleSystem =
  
object (self)
  val mutable particles = []

  method addNewParticle gameobjfile behavior =
    let newpart = {obj = new gameobj gameobjfile; 
		   behavior = behavior} in
    particles <- newpart :: particles
    
  method addParticle part =
    particles <- part :: particles

  method calc t =
    particles <- List.filter (fun x -> x.behavior x.obj t; x.obj#isAlive) 
                             particles;
    (*List.iter (fun x -> x.obj#calculate 1) particles;*)


  method draw scr =
    List.iter (fun x -> x.obj#draw scr !Util.logscreenx !Util.logscreeny)
      particles
end;;


let globalParticles = new particleSystem;;

