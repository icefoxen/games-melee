(* ship.ml
   A ship object!  Yay!
   Okay, we need to add a couple things.  Specifically:
   Weapon slots
   Possibly more than one exhaust trail and weapon location.
   That might be tricky to put into the config files, a bit...

   Simon Heath
   15/04/2004
*)

open Resources;;
open Gameobj;;
open Util;;
open Particles;;
open Weapon;;
open Net;;

let newThrustParticle x y = 
  let p = { obj = new gameobj "thrustparticle.obj";
	    behavior = fun x t -> x#damage 1; } in
  let x = (Random.float 8.) -. 4. +. x
  and y = (Random.float 8.) -. 4. +. y in

(* A misguided but amusing attempt to give particles death-effects...
   It doesn't work, and should never be necessary. *)
(*    p.obj#setDeathFunc 
      (fun o ->
	 print_endline "Bop!";
	  let deathObj = new gameobj "sparkle.obj" in
	  let part = {obj = deathObj; behavior = fun x -> x#damage 1; print_endline "foo";} in
	    part.obj#moveTo o#getX o#getY;
	    print_endline "Beep!";
	    globalParticles#addParticle part);
*)
			  
    p.obj#moveTo x y;
    globalParticles#addParticle p;
;;

type fireMode =
    (* Fire then switch to the next in the group *)
    Linked
    (* Fire then switch to the next even if it's not in the same group *)
  | Grouplinked
    (* Fire all weapons in the group *)
  | Group
    (* Fire all weapons *)
  | All
;;




class ship cfgfile =
  let cfg = getConfig cfgfile in
object (self)
  inherit gameobj cfgfile as super
(*
    (* A bit of a kludge... we search for a death animation and if we find
       one, we use it. *)
  initializer
    let d = cfg#getStr "ship" "deathAnim" in
      if d <> "None" then
	let d o = 
	  let deathObj = new gameobj (cfg#getStr "ship" "deathAnim") in
	  let part = {obj = deathObj; behavior = fun x -> x#damage 1;} in
	    part.obj#moveTo o#getX o#getY;
	    globalParticles#addParticle part
	in
	  deathFunc <- d
*)

  val mutable facing = 0.0
  val mutable thrust = cfg#getFloat "ship" "thrust"
  val mutable turnrate = cfg#getFloat "ship" "turn"

  (* These are offsets for engine trails, so they don't come out of
     the middle of the ship *)
  val mutable thrustX = cfg#getFloat "ship" "thrustX"
  val mutable thrustY = cfg#getFloat "ship" "thrustY"

  val mutable weapons = [|(new weapon (cfg#getStr "ship" "weapon"))|]
  val mutable currentWeapon = 0
  val mutable fireMode = Linked

  val mutable gunX = cfg#getFloat "ship" "gunX"
  val mutable gunY = cfg#getFloat "ship" "gunY"

  method getFacing = facing
  method setFacing x = facing <- x

  method calculate t =
    super#calculate t;

  (* Kinda a kludge, but oh well *)
  method addWeapon cfg =
    let rec exists arr x =
      if x < Array.length arr then
	if arr.(x)#getConfig = cfg then
	  x
	else
	  exists arr (x + 1)
      else
	-1
    in
    let idx = exists weapons 0 in
      if idx = -1 then 
	weapons <- Array.append [|(new weapon cfg)|] weapons
      else
	weapons.(idx)#addWeapon

  (* More of a kludge, since O'Caml for some dumb reason doesn't
     have an Array.filter function *)
  method remWeapon cfg =
    let rec exists arr x =
      if x < Array.length arr then
	if arr.(x)#getConfig = cfg then
	  x
	else
	  exists arr (x + 1)
      else
	raise (Failure "ship#remWeapon")
    in
    let idx = exists weapons 0 in
      if weapons.(idx)#getCount = 1 then
	weapons <- Array.of_list (List.filter (fun x -> x#getConfig <> cfg)
				    (Array.to_list weapons))
      else
	weapons.(idx)#remWeapon

  method getCurrentWeapon = weapons.(currentWeapon)
  method getCurrentWeaponIndex = currentWeapon
  method setCurrentWeapon n =
    currentWeapon <- n mod (Array.length weapons)

  method getWeapons = weapons

  method nextWeapon = 
    currentWeapon <- (currentWeapon + 1) mod (Array.length weapons)

  method prevWeapon =
    currentWeapon <- 
    if currentWeapon = 0 then 
      (Array.length weapons) - 1 else (currentWeapon - 1)

  method getBattery = 0

  method getShield = 0

  method getArmor = 0

  method setFireMode f =
    fireMode <- f

  method nextFireMode =
    match fireMode with
	Linked -> fireMode <- Grouplinked
      | Grouplinked -> fireMode <- Group
      | Group -> fireMode <- All
      | All -> fireMode <- Linked


  (* Take fire mode into account:
     Linked -> Fire rapid shot
     Grouplinked -> Fire and cycle weapon
     Group -> Fire big shot
     All -> Fire rapid shot from all weapons
  *)
  method fire =
    ()


  method makeShot =
    let n = weapons.(currentWeapon)#makeShots facing vector#get_m (Sdltimer.get_ticks ())
    in
    let sx = self#getXOffset gunX
    and sy = (self#getYOffset gunY) in
      List.iter (fun x -> x#moveTo (area#getx +. sx) (area#gety +. sy)) n;
      n
	(*
	  let sx = self#getXOffset gunX
	  and sy = (self#getYOffset gunY) in
	  let shot = new shot gun in
	  weaponReload <- shot#getRefireTime;
	  shot#moveTo (area#getx +. sx) (area#gety +. sy);
	  shot#getVector#set_by_dirmag facing (shot#getSpeed +. vector#get_m);
	  shot
	*)


  method turnLeft time =
    let degrees = turnrate *. time /. 1000. in
      facing <- facing -. degrees;
      if facing < 0. then
	facing <- 360. +. facing;
      self#setSpriteToFacing

  method turnRight time =
    let degrees = turnrate *. time /. 1000. in
      facing <- facing +. degrees;
      if facing > 360. then
	facing <- facing -. 360.;
      self#setSpriteToFacing

  method private setSpriteToFacing =
    let incr = 360 / sprite#getFrameCount in
      sprite#setFrame ((int_of_float facing) / incr)


  (* These methods let you get points that are offset x/y amount
     relative to the center of the sprite, AND which rotate with it.
     Note that the +. (pi /. 2) is a kludge to make the point appear
     90 degrees away from where it normally does.  I should be able to
     do that with pure trig, but at least this is less kludgy than
     what I had before...  *)
  method getXOffset x =
    x *. (cos ((d2r facing) +. (pi /. 2.)))

  method getYOffset y =
    y *. (sin ((d2r facing) +. (pi /. 2.)))

  (*  method isWeaponReloaded =
      weaponReload < 0
  *)
  (*
    method makeDeathObjects =
    []
  *)

  method thrust time =
    (* To offset the thrust points, we just need to get the X and Y
       componants of the wossname and translate 'em... you know what I
       mean.  
    *)
    let thrustXOffset = self#getXOffset thrustX
    and thrustYOffset = self#getYOffset thrustY in
      newThrustParticle (area#getx +. thrustXOffset) 
	(area#gety +. thrustYOffset);
      newThrustParticle (area#getx +. thrustXOffset) 
	(area#gety +. thrustYOffset);
      (*vector#accel_by_dirmag facing thrust *)
      vector#accel_by_dirmag facing (thrust /. mass *. time /. 1000.);


  method drawPoints scr =
    let thrustXOffset = thrustX *. (cos ((d2r facing) +. (pi /. 2.)))
    and thrustYOffset = thrustY *. (sin ((d2r facing) +. (pi /. 2.))) in
      Drawing.drawFilledRect scr (x2screen (area#getx +. thrustXOffset) 
				    !Util.logscreenx 1.)
	(y2screen (area#gety +. thrustYOffset) !Util.logscreeny 1.
	) 3 3 Sdlvideo.blue;

      let gunXOffset = self#getXOffset gunX
      and gunYOffset = self#getYOffset gunY in
	Drawing.drawFilledRect scr (x2screen (area#getx +. gunXOffset) 
				      !Util.logscreenx 1.)
	  (y2screen (area#gety +. gunYOffset) !Util.logscreeny 1.
	  ) 3 3 Sdlvideo.blue;
	
  method drawMeta scr scrx scry =
    super#drawMeta scr scrx scry;
    self#drawPoints scr; 
    

  method toPacket time =
    let n = {
      pe_id = (char_of_int id);
      pe_time = time;
      pe_facing = (int_of_float facing);
      pe_x = (int_of_float area#getx);
      pe_y = (int_of_float area#gety);
      pe_dx = (int_of_float vector#getx);
      pe_dy = (int_of_float vector#gety);
      pe_wep = (char_of_int currentWeapon);
      pe_energy = self#getBattery;
      pe_shield = self#getShield;
      pe_armor = self#getArmor;
      pe_hits = hits;
      pe_wepreload = 0;
      pe_regenproportions = 0;
    } in
      Playere( n )




end;;

