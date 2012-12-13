(* weapon.ml
   A weapon object.
   Pretty basic for now.  Later on we could end up with a whole object
   heirarchy: bolt, beam, missile, fighter...

   Simon Heath
   3/6/2005
*)

open Resources;;
open Gameobj;;
open Util;;
open Particles;;

(* Different types of weapons: bolts, rays, missiles... *)
(* XXX: Collision handling should be overridden so it does line-to-box 
   intersection testing instead of bounding-box
   XXX: Beam weapons?
*)
class shot cfgfile =
  let cfg = getConfig cfgfile in
object (self)
  inherit gameobj cfgfile as super
  val mutable timeToGo = cfg#getInt "shot" "lifetime"
  val mutable damage = cfg#getInt "shot" "damage" 
  val mutable speed = cfg#getFloat "shot" "speed"

  val mutable refireTime = cfg#getInt "shot" "refire"

  method getRefireTime = refireTime
  method setRefireTime x = refireTime <- x
					  
  method impact (o : gameobj) =
    o#damage damage;
    Printf.printf "Object took %d damage and has %d life left\n" damage
      o#getHits;
    self#die;

  method calculate t =
    super#calculate t;
    timeToGo <- timeToGo - t;
    if timeToGo < 0 then
      self#die;


  method getSpeed = speed
		      
  (* This doesn't set magnitude... maybe it shouldn't.  *)
  method setSpeed s = speed <- s

end;;


class guidedshot cfgfile tgt =
object (self)
  inherit shot cfgfile as super
  val mutable target = tgt

(* We guide the shot towards the given target... *)
  method calculate t =
    super#calculate t

end;;

(* KISS...  
   Do Wing-Commander-style weapon groups.
   ...um.  Weapons together just as a count makes it convenient for
   positioning and grouping, but very NOT for refire and cycle-fire.
   So screw the cycle fire.  Hah, two weapons means refire is halved!
   Hah!  We CAN emulate linked fire, by just firing one shot but multiplying
   the damage it does...  Dunno if it's worthwhile, but it's certainly
   possible.
*)
class weapon cfgfile =
object (self)
  val mutable shot = cfgfile
  val mutable nextFireTime = 0
  val mutable refireTime = (getConfig cfgfile)#getInt "shot" "refire"
  val name = (getConfig cfgfile)#getStr "shot" "name"
  val mutable count = 1

  method getName = name
  method getConfig = cfgfile

  method getCount = count
  method setCount c = count <- c
  method addWeapon = count <- count + 1
  method remWeapon = if count > 1 then count <- count - 1

  method getShot = shot
  method setShot s = shot <- s

  method getRefire = refireTime
  method setRefire t = refireTime <- t
  method refireOK = (Sdltimer.get_ticks ()) > nextFireTime

  (* We do this so that reload time only gets checked when someone tries to 
     fire the weapon.
     Note that weapons will never fire twice per frame...  This is
     technically incorrect, but also means we don't have to worry about
     shots being created in the same place.
  *)
  method makeShots dir mag currentt =
    if currentt > nextFireTime then (
      nextFireTime <- currentt + (refireTime / count);
      let s = new shot shot in
	s#getVector#set_by_dirmag dir (s#getSpeed +. mag);
	[s]
    )
    else
      []

end;;



(* This is a class that contains and handles weapon groupings. 
   It also handles all the logic for cycling between weapons, and checking
   reload times...

   Ummmm.  Fing is, y'see, we could make it so each group is just an
   immutable offset, but that'd cap the number of weapons in a group.

   Blah.  You know, we CAN just do it the WC3 way... weapons grouped by
   type.  Then we the modes we have are single, linked, cycle and all.
   KISS.

   XXX: Ummmm.  How do we keep weapon groups from shooting each other down??
   Just don't worry about it?  Probably.  Either that or make a small 
   deviation in the origin of each weapon...
*)
(*
class weapon cfgfile =
object (self)
  val mutable group = 0
  val mutable shot = cfgfile
  val mutable lastFired = 0
  val mutable refireTime = (getConfig cfgfile)#getInt "shot" "refire"
  val mutable xOffset = 0.
  val mutable yOffset = 0.

  method getGroup = 
    group
  method setGroup x = 
    group <- x

  method getX = xOffset
  method getY = yOffset
  method setX x = xOffset <- x
  method setY y = yOffset <- y
  method setXY x y = 
    xOffset <- x; 
    yOffset <- y

  method getShot = shot
  method setShot s = shot <- s

  method getRefire = refireTime
  method setRefire t = refireTime <- t

  (* We do this so that reload time only gets checked when someone tries to 
     fire the weapon *)
  method makeShots x y dir mag currentt =
    if (currentt - lastFired) > refireTime then (
      lastFired <- currentt;
      let s = new shot shot in
	s#moveTo (x +. xOffset) (y +. yOffset);
	s#getVector#set_by_dirmag dir (s#getSpeed +. mag);
	[s]
    )
    else
      []

end;;




class weaponBunch =
object (self)

  val mutable weapons = [||]
  val mutable hardpointX = []
  val mutable hardpointY = []

  val mutable fireMode = Single
  val mutable currentGroup = 0
  val mutable currentWeapon = 0

  method getCurrentGroup =
  method getCurrentWeapon =
  method setCurrentWeapon w =
  method setCurrentGroup g =

  method getNthGroup n = 
  method getNthWeapon n w = 

  method getNextGroup = 

  method getNextWeapon = 

  method getPrevGroup = 

  method getPrevWeapon = 


  method cycleToNext = 
    match fireMode with
	Single -> ()
      | Linked ->
      | Grouplinked -> 
      | Group -> ()
      | All -> ()

  method moveToNext = 



  (* More complicated... grr. *)
  method moveToPrev = 



  (* Fires whichever weapons are appropriate.
     Returns a tuple, (newenergy * [list of shot objects]) *)
  method fire energy =
    match fireMode with
	Single -> ()
      | Linked -> ()
      | Grouplinked -> ()
      | Group -> ()
      | All -> ()

  (* Fires a specific weapon.
     Returns a tuple, (newenergy * shot object list)
     A single weapon can fire more than one shot at a time, after all. *)
  method fireWeapon g w e =
    ()

  method getMode = fireMode
  method setMode m = fireMode <- m

  method moveWeaponTo fromgroup wepidx togroup wepidx2 =
    ()

  method addWeaponTo group wep =
    ()

  method removeWeaponFrom group wep =
    ()

end;;

*)


(*
class weaponBunch =
  and numGroups = 6 in
object (self)

  val mutable weapons = Array.make numGroups [||]
  val mutable hardpointX = Array.make numGroups 0
  val mutable hardpointY = Array.make numGroups 0

  val mutable fireMode = Single
  val mutable currentGroup = 0
  val mutable currentWeapon = 0

  method getCurrentGroup = weapons.(currentGroup)
  method getCurrentWeapon = weapons.(currentGroup).(currentWeapon)
  method setCurrentWeapon w = currentWeapon <- w
  method setCurrentGroup g = currentGroup <- g

  method getNthGroup n = weapons.(n)
  method getNthWeapon n w = weapons.(n).(w)

  method getNextGroup = 
    weapons.((currentGroup + 1) mod numGroups)

  method getNextWeapon = 
    weapons.(currentGroup).((currentWeapon + 1) mod 
			    (Array.length weapons.(currentGroup)))

  method getPrevGroup = 
    weapons.(abs (currentGroup - 1))

  method getPrevWeapon = 
    weapons.(currentGroup).(abs (currentWeapon - 1))


  method cycleToNext = 
    match fireMode with
	Single -> ()
      | Linked -> currentWeapon <- (currentWeapon + 1) mod 
	  (Array.length weapons.(currentGroup))
      | Grouplinked -> 
	  let nextWep = 
	    (currentWeapon + 1) mod (Array.length weapons.(currentGroup)) in
	  let nextGroup = 
	    if nextWep = 0 then
	      (currentGroup + 1) mod (Array.length weapons)
	    else currentGroup
	  in
	    currentWeapon <- nextWep;
	    currentGroup <- nextGroup;
      | Group -> ()
      | All -> ()

  method moveToNext = 
    let nextWep = 
      (currentWeapon + 1) mod (Array.length weapons.(currentGroup)) in
    let nextGroup = 
      if nextWep = 0 then
	(currentGroup + 1) mod (Array.length weapons)
      else currentGroup
    in
      currentWeapon <- nextWep;
      currentGroup <- nextGroup;


  (* More complicated... grr. *)
  method moveToPrev = 
    let prevWep = currentWeapon - 1 in
    let prevGroup =
      if prevWep < 0 then
	currentGroup - 1
      else
	currentGroup
    in
      if prevGroup < 0 then
	currentGroup <- (numGroups - 1)
      else
	currentGroup <- prevGroup
      

      (abs (currentWeapon - 1)) mod (Array.length weapons.(currentGroup)) in
    let nextGroup = 
      if nextWep = 0 then
	(currentGroup + 1) mod (Array.length weapons)
      else currentGroup
    in
      currentWeapon <- nextWep;
      currentGroup <- nextGroup;


  (* Fires whichever weapons are appropriate.
     Returns a tuple, (newenergy * [list of shot objects]) *)
  method fire energy =
    match fireMode with
	Single -> ()
      | Linked -> ()
      | Grouplinked -> ()
      | Group -> ()
      | All -> ()

  (* Fires a specific weapon.
     Returns a tuple, (newenergy * shot object list)
     A single weapon can fire more than one shot at a time, after all. *)
  method fireWeapon g w e =
    ()

  method getMode = fireMode
  method setMode m = fireMode <- m

  method moveWeaponTo fromgroup wepidx togroup wepidx2 =
    ()

  method addWeaponTo group wep =
    ()

  method removeWeaponFrom group wep =
    ()

  method updateReloadTimes t =
    ()

end;;
*)
