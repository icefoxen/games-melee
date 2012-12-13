(* realms.ml
   This holds generalized data structures and manipulation functions
   for the Realms, id est gameplay areas of a certain size.  Realms
   have objects, shots, etc, and their main task is to keep track of all
   objects and do all the necessary calculations.
   It also takes care of meta-game stuff like input, background, music,
   effects, timing...

   XXX: Music, sounds...

   Simon Heath
   4/6/2005
*)

open Resources;;
open Util;;
open Sdlvideo;;
open Sdlkey;;
open Gameobj;;
open Particles;;
open Drawing;;


let fillColor surf col =
   fill_rect ~rect: (surface_info surf).clip_rect surf (map_RGB surf col)
;;

let centerScreenOnObj (o : Ship.ship) =
  logscreenx := o#getArea#getx;
  logscreeny := o#getArea#gety;
;;




(* This UTTERLY ROCKS, but I have to make it work sanely and flexibly. *)
let newDeathParticle x y = 
  let p = { obj = new gameobj "thrustparticle.cfg";
	    behavior = fun x t -> x#calculate t; x#damage 1; } in
    p.obj#moveTo (x +. (Random.float 100.) -. 25.) (y +. (Random.float 100.) -. 25.);
    p.obj#acceldm (Random.float 360.) 10.;
    globalParticles#addParticle p;
;;


(* Oooohkay.
   Well.  What we do is, we take a large image (twice as big as the screen
   in all dimensions) and simply scan the source rect back and forth over it
   as we move around.  It's a bit of a kludge, and not as nice as a Real
   Tiled Background, but by gods, it works!
   An image has to be utterly perfect or it'll jump when it wraps, though.
   Twice as big as the screen in all dimensions, a tile of four images.
   XXX: It would be nice if this could be a Real Tiled Background, but...
   Well, it ain't broke, so I won't worry about fixing it till I have to.
*)
let drawBackground bkg scr =
  let src = {r_x = 0; r_y = 0; r_w = 800; r_h = 600;} in
    if !logscreenx > 0. then
      src.r_x <- (int_of_float !logscreenx) mod 800
    else
      src.r_x <- 800 + ((int_of_float !logscreenx) mod 800);


    if !logscreeny > 0. then
      src.r_y <- 600 - (int_of_float !logscreeny) mod 600
    else 
      src.r_y <- - ((int_of_float !logscreeny) mod 600);

    blit_surface ~src: bkg ~src_rect: src ~dst: scr ();

(*
  let dst = {r_x = 0; r_y = 0; r_w = 256; r_h = 256;} in
  for x = 0 to (!screenx / dst.r_w - 1) do
  for y = 0 to (!screeny / dst.r_h - 1) do
  dst.r_x <- x * dst.r_w;
  dst.r_y <- y * dst.r_h;


(*  Shiny!!
  dst.r_x <- ((int_of_float !logscreenx) * x) mod !screenx;
  dst.r_y <- ((int_of_float !logscreeny) * y) mod !screeny;
*)
  blit_surface ~src: bkg ~dst: scr ~dst_rect: dst ();
  done;
  done;
*)

;;



class realm cfgfile =
  let cfg = getConfig cfgfile in
object (self)
  val mutable objects = []
  val mutable player = new Ship.ship "testship.cfg"
  val mutable dimX = cfg#getFloat "realm" "width"
  val mutable dimY = cfg#getFloat "realm" "height"

  val mutable wasthen = 0
  val mutable isnow = 0
  val mutable framecount = 1
  val mutable continue = true
  val mutable debug = cfg#getBool "realm" "debug"
  val mutable framecap = cfg#getBool "realm" "framecap"
  val mutable background = getImg (cfg#getStr "realm" "background")
  val mutable wallToughness = cfg#getFloat "realm" "wallToughness"
  val mutable speedCap = 
    (* Make sure the speed cap is sane; we have a limit of 2^15 because of
       packet size *)
    let s = cfg#getFloat "realm" "speedCap" in
    let s = (min s (2. ** 15.)) in
    let s = (max s 1.) in s

  val mutable nextWepPressed = false
  val mutable prevWepPressed = false


  method getObjects = objects

  method setObjects (o : gameobj list) = objects <- o

  method addObject o = objects <- o :: objects

  method addObjects olst = objects <- olst @ objects

  method setPlayer p = player <- p
  method getPlayer = player

  method doCalc dt =
    let t = float_of_int dt in
      (* We have "soft" boundaries; for each distance d the player
	 goes beyond them, they get d/2 acceleration in the opposite
	 direction.  No ship should be able to continually go past them.
	 Note that this DOES let you get to very high speeds by pin-balling
	 back and forth.
      *)
    let doReverseAccel o =
      if o#getX > dimX then
	o#acceldm 270. ((o#getX -. dimX) *. t /. 2000.)
      else if o#getX < -. dimX then
	o#acceldm 90. ((-. dimX -. o#getX) *. t /. 2000.);
      if o#getY > dimY then
	o#acceldm 180. ((o#getY -. dimY) *. t /. 2000.)
      else if o#getY < -. dimY then
	o#acceldm 0. ((-. dimY -. o#getY) *. t /. 2000.);
    in

      List.iter (fun x -> doReverseAccel x; x#calculate dt) objects;
      Particles.globalParticles#calc dt;

  method doCollide t =
    let rec loop (current : Gameobj.gameobj) lst =
      if lst = [] then
	()
      else (
	List.iter (fun x -> 
		     if current#isColliding x t then (
		       print_endline "Boom!";
		       current#impact x;
		     )
		  ) lst;
	loop (List.hd lst) (List.tl lst);

      )
    in
      loop (List.hd objects) (List.tl objects)
	

  method doDrawing scr =
    fillColor scr black;
    
    drawBackground background scr;

    Particles.globalParticles#draw scr;
    List.iter (fun x -> x#draw scr !logscreenx !logscreeny) objects;
    if debug then
      List.iter (fun x -> x#drawMeta scr !logscreenx !logscreeny) objects;
    self#doGui scr;

  method doGui scr =
    (* Draw framerate stuff *)
    if debug then
      Text.drawTextAbs 
	(Printf.sprintf 
	   "lastupdate: %d updatetime: %d time to draw: %dms fps: %2.2f" 
	   (wasthen) (isnow) (isnow - wasthen) 
	   (1000. /. (float_of_int (Sdltimer.get_ticks () / framecount))))
	"Arial.ttf" red 5 5 scr;
    (* Draw weapon display *)
    let w = player#getWeapons in
    let displayheight = (Array.length w * 15) + 10 in
      drawVLine scr 600 1 displayheight green;
      drawVLine scr 799 1 displayheight green;
      drawHLine scr 600 799 1 green;
      drawHLine scr 600 799 displayheight green;

      for x = 0 to (Array.length w) - 1 do
	Text.drawTextAbs w.(x)#getName "Arial.ttf" green 620 (x * 15) scr;
	if player#getCurrentWeaponIndex = x then
	  let color = 
	    if player#getCurrentWeapon#refireOK then blue
	    else red in
	    drawFilledRect scr 610 ((x * 15) + 10) 5 5 color;
      done;

      (* Draw coordinate display *)
      Text.drawTextAbs (Printf.sprintf "X: %0.0f  Y: %0.0f" player#getX player#getY) "Arial.ttf" green 600 400 scr;

  method doInput dt =
    ignore (Sdlevent.poll());
    if (is_key_pressed !Input.menu) or (is_key_pressed KEY_q) then
      continue <- false;

    if (is_key_pressed !Input.turnright) then
      player#turnRight (float_of_int dt);

    if (is_key_pressed !Input.turnleft) then
      player#turnLeft (float_of_int dt);

    if (is_key_pressed !Input.thrust) then
      player#thrust (float_of_int dt);

    if (is_key_pressed !Input.fire) then
      self#addObjects (player#makeShot : 
			 Weapon.shot list :> Gameobj.gameobj list);

    (* A bit of cleverness to only register key-down's... *)
    if (is_key_pressed !Input.nextWep) then
      if not nextWepPressed then (
	player#nextWeapon;
	nextWepPressed <- true;
      )
      else
	()
    else
      nextWepPressed <- false;

    if (is_key_pressed !Input.prevWep) then
      if not prevWepPressed then (
	player#prevWeapon;
	prevWepPressed <- true;
      )
      else
	()
    else
      prevWepPressed <- false;

    if (is_key_pressed !Input.special) then
      ()


(* This is now done in doCalc so it only goes through the list once
  method doEdges dt =
    let t = float_of_int dt in
    let doReverseAccel o =
      if o#getX > dimX then
	o#acceldm 270. ((o#getX -. dimX) *. t /. 1000. *. wallToughness)
      else if o#getX < -. dimX then (
	o#acceldm 90. ((-. dimX -. o#getX) *. t /. 1000. *. wallToughness);
      );
      if o#getY > dimY then
	o#acceldm 180. ((o#getY -. dimY) *. t /. 1000. *. wallToughness)
      else if o#getY < -. dimY then (
	o#acceldm 0. ((-. dimY -. o#getY) *. t /. 1000. *. wallToughness);
      )
    in
      List.iter doReverseAccel objects;
*)

  method doDeath =
    let alivelst, deadlst = List.partition (fun x -> x#isAlive) objects in
      objects <- alivelst;
      (* Okay, each class can just override it's own death animation
	 method, gods dammit. *)
      (* List.iter (fun x -> self#addObjects x#makeDeathObjs) deadlst; *)


  method doMainloop scr =
    wasthen <- Sdltimer.get_ticks ();
    isnow <- Sdltimer.get_ticks ();
    self#addObject (player : Ship.ship :> Gameobj.gameobj);
    self#addObject (new gameobj "testobj.cfg");
    
    player#addWeapon "testweapon1.cfg";
    player#addWeapon "testweapon2.cfg";
    player#addWeapon "testweapon.cfg";

    let addStuffIncr = ref 15000 in

      while continue do
	framecount <- framecount + 1;

	let dt = (Sdltimer.get_ticks ()) - wasthen in
	  self#doCalc dt;
	  centerScreenOnObj player;

	  self#doInput dt;

	  (* Not terribly efficient, since we iterate over the list four
	     times, buuut... *)

	  self#doCollide dt;
	  self#doDeath;
	  self#doDrawing scr;

	  (* Make a bit of dynamicism, just for now *)
	  (*
	    addStuffIncr := !addStuffIncr - dt;
	    if !addStuffIncr < 0 then (
	    addStuffIncr := 15000;
	    let x = new Gameobj.gameobj "testobj.cfg" in
	    x#moveTo (Random.float 300.) (Random.float 300.);
	    self#addObject x;
	    );
	  *)

	  (* XXX: Um, might this want to be after the framecap? 
	     And isnow seems superfluous.
	  *)
	  wasthen <- isnow;
	  isnow <- Sdltimer.get_ticks ();
	  
	  (* 60 is desired fps *)
	  if framecap then
	    while ((Sdltimer.get_ticks ()) - wasthen) < (1000 / 60) do
	      ()
	    done;
	  flush stdout;
	  flip scr;
      done;

end;;



(*
open Resources;;
open Util;;

let realms = Hashtbl.create 24;;

let doubleArray n =
  let arrlen = Array.length n 
  in
    Array.init (arrlen * 2) 
      (fun x ->
	 if x <= arrlen - 1 then
	   n.(x)
	 else
	   n.(0))
;;

  

class realm table =
  let c = Util.config in
  let no = c#getInt (table ^ ".numobjects") in

object (self)
  (* Have a realm config file that lists the 
     configs for all the objects in it?  I'd need LISTS for config stuff
     then...  *)
  (* Or the TYPES of objects and then gives PARAMETERS for them.
     Scheme helps? *)
  val mutable numobjects = c#getInt (table ^ ".numobjects")
  val mutable objects = 
    Array.init no (fun x -> new Spaceobj.spaceobj "rock")

  val mutable sizex = c#getInt (table ^ ".sizex")
  val mutable sizey = c#getInt (table ^ ".sizey")

  val mutable name = c#getStr (table ^ ".name")
  val mutable flags = () (* new Flags.flags "flags.cfg" *)

(* ACTUALLY, for the background and foreground layers, I can
   just have scrolling bitmaps!  Hahaha, that's hilarious, 'cause
   it's so dumb and it'll probably work so well!  Then I just make the far
   background scroll half as fast as the near background and it's perfect!

   The scrolling ish a bit tricky though...  Do we want it happen in
   Gameworld or Screenworld?  Screenworld, it'll have to be, to do 
   different parallax rates.  Makes it tricky though.
   But ah-ha!  That's what we have sprite objects for!  W00t!
   Now, how do we make it infinate?  We could just dynamically tile it,
   or we could do some clever blit-chopping...
   The latter might be faster and certainly cleverer, but possibly
   more complex as well...
*)
 
  (* Background stars layer 1 *)
(* Okay.  To scroll the background, we need to check whether the screen
   is completely in it's bounds.  If it is, we're cool.
   Else, we must take the missing part from the far edge of it and jam it
   in, and if the missing edge is over the size of the screen, move the
   background sprite.

   Might it be simpler to simply have a background surface and view the
   screen rect as scrolling over it?  Well, that's really what happens, so...
*)
  val mutable layer1 = new Sprite.background "pipebkg"

  (* Background stars layer 2 *)
  val mutable layer2 =
    Array.init 1000 (fun x -> new Spaceobj.spaceobj "star")

  (* Actor layer *)
  val mutable layer3 = 
    Array.init no (fun x -> new Spaceobj.spaceobj "rock")

  (* Effects layer --shields, engine, etc *)
  val mutable layer4 = 
    Array.init no (fun x -> new Spaceobj.spaceobj "dummy")

  (* Light/fog layer *)
  val mutable layer5 = 
    Array.init no (fun x -> new Spaceobj.spaceobj "dummy")

  (* GUI/HUD layer *)
  val mutable layer6 = 
    Array.init no (fun x -> new Spaceobj.spaceobj "dummy")



  method getObjects = objects
  method getNumObjects = numobjects
  method setObjects n =
    objects <- n

  method setNumObjects n =
    numobjects <- n

  (* Ten to one there's a fencepost error here, but oh well.  *)
  method addObject obj =
    numobjects <- numobjects + 1;
    if (Array.length objects) <= numobjects then
      objects <- doubleArray objects;
    objects.(numobjects) <- obj

  (* Takes an object, finds it's index.  Linear!! *)
  method findObject n =
    let i = ref 0 in
      while !i < numobjects && objects.(!i) <> n do
	incr i
      done;
      !i
	
  (* Takes an index, returns the object. *)
  method getObject n =
    objects.(n)

  (* Remove the object at i.  Leaves no gaps in the array. *)
  method removeObject i =
    if i < numobjects then (
      numobjects <- numobjects - 1;
      objects.(i) <- objects.(numobjects)
    )

  method drawBkg scr =
    layer1#blit scr;
    

  method drawObjs scr =
    self#drawBkg scr;
    for x = 0 to numobjects - 1 do
      objects.(x)#draw scr !logscreenx !logscreeny
    done;

  method drawBoxes scr =
    for x = 0 to numobjects - 1 do
      objects.(x)#drawBox scr !logscreenx !logscreeny
    done;

  method drawVectors scr =
    for x = 0 to numobjects - 1 do
      objects.(x)#drawVector scr !logscreenx !logscreeny
    done;


  (* Crappy algorithm, but it can handle 100-500 objects easy,
     and it's simple.  Good enough for testing. *)
  (* XXX: Possibilities:
     1) Use nifty closest-point algorithm
     2) Use simple radius-check for locality, THEN if that's within bounds
     do the bounding-box check
     3) Do bitmap collision together with a radius-check.
  *)
  method collideObjs =
    for x = 0 to numobjects - 1 do
      for y = x to numobjects - 1 do
	if objects.(x)#isColliding objects.(y) then (
	  objects.(x)#impact objects.(y);
	)
      done
    done



  method calcObjs t =
    layer1#moveTo (x2screen 0. !logscreenx 1.) 
      (y2screen 0. !logscreeny 1.);
    for x = 0 to numobjects - 1 do
      objects.(x)#calculate (t * !timecompress)
    done
    

end;;


let getRealm name =
  if not (Hashtbl.mem realms name) then
    Hashtbl.add realms name (new realm name);
  Hashtbl.find realms name
;;
*)
