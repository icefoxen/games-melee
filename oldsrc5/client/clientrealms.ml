(* clientrealms.ml
   This holds generalized data structures and manipulation functions
   for the Realms, id est gameplay areas of a certain size.  Realms
   have objects, shots, etc, and their main task is to keep track of all
   objects and do all the necessary calculations.
   It also takes care of meta-game stuff like input, background, music,
   effects, timing...

   Okay, so this is the client half of the deal.  That means that we
   don't actually control what's going on.  We simulate what we
   THINK is going on, and listen for events from the server.  These events
   will be sent out periodically, and will basically be a bunch of
   information on the position and direction of objects.  When we get those,
   we change the current state to match what it should be.
   We send out input packets, which simply tell what keys are pressed.
   We also have a chat interface in there somewhere.

   XXX:
   Then, server-time synchronization (I think this works), 
   real player-death handling (has to be server-side too),

   XXX: time-based animations, death-effects, shared particles(?), 

   XXX: Music, sounds...

   Simon Heath
   4/6/2005
*)


open Util;;
open Sdlvideo;;
open Sdlkey;;
open Gameobj;;
open Particles;;
open Drawing;;

open Resources;;
open Unix;;
open Net;;


let fillColor surf col =
   fill_rect ~rect: (surface_info surf).clip_rect surf (map_RGB surf col)
;;

let centerScreenOnObj (o : Ship.ship) =
  logscreenx := o#getArea#getx;
  logscreeny := o#getArea#gety;
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
   Hahahah...  We could take ANY image, tile it onto a surface of the
   appropriate size, and do this anyway!  W00t!  I love it when speed-
   for-memory tradeoffs make life easier.
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


(* The handshake SHOULD contain all the data the client needs, right?
   Right?
*)
class realm sh serveriaddr ic oc =
  (*  let cfg = getConfig cfgfile in *)
  let listenport = (sh.sh_dataport + 2 +
		    (int_of_char sh.sh_clientid * 2)) in
  let sendport = (sh.sh_dataport + 1 +
		  (int_of_char sh.sh_clientid * 2)) in

object (self)
  val netq = new Netq.netq serveriaddr sendport listenport SOCK_DGRAM
  val serverip = serveriaddr
  val serverport = sh.sh_dataport
  val serversockaddr = ADDR_INET( serveriaddr, sh.sh_dataport ) 
(*  val selfsockaddr = ADDR_INET( getMyIP (), selfdataport ) *)
  val chatic = ic
  val chatoc = oc
  val myid = int_of_char sh.sh_clientid

  (* XXX: Background should be from config file *)
  val mutable background = getImg "bigbackground5.png"
  val mutable objects = Hashtbl.create 64
  val mutable players = (Array.map (fun x -> new Ship.ship x) 
			   sh.sh_playerindex)
  val mutable wasthen = 0
  val mutable isnow = 0
  val mutable framecount = 1
  val mutable continue = true

  val inputmgr = new Input.inputmgr "keys.cfg"
(*
  val mutable nextWepPressed = false
  val mutable prevWepPressed = false
*)


  val mutable debug = false

  val mutable serverTime = 0

  method marshalInputData =
    0


  (* We only send data when the keystate changes, which works pretty
     damn well.  XXX: UDP uncertainty may let packets be dropped, which
     would be serious for this.
  *)
  method doServerOutput =
    (*    if isnow > nextSend then ( *)
    if inputmgr#isKeystateNew then (
      let obj = {
	pi_time = isnow;
	pi_data = inputmgr#encode;
	pi_id = (char_of_int myid);
      } in
	netq#sendObjects [obj];
    )

  (* Here, we handle data recieved from the server.  If it's a player event,
     we check if it's newer than the last event we've seen, and if so,
     we do it.
     If it's an object event, we check if we have to create a new object
     or if we already have one, then do the same.
     If it's a death event, we kill the object concerned.

     This is, on the whole, pretty straightforward.  Still hairier than
     I'd prefer though.
  *)
  method doServerInput =
    let input = netq#getObjects in
      (*      Printf.printf "Got %d object events from the server\n" 
	      (List.length input);
      *)
    let updateObj = function
	Playere( e ) -> 
	  let pid = int_of_char e.pe_id in
	    serverTime <- max serverTime e.pe_time;
	    if e.pe_time > (players.(pid)#getLastUpdate) then (
	      players.(pid)#setLastUpdate e.pe_time;
	      players.(pid)#moveTo (float_of_int e.pe_x) 
		(float_of_int e.pe_y);
	      players.(pid)#setFacing (float_of_int e.pe_facing);
	      players.(pid)#getVector#setxy (float_of_int e.pe_dx)
		(float_of_int e.pe_dy);	      
	    );
      | Objecte( e ) -> 
	  Printf.printf "Got input of object %d\n" e.oe_id;
	  serverTime <- max serverTime e.oe_time;
	  if Hashtbl.mem objects e.oe_id then (
	    let o = Hashtbl.find objects e.oe_id in
	      if e.oe_time > o#getLastUpdate then (
		o#setLastUpdate e.oe_time;
		o#moveTo (float_of_int e.oe_x) (float_of_int e.oe_y);
		o#getVector#setxy (float_of_int e.oe_dx) 
		  (float_of_int e.oe_dy)
	      )
	      else ()
	  )
	  else (
	    let o = (new gameobj (Typeindex.index2str e.oe_type)) in
	      o#moveTo (float_of_int e.oe_x) (float_of_int e.oe_y);
	      o#getVector#setxy (float_of_int e.oe_dx) 
		(float_of_int e.oe_dy);
	      o#setLastUpdate e.oe_time;
	      Hashtbl.add objects e.oe_id o;
	      print_endline "New object done";                        
	  )
      | ObjectDeath( objid ) ->
	  if objid = myid then (
	    print_endline "You have died, sucker!";
	    sleep 10;
	    exit 0;
	  )
	  else if Hashtbl.mem objects objid then
	    let o = Hashtbl.find objects objid in
	      o#die
	  else if objid < (Array.length players) then (
	    Printf.printf "Player %d go boom!" objid;
	    players.(objid)#die;
	  )

    in
      List.iter updateObj input;

  method doChat = 
    ()
      
  (* This is the client, so we just do simple prediction, then correct when
     the server sends us the real info.
  *)
  method doCalc dt =
    let t = float_of_int dt in
      (* We have "soft" boundaries; for each distance d the player
	 goes beyond them, they get d/2 acceleration in the opposite
	 direction.  No ship should be able to continually go past them.
	 Note that this DOES let you get to very high speeds by pin-balling
	 back and forth.
      *)
      Hashtbl.iter (fun _ x -> x#calculate dt;) objects;
      Array.iter (fun x -> x#calculate dt) players;
      Particles.globalParticles#calc dt;

  method doDrawing scr =
    fillColor scr black; 
    drawBackground background scr;
    Particles.globalParticles#draw scr;
    (*    Printf.printf "Drawing %d things\n" (List.length objects); *)
    Hashtbl.iter (fun _ x -> x#draw scr !logscreenx !logscreeny) objects;
    Array.iter (fun x -> if x#isAlive then x#draw scr !logscreenx !logscreeny) players;

    if debug then
      Hashtbl.iter (fun _ x -> x#drawMeta scr !logscreenx !logscreeny) 
	objects;
    self#doGui scr;

  (* We not only have to delete objects that have died, but we must cull
     out objects that the server has not mentioned for a while; say, a 
     second.
  *)
  method doDeath =
    let alivelst = Hashtbl.create 64 in
      Hashtbl.iter 
	(fun ky obj -> 
	   if obj#isAlive && 
	     ((serverTime - obj#getLastUpdate) < 1000) then
	       Hashtbl.add alivelst ky obj
	   else
	     Printf.printf "Object died: %d\n" ky) 
	objects;
      objects <- alivelst;
      (* Okay, each class can just override it's own death animation
	 method, gods dammit. *)
      (* List.iter (fun x -> self#addObjects x#makeDeathObjs) deadlst;
      *)
      

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
    let w = players.(myid)#getWeapons in
    let displayheight = (Array.length w * 15) + 10 in
      drawVLine scr 600 1 displayheight green;
      drawVLine scr 799 1 displayheight green;
      drawHLine scr 600 799 1 green;
      drawHLine scr 600 799 displayheight green;

      for x = 0 to (Array.length w) - 1 do
	Text.drawTextAbs w.(x)#getName "Arial.ttf" green 620 (x * 15) scr;
	if players.(myid)#getCurrentWeaponIndex = x then
	  let color = 
	    if players.(myid)#getCurrentWeapon#refireOK then blue
	    else red in
	    drawFilledRect scr 610 ((x * 15) + 10) 5 5 color;
      done;

      (* Draw coordinate display *)
      Text.drawTextAbs (Printf.sprintf "X: %0.0f  Y: %0.0f" players.(myid)#getX players.(myid)#getY) "Arial.ttf" green 600 400 scr;


  method doInput (dt : int) =
    inputmgr#readKeyStrokes;
    if (is_key_pressed KEY_q) then
      continue <- false;

    let dt = float_of_int dt in
    let im = inputmgr in
      if (im#isKeyPressed Input.thrustmask) then
	players.(myid)#thrust dt;

      if im#isKeyPressed Input.turnlmask then
	players.(myid)#turnLeft dt
      else if im#isKeyPressed Input.turnrmask then
	players.(myid)#turnRight dt;

      if im#isKeyPressed Input.firemask then
	();
      if im#isKeyPressed Input.specialmask then
	();
      if im#isKeyPressed Input.incwepmask then
	players.(myid)#nextWeapon
      else if im#isKeyPressed Input.decwepmask then
	players.(myid)#prevWeapon;



  (*

    ignore (Sdlevent.poll()); (* if (is_key_pressed !Input.turnright)
    then player#turnRight (float_of_int dt);

    if (is_key_pressed !Input.turnleft) then
    player#turnLeft (float_of_int dt);

    if (is_key_pressed !Input.thrust) then
    player#thrust (float_of_int dt);

    if (is_key_pressed !Input.fire) then
    self#addObjects (player#makeShot : 
    Weapon.shot list :> Gameobj.gameobj list);
  *)

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
  *)

  initializer
    Typeindex.initIndex 
      (Array.map (fun x -> fst x) sh.sh_objindex);

  method doMainloop scr =

    try
      while continue do
	framecount <- framecount + 1;
	wasthen <- isnow;
	isnow <- Sdltimer.get_ticks ();
	self#doServerInput;
	self#doChat;
	let dt = isnow - wasthen in

	  self#doInput dt;
	  self#doCalc dt;
	  self#doDeath;
	  centerScreenOnObj players.(myid);
	  self#doDrawing scr;
	  self#doGui scr;

	  self#doServerOutput;

	  if not players.(myid)#isAlive then (
	    Text.drawTextAbs "You Have Died" "Arial.ttf" red 350 300 scr;
	    sleep 5;
	    continue <- false;
	  );

	  flip scr;
	  flush Pervasives.stdout;
      done
    with
	Sys_error( "Connection refused" ) ->
	  Printf.eprintf "Lost connection with server!\n"

      (*
	val mutable objects = []
	val mutable player = new Ship.ship "testship.shp"
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




      (*  val mutable background = cfg#getStr "realm" "background" *)

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
	o#acceldm 270. ((o#getX -. dimX) *. t /. 1000. *. wallToughness)
	else if o#getX < -. dimX then (
	o#acceldm 90. ((-. dimX -. o#getX) *. t /. 1000. *. wallToughness);
	);
	if o#getY > dimY then
	o#acceldm 180. ((o#getY -. dimY) *. t /. 1000. *. wallToughness)
	else if o#getY < -. dimY then (
	o#acceldm 0. ((-. dimY -. o#getY) *. t /. 1000. *. wallToughness)
	);
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



	method doEdges dt =
	let t = float_of_int dt in
	let doReverseAccel o =
	if o#getX > dimX then
	o#acceldm 270. ((o#getX -. dimX) *. t /. 2000.)
	else if o#getX < -. dimX then (
	o#acceldm 90. ((-. dimX -. o#getX) *. t /. 2000.);
	Printf.printf "Boing! %f\n" o#getX;
	);
	if o#getY > dimY then
	o#acceldm 180. ((o#getY -. dimY) *. t /. 2000.)
	else if o#getY < -. dimY then (
	o#acceldm 0. ((-. dimY -. o#getY) *. t /. 2000.);
	Printf.printf "Spang! %f\n" o#getY;
	)
	in
	List.iter doReverseAccel objects;

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
	self#addObject (new gameobj "testobj.obj");
	
	player#addWeapon "testweapon1.wep";
	player#addWeapon "testweapon2.wep";
	player#addWeapon "testweapon.wep";

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
	let x = new Gameobj.gameobj "testobj.obj" in
	x#moveTo (Random.float 300.) (Random.float 300.);
	self#addObject x;
	);
      *)

	wasthen <- isnow;
	isnow <- Sdltimer.get_ticks ();
	
	if framecap then
	while ((Sdltimer.get_ticks ()) - wasthen) < 25 do
	()
	done;
	flush stdout;
	flip scr;
	done;
      *)
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
  val mutable flags = () (* new Flags.flags "flags." *)

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
