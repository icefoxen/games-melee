(* serverrealms.ml
   A realm is what we call a level or arena; a gameplay area of a certain
   fixed size.  We have one object that keeps track of all the things like
   collision detection and physics handling.
   This is the server-side, so we do no drawing, but instead send
   all relevant info to the clients.  
   It also takes care of meta-game stuff like input, background, music,
   effects, timing...

   Okay, we're taking this from the top, 'cause the player handling is
   fucked up when jammed on top of everything else.

   Simon Heath
  2/7/2005
*)

open Resources;;
open Util;;
open Gameobj;;
open Ship;;
open Particles;;
open Net;;

class realm cfgfile clientlst dataport =
  let cfg = getConfig cfgfile in
object (self)
  val mutable players = clientlst

  val mutable dimX = cfg#getFloat "realm" "width"
  val mutable dimY = cfg#getFloat "realm" "height"
  val mutable map = cfg#getStr "realm" "map"
  val mutable objects =  
     Array.to_list 
       (Array.map (fun x -> (x#getShipobj : ship :> gameobj)) clientlst)


  val mutable wasthen = 0
  val mutable isnow = 0
  val mutable framecount = 1
  val mutable continue = true
  val mutable debug = cfg#getBool "realm" "debug"
  val mutable framecap = cfg#getBool "realm" "framecap"
  val mutable background = getImg (cfg#getStr "realm" "background")
  val mutable wallToughness = cfg#getFloat "realm" "wallToughness"
  val mutable speedCap = 
    (* Make sure the speed cap is sane; we have a limit of 2^15
       because of packet size *)
    let s = cfg#getFloat "realm" "speedCap" in
    let s = (min s (2. ** 15.)) in
    let s = (max s 1.) in s

(* This makes sure we only send to each client every 1/5 second
   so as not to suck bandwidth, and cycle through the clients.
*)
  val mutable nextClient = 0
  val throttleInterval =
    (cfg#getInt "realm" "throttleInterval") / (Array.length clientlst)
  val mutable nextSend = 0

  method getObjects = objects

  method setObjects (o : gameobj list) = objects <- o

  method addObject o = objects <- o :: objects

  method addObjects olst = objects <- olst @ objects

  (*
    method setPlayers p = players <- p
    method getPlayers = players
    method getPlayer n = players.(n)
    method setPlayer n p = players.(n) <- p
  *)
			     

  (* Okay.  We've changed this so the player's inputmanager hangs on to
     the player's current keystate (XXX: UDP uncertainty!).  And then
     each iteration, it checks for new input, adopts it if it exists, and
     then does stuff according to the im's state.
     And it seems to work pretty damn well.  Purr.  I love it when
     things work.


     XXX: Okay.  So we need to check if a player is alive before doing this.
  *)
  method doClientInput dt =
    let dt = float_of_int dt in
    let handleClientInput (p : Players.player) =
      let data = p#getClientData
      and ship = p#getShipobj
      and im = p#getInputMgr 
      in
	(* We set the client state to the latest data we've recieved *)
	List.iter (fun x -> if p#getLastClientUpdateTime < x.pi_time then (
		     im#decode x.pi_data;
		     p#setLastClientUpdateTime x.pi_time;
		   )) data;

	(* Then we check the keystate and do whatever based on it *)
	if (im#isKeyPressed Input.thrustmask) then
	  ship#thrust dt;

	if im#isKeyPressed Input.turnlmask then
	  ship#turnLeft dt
	else if im#isKeyPressed Input.turnrmask then
	  ship#turnRight dt;

	if im#isKeyPressed Input.firemask then
	  self#addObjects (ship#makeShot : Weapon.shot list :> Gameobj.gameobj list);
	if im#isKeyPressed Input.specialmask then
	  ();

	if im#isKeyPressed Input.incwepmask then
	  ship#nextWeapon
	else if im#isKeyPressed Input.decwepmask then
	  ship#prevWeapon;
    in
      Array.iter (fun x -> handleClientInput x) players;


  method doClientChat =
    ()

  (* XXX: Okay, so we have to only send data if the player is alive to get
     it.
  *)
  method doClientOutput =
    (* We have to throttle output so as not to suck bandwidth. *)
    if isnow > nextSend then (
      (* First, we skim through skipping the dead players *)
      while not players.(nextClient)#isAlive do
	nextClient <- (nextClient + 1) mod (Array.length players);
      done;
      (* We're only going to do 1 send per game loop, no matter
	 what.  If we're having less than one game loop per 200ms, we
	 have bigger problems than some extra lag.  *)
      let p = players.(nextClient) in
	p#sendClientData isnow;
	(* XXX: How do we send the appropriate type to the client? 
	   Hmm, the most obvious way is to have an array mapping ints
	   to types.  That's a pain in the butt though, 'cause you have
	   to search back and forth all the bloody time.
	   A better way may be to have each gameobj know it's type id...
	   But then it'd have to find it out when created, which... well,
	   could be done, but might be a bit tricky.
	   Though then there's still the client-side issue of meshing object
	   types and id's from packets to gameobj's...  not sure about
	   the best way to approach that one.
	*)
	p#sendClientObjects (List.map (fun x -> x#toPacket isnow) objects);
	nextSend <- isnow + throttleInterval;
	nextClient <- (nextClient + 1) mod (Array.length players);
    )
      
      

  (*
    for x = 0 to (Array.length players - 1) do
    let p = players.(x) in
    let psockaddr = Unix.ADDR_INET( p#getIP, (dataport + 1 + x) ) in
    netq#sendObjects [p#toPlayerEvent 10] psockaddr;
    done;
  *)

  method doCalc dt =
    let t = float_of_int dt in
      (* We have "soft" boundaries; for each distance d an object
	 goes beyond them, they get d/wallToughness acceleration in the 
	 opposite direction
	 No ship should be able to continually go past them.
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

  (* A fairly dumb way of testing whether objects are touching, but 
     it works okay for <500 objects or so.  
     Note that we don't keep non-tangible objects seperate,
     which would be a pretty simple optimization. *)
  method doCollide t = 
    let rec loop (current : Gameobj.gameobj) lst =
      if lst = [] then
	()
      else (
	List.iter (fun x -> 
		     if current#isColliding x t then (
		       print_endline "Impact happened";
		       current#impact x;)
		  ) lst;
	loop (List.hd lst) (List.tl lst);
      )
    in
      loop (List.hd objects) (List.tl objects)


  (* We have to handle player and object deaths seperately, since
     players come with networking stuff attached
  *)
  method doDeath =
    (* First we handle dead players (badly)...  *)
    Array.iter (fun x -> if not x#isAlive then x#cleanUpNet) players;

      (* Then we handle dead objects *)
      let alivelst, deadlst = List.partition (fun x -> x#isAlive) objects in
	objects <- alivelst;
	(* Send death notification messages to the clients *)
	let sendDeathNotification obj pl =
	  if pl#isAlive then
	    pl#sendClientObjects [(Net.ObjectDeath( obj#getID ))]
	in
	  List.iter (fun x -> Array.iter (sendDeathNotification x) players)
	    deadlst;
	  

	  (* Then we handle death effects *)
	  (* Okay, each class can just override it's own death animation
	     method, gods dammit. 
	     But that still doesn't work 'cause death anims would have death
	     anims, forever.
	     ...hm, maybe as a death animation they return a gameobj
	     OPTION?  That'd require some kludging to handle in the config,
	     but it may work.
	  *)
	  (* List.iter (fun x -> self#addObjects x#makeDeathObjs) deadlst;
	  *)


  method doMainloop =
    print_endline "Entering mainloop";

    (* Init players to random positions *)
    let initPlayers i p = 
      let ship = p#getShipobj in
      let rx = Random.float (dimX *. 2. -. dimX)
      and ry = Random.float (dimY *. 2. -. dimY) in
	Printf.printf "Ship %d moved to %f, %f\n" i rx ry;
	ship#moveTo rx ry
    in
      Array.iteri initPlayers players;
      Printf.printf "We have %d players!\n" (Array.length players);


      (* Set up timing *)
      wasthen <- Sdltimer.get_ticks ();
      isnow <- Sdltimer.get_ticks ();

      (* Add map objects *)
      let map = (new Maploader.maploader (!Resources.datadir ^ map)) in
	(*      Printf.printf "Hello mapobjs: %d\n" (List.length map); *)
	self#addObjects map#getObjects;

	while objects <> [] do
	  framecount <- framecount + 1;
	  wasthen <- isnow;
	  isnow <- Sdltimer.get_ticks ();
	  let dt = isnow - wasthen in
	    self#doClientInput dt;
	    self#doCalc dt;
	    self#doCollide dt;
	    self#doDeath;
	    self#doClientChat;

	    self#doClientOutput;
	    flush stdout;
	done;

end;;

