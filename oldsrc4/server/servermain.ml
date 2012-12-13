(* servermain.ml
   Main game logic and setup.
   For the server side of things, at least.

   This also takes the accepting connections, server handshaking and
   recieving all the appropriate data, and setting up the realm and players.

   XXX: Do version checking.
   
   Simon Heath
   27/5/2005
*)


open Resources;;
open Config;;
open Util;;
open Net;;
open Unix;;


let serverVersMajor = 0;;
let serverVersMinor = 0;;


(* Okay, now we need to actually send useful stuff in the handshake... 
   To do that, we need to grab all the objects, players, and shots.
   We get the objects from the map, run through them and merge all the
   ones with the same config, then index them up.  We do the same thing
   for the shots, taking them from the players probably.  The players...
   I have no freakin' clue, as I don't know how player config is gonna
   work.
*)
let sendHandshakeToClient oc servercfg realmcfg clientid objidx =
  let realmcfg = getConfig realmcfg in
  let handshake = {
    (* Um.  Slight architecture issue here.  I don't want to pass the config
       file through two useless wossnames just to get here.  I think that
       this should maybe go outside of listenForClients.
       Hmmm.  As should getHandshakeFromClient, then.
    *)
    sh_name = servercfg#getStr "server" "name";
    (* Um, I think we can use the handshake connection for chat... *)
    sh_chatport = servercfg#getInt "server" "port";
    sh_dataport = servercfg#getInt "server" "dataport";
    sh_clientid = (char_of_int clientid);
    sh_version_major = serverVersMajor;
    sh_version_minor = serverVersMinor;
    sh_packagename = realmcfg#getStr "realm" "name"; 
    sh_realmdata = realmcfg#get_string;

    (* The objects we get from the realm/map, the players we get from
       the client (though we don't know how yet), and the shots we get
       from the players (or just an index list in the realm/map

       Technically, we shouldn't have to load the map twice --once here and
       once in the realm.  If we load it three times, it's gonna get it's
       own entry in the resource system.
    *)
    sh_objindex = objidx;
    sh_playerindex = [||];
    sh_shotindex = [||];
  }
  in
    output_value oc handshake;
    flush oc;
    handshake
;;


let getHandshakeFromClient ic =
  (* Here, we just directly unmarshal whatever we recieve.  I'm not too
     sure how to verify that it's not something nasty.
     XXX: Figure out how.  It may involve just reading in the string and
     doing some magic to verify that it is what we want, THEN unmarshalling
     it.
  *)
  (input_value ic: clientHandshake)
;;



(* This sets up the network and waits until it has the given number of
   client connections/data on hand.
   They are kept in the form of a tuple of
   (client sockaddr, input channel, output channel)
   and passed on to the realm
   The channels are TCP.  The fun UDP stuff comes later.

   Note that this could be a problem since it waits for each client to
   connect before sending any data or anything, so we spend a lot of
   time just blocking; if the clients take a long time connecting, we
   could also have timeout issues.
   I don't think we can easily thread this though, since no matter what
   the function won't return until all clients are accepted, and the point
   is that we move ththe handshaking out of it.  Maybe this function
   shouldn't exist and should just be part of the main function?
*)
(*
let listenForClients iaddr portnum numclients =
  let portnum = portnum
  and numclients = numclients in
  let clientlist = ref [] in
  let socket = socket PF_INET SOCK_STREAM 0 in
    try
      bind socket (ADDR_INET( iaddr, portnum ));
      for x = 0 to (numclients - 1) do
	listen socket 16;
	let (fd, sockaddr) = accept socket in
	let ic = in_channel_of_descr fd
	and oc = out_channel_of_descr fd in
	  clientlist := (sockaddr, ic, oc) :: !clientlist
      done;
      !clientlist;
    with
	Unix_error( e, _, _ ) -> 
	  Util.error "Fatal error: %s\n" (error_message e)
      | Failure( "input_value: bad object" ) ->
	  Util.error "Fatal error: Recieved wrong data during handshake%s\n" ""
;;
*)



let mainloop cfg clientlst dataport =
  let r = new Serverrealms.realm "testrealm.rlm" clientlst dataport in
    r#doMainloop;
;;



let usage () =
  print_endline "Usage: vrserver [-p num] [-c cfgfile] [-n numclients] [-r realm] [-d datadir]";
  Sdl.quit ();
  exit 1;
;;


let main () =

  (* Init  *)
  Sdl.init [`EVERYTHING];
  (* XXX:  Hrrrm.  We need to do this because the server uses gameobj's which
     use sprites which load images which don't work if the videomode isn't
     set.  Do I really want to chop gameobjs into seperate parts for client
     and server?  Not really.  But...
  *)
  ignore (Sdlvideo.set_video_mode ~w: 320 ~h: 240 ~bpp: 16 
	    [`DOUBLEBUF; `SWSURFACE]);

  (* Arg parsing!  Yay! *)
  let cfgfile = ref "server.cfg"
  and portnum = ref (-1)
  and realm = ref ""
  and numclients = ref (-1) in
    for x = 1 to (Array.length Sys.argv) - 1 do
      if Sys.argv.(x) = "-c" then
	cfgfile := Sys.argv.(x + 1);

      if Sys.argv.(x) = "-p" then
	portnum := int_of_string Sys.argv.(x + 1);

      if Sys.argv.(x) = "-n" then
	numclients := int_of_string Sys.argv.(x + 1);

      if Sys.argv.(x) = "-r" then
	realm := Sys.argv.(x + 1);

      if Sys.argv.(x) = "-d" then
	datadir := Sys.argv.(x + 1);

      if Sys.argv.(x) = "-h" then
	usage ();


    done;


    (* If we override the default config values for something, we set it here.
       Only the main function should read the server config file, ideally. *)
    let cfg = getConfig !cfgfile in
      if !portnum = -1 then
	portnum := cfg#getInt "server" "port";

      if !numclients = -1 then
	numclients := cfg#getInt "server" "numclients";

      if !realm = "" then
	realm := cfg#getStr "server" "realm";

      if !datadir = "./" then
	datadir := cfg#getStr "server" "datadir";

      (* First, we grab the server config, then we listen for client connections,
	 then we okay them for ship configurations (XXX: make ship 
	 configurations), then we build the realms and life is keen.
      *)

      let ipaddr = getMyIP () in
	
      (* listenForClients does all the initial connections and
	 handshaking, opening two tcp channels which are then, just
	 for fun, used for the chat interface.  The mainloop opens the
	 UDP channels that will transmit all the gameobjs and
	 playerobjs.  
	 XXX: Obj.magic!	 
      *)
      let clientlist = Array.create !numclients (Obj.magic ()) in
      let socket = socket PF_INET SOCK_STREAM 0 in
	
	(* XXX: This shouldn't be here 
	   But we need the object index to send to the client!
	   Maybe the request-reply is a better idea architecturally?
	   But it puts more burden on the client, and to be truely Right
	   it'd have to be purged/swept occasionally...
	   KISS!!!!!

	   Okay, so now we need to grab the shots as well...
	   How the hell do we do that?  Through the players?
	*)
      let map = (getConfig !realm)#getStr "realm" "map" in
      let mapobjs = (new Maploader.maploader 
		     (!Resources.datadir ^ map))#getObjectArray in
	print_endline "Bop!";
	
	bind socket (ADDR_INET( ipaddr, !portnum ));
	listen socket 16;
	Printf.printf "Listening on port %d\n" !portnum;

	for i = 0 to (!numclients - 1) do
	  Printf.printf "Client #%d connecting... \n" i;
	  flush Pervasives.stdout;
	  let (fd, sockaddr) = accept socket in
	  let ic = in_channel_of_descr fd
	  and oc = out_channel_of_descr fd in
	  let hs = getHandshakeFromClient ic in
	    print_endline "Sending handshake...";
	    printClientHandshake hs;
	    let player = new Players.player "testship.shp" i hs.ch_name 
			    fd sockaddr (cfg#getInt "server" "dataport") in
	      clientlist.(i) <- player;
	      flush oc;
	done;
	Array.iteri (fun i x -> 
		       ignore (sendHandshakeToClient
				 x#getOutchan cfg !realm i mapobjs)) clientlist; 

	print_endline "Clients all connected!  Yay!";

	mainloop !realm clientlist (cfg#getInt "server" "dataport");

	(* De-init *)
	shutdown socket SHUTDOWN_ALL;
	Sdl.quit ()    
;;


let _ =
  main ();;

