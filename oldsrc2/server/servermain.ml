(* servermain.ml
   Main game logic and setup.
   For the server side of things, at least.

   This also takes the accepting connections, server handshaking and
   recieving all the appropriate data, and setting up the realm and players.
   
   Simon Heath
   27/5/2005
*)


open Config;;
open Util;;
open Net;;
open Unix;;


let serverVersMajor = 0;;
let serverVersMinor = 0;;


(* There's gotta be a better way than DNS'ing ourselves... *)
let getMyIP () =
  let myname = gethostname () in
  let mydnsentry = gethostbyname myname in
  let myip = myentry.h_addr_list.(0) in
    Printf.printf "IP grabbed: %s\n" (string_of_inet_addr myip);
    myip
;;


let sendHandshakeToClient oc servercfg realmcfg clientid =
  let realmcfg = getConfig realm in
  let handshake = {
    (* Um.  Slight architecture issue here.  I don't want to pass the config
       file through two useless wossnames just to get here.  I think that
       this should maybe go outside of listenForClients.
       Hmmm.  As should getHandshakeFromClient, then.
    *)
    sh_name = servercfg#getStr "server" "name";
    (* Um, I think we can use the handshake connection for chat... *)
    sh_chatport = servercfg#getStr "server" "port";
    sh_dataport = servercfg#getStr "server" "dataport";
    sh_clientid = clientid;
    sh_version_major = serverVersMajor;
    sh_version_minor = serverVersMinor;
    sh_packagename = realmcfg#getStr "realm" "name"; 
    sh_realmdata = realmcfg#to_string;

    (* The objects we get from the realm/map, the players we get from
       the client (though we don't know how yet), and the shots we get
       from the players (or just an index list in the realm/map
    *)
    sh_objindex = [||];
    sh_playerindex = [||];
    sh_shotindex = [||];
  }
  in
    output_value oc handshake;
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
let listenForClients iaddr portnum numclients =
  let portnum = portnum
  and numclients = numclients in
  let clientlist = ref [] in
  let socket = socket PF_INET SOCK_STREAM 0 in
    bind sock (ADDR_INET( iaddr, portnum ));
    listen sock 16;
    for x = 0 to (numclients - 1) do
      let (fd, sockaddr) = accept sock in
      let ic = in_channel_of_descr fd
      and oc = out_channel_of_descr oc in
	clientlist := (sockaddr, ic, oc) :: !clientlist
    done;
    !clientlist;
;;



let mainloop scr bkg =
  let r = new Realms.realm "testrealm.cfg" in
    r#doMainloop scr;
;;

let usage () =
  print_endline "Usage: vrserver [-p num] [-c cfgfile] [-n numclients]";
  Sdl.quit ();
  exit 1;
;;


let main () =

  (* Init  *)
  Sdl.init [`EVERYTHING];
  (*  Sdlwm.set_caption ~title: "Voidrunner Server" ~icon: "";
      Sdlmouse.show_cursor false;

      Sdlttf.init ();
      Audio.initSound 16; 
      Random.self_init ();

      Input.loadKeyDefs ();

  (* Grafix setup... of which there is none *)
      let screen = set_video_mode ~w: !screenx ~h: !screeny ~bpp: 16 
      [`DOUBLEBUF; `SWSURFACE] in

      let bkg = create_RGB_surface_format screen [`HWSURFACE] 
      ~w: !screenx ~h: !screeny in
      fill_rect bkg (map_RGB bkg black);
  *)

  (* Arg parsing!  Yay! *)
  let cfgfile = ref "server.cfg"
  and portnum = ref -1
  and realm = ref ""
  and numclients = ref -1 in
    for x = 1 to (Array.length Sys.argv) - 1 do
      if Sys.argv.(x) = "-c" then
	cfgfile := Sys.argv.(x + 1);

      if Sys.argv.(x) = "-p" then
	portnum := int_of_string Sys.argv.(x + 1);

      if Sys.argv.(x) = "-n" then
	numclients := int_of_string Sys.argv.(x + 1);

      if Sys.argv.(x) = "-r" then
	realm := Sys.argv.(x + 1);

      if Sys.argv.(x) = "-h" then
	usage ();
    done;
    (* If we override the default config values for something, we set it here.
       Only the main function should read the server config file, ideally. *)
    let cfg = getConfig !cfgfile in
      if !portnum = -1 then
	portnum := cfg#getInt "server" "port";
      if !numclients = -1 then
	!numclients := cfg#getInt "server" "numclients";
      if !realm = "" then
	!realm := cfg#getStr "server" "realm";

      (* First, we grab the server config, then we listen for client connections,
	 then we okay them for ship configurations (XXX: make ship 
	 configurations), then we build the realms and life is keen.
      *)

      let ipaddr = getMyIP () in
	
      (* listenForClients does all the initial connections and
	 handshaking, opening two tcp channels which are then, just
	 for fun, used for the chat interface.  The mainloop opens the
	 UDP channels t hat will transmit all the gameobjs and
	 playerobjs.  *)
      let realmconfig = cfg#getStr "server" in
      let clientlist = ref [] in
      let socket = socket PF_INET SOCK_STREAM 0 in
	bind sock (ADDR_INET( ipaddr, !portnum ));
	listen sock 16;
	for x = 0 to (numclients - 1) do
	  let (fd, sockaddr) = accept sock in
	  let ic = in_channel_of_descr fd
	  and oc = out_channel_of_descr oc in
	    clientlist := (sockaddr, ic, oc) :: !clientlist;
	    getHandshakeFromClient ic;
	    sendHandshakeToClient oc realmconfig x;
	done;


	(*	let clientlist = listenForClients ipaddr !portnum !numclients !realm in
	*)  (*      mainloop cfg clientlist; *)


	  (* De-init *)

	  (* Sdlttf.quit (); *)
	  Sdl.quit ()    
;;


let _ =
  main ();;

