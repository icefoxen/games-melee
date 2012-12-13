(* clientmain.ml
   Main client logic and setup.

   Simon Heath
   22/6/2005
*)


open Config;;
open Sdlvideo;;
open Sdlkey;;
open Util;;
open Net;;

open Unix;;

let clientVersMajor = 0;;
let clientVersMinor = 0;;

let sendHandshakeToServer oc clientname clientip =
  let ch = {
    ch_name = clientname;
    ch_ip = clientip;
    ch_version_major = clientVersMajor;
    ch_version_minor = clientVersMinor;
  } in
    output_value oc ch;
    flush oc;
    ch;
;;


let getHandshakeFromServer ic =
  (input_value ic: serverHandshake)
;;


let mainloop scr handshake serveraddr =
  try
    let r = new Clientrealms.realm handshake serveraddr in
      r#doMainloop scr;
  with
      Unix.Unix_error( errcode, str1, str2 ) ->
   Printf.printf "Bop error: %s, %s, %s\n" (Unix.error_message errcode) str1 str2;
 exit 1;

;;


let usage () =
  print_endline "Usage: vrclient [-s server] [-b ship] [-p serverport] [-n playername] [-d datadir] [-c cfgfile]";
  Sdlttf.quit ();
  Sdl.quit ();
  exit 1;
;;


let main () =

  (* Init... messing up the order of things, esp. config-files, could be bad
  *)
  Sdl.init [`EVERYTHING];
  Sdlwm.set_caption ~title: "Voidrunner" ~icon: "";
  Sdlmouse.show_cursor false;

  Sdlttf.init ();

  Random.self_init ();
  Audio.initSound 16; 






  (* So... we need to parse commmand line args and config stuff, then
     connect to the server, send the handshake, recieve the reply,
     and get all the nice juicy data out of it.
     Primarily: Ship, realm, objects, players, and weapons.
  *)

  let server = ref "" 
  and serverport = ref (-1)
  and playername = ref ""
  and playership = ref ""  
  and cfgfile = ref "client.cfg" in
    for x = 1 to (Array.length Sys.argv) - 1 do
      if Sys.argv.(x) = "-c" then
	cfgfile := Sys.argv.(x + 1);

      if Sys.argv.(x) = "-s" then
	server := Sys.argv.(x + 1);

      if Sys.argv.(x) = "-p" then
	serverport := int_of_string Sys.argv.(x + 1);

      if Sys.argv.(x) = "-n" then
	playername := Sys.argv.(x + 1);

      if Sys.argv.(x) = "-b" then
	playership := Sys.argv.(x + 1);

      if Sys.argv.(x) = "-d" then
	Resources.datadir := Sys.argv.(x + 1);

      if Sys.argv.(x) = "-h" then
	usage ();
    done;

    let cfg = Resources.getConfig !cfgfile in
      if !serverport = -1 then
	serverport := cfg#getInt "client" "port";

      if !server = "" then
	server := cfg#getStr "client" "server";

      if !playername = "" then
	playername := cfg#getStr "client" "playername";

      if !playership = "" then
	playership := cfg#getStr "client" "playership";

      if !Resources.datadir = "./" then
	Resources.datadir := cfg#getStr "client" "datadir";
      
      (* We can't initialize some things until we know what config file
	 and data dir we're using *)
      Input.loadKeyDefs ();

      let serveraddr = inet_addr_of_string !server in
      let socket = socket PF_INET SOCK_STREAM 0 in
	Unix.connect socket (ADDR_INET( serveraddr, !serverport ));
	let ic = in_channel_of_descr socket
	and oc = out_channel_of_descr socket in
	  print_endline "Server connected to!  Handshaking...\n";
	  flush Pervasives.stdout;

	  ignore (sendHandshakeToServer oc !playername (string_of_inet_addr (getMyIP ())));
	  let serverhs = getHandshakeFromServer ic in
	    flush oc;
	    print_endline "Server is handshook!";
	    printServerHandshake serverhs;
	    

	    (* Grafix setup *)
	    let screen = set_video_mode ~w: !screenx ~h: !screeny ~bpp: 16 
			   [`DOUBLEBUF; `SWSURFACE] in


	      (* Write out config files --this is silly, but it works *)
	    let dumpConfig c =
	      let nm, contents = c in
	      let f = open_out (!Resources.datadir ^ nm) in
		output_string f contents;
		close_out f;
	    in
	      Array.iter dumpConfig serverhs.sh_objindex;

	      (* Mainloop *)
		mainloop screen serverhs serveraddr; 

	      (* De-init *)
	      shutdown socket SHUTDOWN_ALL;
	      Sdlttf.quit ();
	      Sdl.quit ()    

  ;;


let _ =
  main ();;

