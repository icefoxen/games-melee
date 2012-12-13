(* net.ml
   General networking information.
   Functions to send and recieve data of various types, as well as
   building said data.  Also includes the general UDP framework, and some
   marshalling and unmarshalling functions for objects.

   Note for the packet size: UDP headers add 44 bytes to the final size
   of each packet, but more than one structure can be packed into a packet,
   up to 1500 bytes.

   Simon Heath
   23/6/2005
*)

open Unix;;




(* This is the packet that the server sends to the client to tell it what's
   happening in the universe.
   Note that currently, we really don't care what animation step an
   object is in.
   I'm tempted to make it so the x and y, and more likely dx and dy, are
   two halves of a single int, or possibly a collection of chars abused in
   more or less the same fashion.  The dx and dy are the most likely target.
   However, doing this would limit the size of the arena and/or the maximum
   speed of +-16384 (2^15 / 2, with 1 bit left over).

   Size: 28 bytes
   Period: Often, 1/5 second
   Direction: Server -> Player
   Protocol: UDP
*)
type objectEvent = {
  oe_id : int;   (* Could maybe be a short *)
  oe_type : int; (* Could probably be a short *)
  oe_time : int; (* Could very maybe be a short *)
  oe_x : int;    (* Could maybe be a short *)
  oe_y : int;    (* Could maybe be a short *)
  oe_dx : int;   (* Could probably be a short *)
  oe_dy : int;   (* Could probably be a short *)
}


(* This is the packet that the server sends to the client to tell the 
   client what the state of each player is.
   This is subject to change, as I don't know how half of this stuff
   will be implemented.

   Size: 46 bytes
   Period: Often, 1/5 second
   Direction: Server -> Player
   Protocol: UDP
*)
type playerEvent = {
  pe_id : char;  
  pe_time : int;   (* Could very maybe be a short *)
  pe_facing : int; (* Could maybe be a char *)
  pe_x : int;      (* Could maybe be a short *)
  pe_y : int;      (* Could maybe be a short *)
  pe_dx : int;     (* Could probably be a short *)
  pe_dy : int;     (* Could probably be a short *)
  pe_wep : char;  
  pe_energy : int; (* Could probably be a short *)
  pe_shield : int; (* Could probably be a short *)
  pe_armor : int;  (* Could probably be a short *)
  pe_hits : int;   (* Could probably be a short *)
  pe_wepreload : int;  (* Have no clue how this'll be implemented; bitmap? *)
  pe_regenproportions : int; (* Or this either *)
}

(* This would be a kludge if it weren't so elegant.
   We need to be able to send different events over the same channel,
   and the client has to be able to tell them apart to unmarshal them.
   This does both.  Why the hell don't more languages have tagged unions?

   We even extend this to include object death messages.  It just has 
   the id of the object that died.
*)
type eventwrapper = 
    Playere of playerEvent
  | Objecte of objectEvent
  | ObjectDeath of int
;;

(* This probably goes over TCP, remember.  There can be any manner of
   IRC-ish server commands and such stuck into the text.
   These are yet to be specified.

   Size: Variable, 8+ bytes.
   Period: Intermittant, possibly never
   Direction: Player <-> Server
   Protocol: TCP
*)
type chatPacket = {
  chat_pid : int;
  chat_text : string;
}

(* This is the packet that the client sends to the server to tell it what
   the client's doing.
   Keystates needed: see input.ml for keystate->int encoding.
   That makes: 8

   Size: 9 bytes.
   Period: Often, 1/5 second
   Direction: Player -> Server
   Protocol: TCP
*)
type playerInput = {
  pi_id : char;
  pi_data : int;
  pi_time : int;
}

type inputwrapper =
    Chati of chatPacket
      (* XXX: This isn't used currently, but should be soon. *)
  | Playeri of playerInput
;;


(* We can't marshal objects.  Well fuck.  We have to kludge around it.
   Should these be defined here, or with the classes?  Here works better,
   (we can send lists of 'em in the server handshake) but with the classes 
   makes more sense.

   Nah, we can put them there.  Net will come after objects... but before
   realms.  Hmm.

   ...hm.  Are these even necessary?  Can we just send a list of the
   config files?  Or the config files themselves, for that matter.  Wah...  
   The latter will be more inefficient, but to coin a phrase, so what?
   It's a matter of maybe a few kilobytes, happening once, and the
   simplicity of it...
   So it basically comes down to an issue of people fucking up their config
   files, in game, and that's just silly since they're only read once anyway.
   Argle.  We'll just send the config file contents, and we'll be fine.
*)
(*
type gameobjSerialized = {
}

(* THIS may cause a bit of issue, as players can be customized... 
   We really don't know how though, yet.  So leave it.
*)
type playerSerialized = {
}

type shotSerialized = {
}

type realmSerialized = {
}
*)

(* This is sent to the server when the client connects to it.
   It sends all the ship data and so on the client has.
   It MAY be easier to construct ship data on the server, with the client
   just being an interface again, instead of trying to verify the data the
   client sends.  This is more secure, but also means another exchange
   between the client and server...

   Size: Variable, 16+ bytes, possibly a bit big
   Period: Once
   Direction: Player -> Server
   Protocol: TCP
*)
type clientHandshake = {
  ch_name : string;
  ch_ip : string;
  ch_version_major : int;
  ch_version_minor : int;
}


(* This is sent to each client when it connects to the server.
   It has the server name, the level data, and tells it what ports the
   client should listen on/send to for chat and data.

   It also tells the client basically everything there is to know abou
   the game universe.
   
   It also has to send all the object, realm, and player data, but that's 
   another story...  Especially since it's really an issue of cacheing
   and synchronization of large, arbitrary hunks of data!  Yeek.
   Well, not so much, at least for the Important stuff.  It'd be pretty
   simple (in fact, necessary, considering the structure of objectEvents)
   for the server to just send the client all the numerical data on each
   object type.  

   Okay, here's a proposal.  It sends a file list, with MD5 sums.
   The player sends back a list of files it doesn't have and/or don't have
   matching sums.  There's a directory for each server.


   We can use text here if we need to.

   Size: Variable, 41+ bytes, possibly quite large (~100kb)
   Period: Once
   Direction: Server -> Player
   Protocol: TCP
 *)
type serverHandshake = {
  sh_name : string;
  sh_chatport : int;
  sh_dataport : int;
  sh_clientid : char;
  sh_version_major : int;
  sh_version_minor : int;

  sh_packagename : string;

  (* We send the contents of each config file to each client, since it
     makes life really easy.
     Or perhaps even the config file object's marshalled hashtable, if we
     want to get silly and channels/files are a pain in the ass.  *)
  sh_realmdata : string;
  sh_objindex : (string * string) array;
  (* XXX: Work out player configuration...  Screw it for now. 
     At the moment, players are configured merely by config files.
     No UI.
  *)
  sh_playerindex : string array;
  sh_shotindex : string array;
}


(* This is the type 
type clientinfo = {
}
*)

(* A few demented bit-pushing functions, 'cause sometimes we want 16-bit
   values.  We use two chars for this, 'cause I really don't see any more
   convenient way.
*)
let char22int (ca, cb) =
  (((int_of_char ca) lsl 8) lor (int_of_char cb))
;;

let int2char2 i =
  let msb = char_of_int (0xFF land i)
  and lsb = char_of_int (0xFF00 land (i lsr 8)) in
    (msb, lsb)
;;

let getMyIP () =
  let myname = gethostname () in
  let mydnsentry = gethostbyname myname in
  let myip = mydnsentry.h_addr_list.(0) in
    Printf.printf "My IP grabbed: %s\n" (string_of_inet_addr myip);
    myip
;;

let getIP h =
  let mydnsentry = gethostbyname h in
  let myip = mydnsentry.h_addr_list.(0) in
    Printf.printf "Other IP grabbed: %s\n" (string_of_inet_addr myip);
    myip
;;


let printServerHandshake sh =
  Printf.printf "Server name: %s\n" sh.sh_name;
  Printf.printf "Server chatport: %d\n" sh.sh_chatport;
  Printf.printf "Server dataport: %d\n" sh.sh_dataport;
  Printf.printf "Server clientid: %d\n" (int_of_char sh.sh_clientid);
  Printf.printf "Server version: %d.%d\n" sh.sh_version_major sh.sh_version_minor;
  Printf.printf "Server packagename: %s\n" sh.sh_packagename;
  Printf.printf "Server realmdata:\n%s\n" sh.sh_realmdata;
  Printf.printf "Server object data:\n%s\n"
    (Array.fold_left (fun x y -> 
			let f, d = y in x ^ f ^ "\n" ^ d ^ "\n") "" sh.sh_objindex);
;;

					 
let printClientHandshake ch = 
  Printf.printf "Client name: %s\n" ch.ch_name;
  Printf.printf "Client ip: %s\n" ch.ch_ip;
  Printf.printf "Client version: %d.%d\n" ch.ch_version_major ch.ch_version_minor;
;;


(*
(* Translation functions turning objects into their marshalled type. *)
let gameobj2packet (g : Gameobj.gameobj) id typ time =
  let p = {
    oe_id = id;
    oe_type = typ;
    oe_time = time;
    oe_x = (int_of_float g#getX);
    oe_y = (int_of_float g#getY);
    oe_dx = (int_of_float g#getVector#getx);
    oe_dy = (int_of_float g#getVector#gety);
  } in
    Objecte( p )
;;

(* XXX: We should select the gameobj config file depending on the oe_type *)
let packet2gameobj p =
  match p with 
      Objecte( f ) -> 
	let g = new Gameobj.gameobj "testobj.obj" in
	  g#moveTo (float_of_int f.oe_x) (float_of_int f.oe_y);
	  g#getVector#setxy (float_of_int f.oe_dx) (float_of_int f.oe_dx);
    | Playere( _ ) -> raise (Failure "packet2gameobj")
	
;;

let ship2packet (s : Ship.ship)
  *)

let player2packet g =
(*
    pe_id
  pe_time
  pe_x
  pe_y
  pe_dx
  pe_dy
  pe_wep
  pe_energy
  pe_shield
  pe_armor
  pe_hits
  pe_wepreload
  pe_regenproportions
  *)
  ()
;;

let packet2player p =
  ()
;;


