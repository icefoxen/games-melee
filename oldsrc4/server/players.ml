(* players.ml
   This is an object to hold all the annoying little administrative
   details of players that the server needs to know about, like
   name, index number, network connections, and the ship object itself.


   5/7/2005
*)

open Unix;;
open Net;;


let sockaddr2ip = function
    ADDR_UNIX( _ ) -> raise (Failure "sockaddr2ip: given unix socket")
  | ADDR_INET( s, _ ) -> s
;;

let sockaddr2port = function
    ADDR_UNIX( _ ) -> raise (Failure "sockaddr2port: given unix socket")
  | ADDR_INET( _, p ) -> p
;;

(* Okay, this contains all the info necessary for a player.
   Ship information, network information, network connections, index
   information
*)
class player shp id name tcpsock sockadd dataport =
  let mylistenport = dataport + (id * 2) + 1
  and mysendport = dataport + (id * 2) + 2 in
object (self)
  val mutable ship = shp
  val mutable shipobj = new Ship.ship shp
  val mutable id = id
  val mutable name : string = name
  val mutable ic = in_channel_of_descr tcpsock
  val mutable oc = out_channel_of_descr tcpsock

  val mutable socket = tcpsock
  val mutable iaddr = sockaddr2ip sockadd
  val mutable iport = sockaddr2port sockadd
  val mutable sockaddr = sockadd
			   
  (* So now we worry about port conflicts...  Which are solvable, really. *)
  val mutable netq = new Netq.netq 
		       (sockaddr2ip sockadd) mysendport mylistenport
		       SOCK_DGRAM
  val inputmgr = new Input.inputmgr "keys.cfg"

  method getShip = ship
  method getShipobj = shipobj
  method getID = id
  method getName = name
  method getInchan = ic
  method getOutchan = oc
  method getSocket = socket
  method getIP = iaddr
  method getPort = iport
  (* This returns the socket address it's SENDING TO, not listening on! *)
  method getSockAddr =
    sockaddr
  method getInputMgr = inputmgr

  method getDataport =
    dataport

  method sendClientData t =
    netq#sendObjects [(shipobj#toPacket t)];
    Printf.printf "Client %d: data sent\n" id;

  method sendClientObjects objlst =
    netq#sendObjects objlst;
    Printf.printf "Client %d: %d objects sent\n" id (List.length objlst);

  method getClientData =
    let input = netq#getObjects in
      List.iter 
	(fun x -> Printf.printf "Got client input! C: %d time: %d data: %d\n"
	   (int_of_char x.pi_id) x.pi_time x.pi_data)
	input;
      input;
    

  (* Sends and recieves strings over the TCP connection 
     XXX: ...Wait, recieve will block if there's no info!  Smeg!  
     Waha, that's why netq's now support TCP.
  *)
  method sendChatMessage (msg : string) =
    ()
(*
    let cp = { chat_pid = id; chat_text = msg; } in
    netq#sendObjects [Chati( cp )] sockaddr
*)

  method getChatMessage =
    ""

  method toPlayerEvent time =
    shipobj#toPacket time

  method recievePlayerInput =
    ()


end;;
