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
class player shp id name ichan ochan sock sockaddr =
object (self)
  val mutable ship = shp
  val mutable shipobj = new Ship.ship shp
  val mutable id : int = id
  val mutable name : string = name
  val mutable ic = ichan
  val mutable oc = ochan
  val mutable socket : file_descr = sock
  val mutable iaddr = sockaddr2ip sockaddr
  val mutable port = sockaddr2port sockaddr

  method getShip = ship
  method getShipobj = shipobj
  method getID = id
  method getName = name
  method getInchan = ic
  method getOutchan = oc
  method getSocket = socket
  method getIP = iaddr
  method getPort = port
  (* This returns the socket address it's SENDING TO, not listening on! *)
  method getSockAddr =
    ADDR_INET( iaddr, port )


  (* Sends and recieves strings over the TCP connection 
     XXX: ...Wait, recieve will block if there's no info!  Smeg!  *)
  method sendChatMessage (msg : string) =
    output_value oc msg;

  method getChatMessage =
    (input_value ic : string);

  method toPlayerEvent time =
    shipobj#toPacket time



end;;
