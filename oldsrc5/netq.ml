(* netq.ml
   A polymorphic object that sends and recieves and buffers data.
   Oh, MAN, this is so much nicer than the structured version!  O_O
   And functional programming is great for datastructures, but sucky for
   IO processing.

   ...hmm.  Multiplexing UDP over a single connection is bloody annoying,
   it seems.

   Simon Heath
   5/7/2005
*)

open Unix;;
open Net;;


(* Kay, a few changes happenin' here. *)
class ['a, 'b] netq destip sendport listenport proto =
object (self)
  val sendsock = socket PF_INET proto 0;
  val listensock = socket PF_INET proto 0;
  val mutable ic = Pervasives.stdin;
  val mutable oc = Pervasives.stdout;
  val mutex = Mutex.create ();
  val queue = Queue.create ();
  val mutable listenerThread = Thread.create ignore ();
  val mutable isAlive = true;

  initializer
    Printf.printf "Netq made.  Listening on %d, sending on %d\n" listenport
      sendport;
    bind listensock (ADDR_INET( (getMyIP ()), listenport ));
    connect sendsock (ADDR_INET( destip, sendport ));
    ic <- in_channel_of_descr listensock;
    oc <- out_channel_of_descr sendsock;
    self#spawnListenerThread;

  method private spawnListenerThread =
    let tf () =
      while true do
	let data = (input_value ic : 'a) in
	  (*	  Printf.printf "Grabbed data from %d\n" listenport; *)
	  Mutex.lock mutex;
	  Queue.add data queue;
	  Mutex.unlock mutex;
      done
    in
      listenerThread <- Thread.create tf ();

  method kill =

    (* Dammit, this keeps crapping out for some reason when a players 
       dies... *)
    if isAlive then (
      try
	print_endline "Killing netq nicely...";
	Thread.kill listenerThread; 
	close_in ic;
	close_out oc; 
      with
	  x -> print_endline "Blop!"; raise x
	    shutdown sendsock SHUTDOWN_ALL;
	    shutdown listensock SHUTDOWN_ALL;
	    isAlive <- false;

    )

  method isAlive = isAlive

  method getObjects =
    let lst = ref [] in  
      Mutex.lock mutex;
      Queue.iter (fun x -> lst := x :: !lst) queue;
      Queue.clear queue;
      Mutex.unlock mutex;
      !lst;


  (* XXX: This may end up doing a lot of I/O, so it may be better to
     have another thread and output queue for this.  Possibly with a
     select involved.  *)
  method sendObjects (objs : 'b list) =
    (*    Printf.printf "Sending to %d\n" sendport; *)
    if isAlive then (
      List.iter (output_value oc) objs;
      flush oc;
    )
    else
      Printf.printf "Warning: netq tried to send to closed channel!\n";
end;;

