(* netq.ml
   A polymorphic object that sends and recieves and buffers data over UDP.
   Oh, MAN, this is so much nicer than the structured version!  O_O
   And functional programming is great for datastructures, but sucky for
   IO processing.

   Simon Heath
   5/7/2005
*)

open Unix;;
open Net;;


class ['a, 'b] netq port =
object (self)
  val sock = socket PF_INET SOCK_DGRAM 0;
  val mutable ic = Pervasives.stdin;
  val mutable oc = Pervasives.stdout;
  val mutex = Mutex.create ();
  val queue = Queue.create ();
  (* We never actually use the thread handle, and it doesn't seem that
     we can ask it if it's finished executing (short of a join, which'd
     be silly), but it's nice to have *)
  val mutable listenerThread = Thread.create ignore ();
  initializer
    bind sock (ADDR_INET( (getMyIP ()), port ));
    ic <- in_channel_of_descr sock;
    oc <- out_channel_of_descr sock;
    self#spawnListenerThread;

  method private spawnListenerThread =
    let tf () =
      while true do
	let data = (input_value ic : 'a) in
	  Printf.printf "Grabbed data from %d\n" port;
	  Mutex.lock mutex;
	  Queue.add data queue;
	  Mutex.unlock mutex;
      done
    in
      listenerThread <- Thread.create tf ();

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
  method sendObjects (objs : 'b list) sockaddr =
    match sockaddr with
	ADDR_INET( _, port ) -> Printf.printf "Sending to %d\n" port;
	  connect sock sockaddr;
	  List.iter (output_value oc) objs;
	  flush oc;
      | _ -> ()
	  
end;;

