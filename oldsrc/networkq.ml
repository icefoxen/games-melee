(* networkq.ml
   This is an object that abstracts out the network interface for us.
   Basically, it spawns a thread that listens for connections and, when
   it gets one, puts the data it recieves into a queue and makes it avaliable
   to the object's owner.  When ordered to send data, it sends data.
   It works over UDP, and each networkq object sends/recieves a single type
   of data.
*)
