(* typeindex.ml
   This is a bit of a kludge.
   It intends to offer a way to easily and consistantly give each config
   file a type number, and associate gameobjects with that number.
   That way the server can send different objects and the client will
   know what they are.

   The way we do that is thus:
   Each gameobj knows it's index number.  It does this by asking the
   typeindex system when it's started.  The typeindex checks, probably with
   a hashtable, and gives it the result.

   This whole thing is a bit of an efficiency hack; if bandwidth were
   infinate we could just send the object's string to the client instead
   of a one-number code.
   However, there are alternatives, if I want to do the work of coding
   them:
   1) The server doesn't send the object's type with each packet.  When
   the client doesn't know an object's type, it asks the server, which
   then tells it, and the client then remembers.  This could be done using
   the TCP chat line as a carrier.  The client then remembers the types
   of ALL the objects it's ever seen.
   Downsides: We never get rid of an object's type, though gc-ish
   weeding could be done if you really wanted.  I'll consider it for
   an efficiency thing if I ever really want to get rid of 4 bytes per
   server packet.
   2) Just use a plain ol' hash function, and use the above to handle
   collisions.  It amounts to almost the same thing, except the above is
   better.  This uses less memory but more bandwidth and a bit more CPU.

   Simon Heath
   13/7/2005
*)


let typeindex = ref [||];;


let initIndex idx =
  Printf.printf "Initing typeindex to %d items\n" (Array.length idx);
  typeindex := idx;
  Array.iter (fun x -> Printf.printf "Item: %s\n" x) !typeindex;

;;

(* Inelegant, but at least it only happens once. 
   And well, it can't happen here anyway, since the client doesn't get
   to touch the maploader.
   There's something to be said for elegance...
*)
(*
let initIndexFromMap mp =
  (new Maploader.maploader (!Resources.datadir ^ mp))#getObjectArray
;;
*)

let str2index (s : string) =
  let rec loop i =
    try
      if !typeindex.(i) = s then
	i
      else if i > ((Array.length !typeindex ) - 1) then
	raise Not_found
      else
	loop (i + 1)
    with
	Invalid_argument( _ ) ->
	  Printf.printf "Typeindex.str2index: You gave me %d when I wanted <%d!\n"
	  i (Array.length !typeindex);
	  exit 1;
  in
    loop 0
;;

let index2str i =
  try
    !typeindex.(i)
  with
      Invalid_argument( _ ) ->
	Printf.printf "Typeindex.index2str: You gave me %d when I wanted <%d!\n"
	i (Array.length !typeindex);
	exit 1;
;;
