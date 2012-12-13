(* map.ml
   This is a map loader class.
   It is basically a config class that's been hacked up a bit to
   read a file with a bunch of categories, each representing one object,
   and return a list of game objects constructed from this specification.

   Relevant config fields are:
   x, y, vectord, vectorm, objectfile, and fixed

   Note that these are just the raw map data: objects and locations.
   The rest of the data like background, music, size, etc is kept in the
   realm files.

   Simon Heath
   4/7/2005
*)


open Config;;
open Gameobj;;


class maploader str =
  let filename = str in

object (self)

  inherit config filename
  initializer
    self#read_file filename

  method private cat2obj category =
    try
      let objfile = cfg2str (Hashtbl.find category "objectfile") in
      let obj = new gameobj objfile in
	obj#moveTo (cfg2float (Hashtbl.find category "x")) 
	  (cfg2float (Hashtbl.find category "y"));
	obj#getVector#set_by_dirmag 
	  (cfg2float (Hashtbl.find category "vectord"))
	  (cfg2float (Hashtbl.find category "vectorm"));
	if (cfg2bool (Hashtbl.find category "fixed")) then
	  obj#setMass 0.;
	obj;
    with
	a -> print_endline ("Error reading map file " ^ filename);
	  raise a;

  method getObjects =
    let foldfunc ky vl lst =
      (self#cat2obj vl) :: lst
    in
      Hashtbl.fold foldfunc data []

  (* This returns an array of the config files for one of each type of 
     object. 
     It may be a better idea to return an array of (name, data) tuples.
  *)
  method getObjectArray =
    (* Okay.  First off, we have a hashtable full of hashtables of stuff, 
       the first one with some duplicates, and we want some structure full 
       of stuff, no duplicates.  *)
(*
    let dst = Hashtbl.create 64 in
      (* This is one weird puppy.  For each sub-hashtable, we get the 
	 value of the "objectfile" key and turn it into a string.
	 Then we check if it's in the dest, and if not, we add it as
	 a key and as a value we load the config file and dump it to
	 a string.
	 Hehe, I love my Resources system so hard.  This pre-caches
	 everything automagically.  Now if my config internals were
	 as nice, we wouldn't slurp an entire file with each #get_string. *)
    let flatten nm vl =
      let objfile = cfg2str (Hashtbl.find vl "objectfile") in
	if not (Hashtbl.mem dst objfile) then
	  Hashtbl.add dst objfile ((Resources.getConfig objfile)#get_string);
    in
      Hashtbl.iter flatten data;
      (* Inefficient, yet somehow satisfying.  The OCaml libraries for 
	 various collections aren't orthogonal enough; we should be able
	 to turn a hashtable into an assoc list. *)
      let foldfunc ky vl lst =
	(ky, vl) :: lst
      in
	Array.of_list (Hashtbl.fold foldfunc dst [])
*)
    let dst = ref [] in
    let flatten nm vl =
      let objfile = cfg2str (Hashtbl.find vl "objectfile") in
	if not (List.mem_assoc objfile !dst) then
	  dst := (objfile, ((Resources.getConfig objfile)#get_string)) ::
	    !dst;
    in
      Hashtbl.iter flatten data;
      Array.of_list !dst;

	
      
end;;
