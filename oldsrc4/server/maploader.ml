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

  method numObjects =
    let i = ref 0 in
      Hashtbl.iter (fun x y -> incr i) data;
      !i;

  method getObjects =
(*    Printf.printf "Getting objects, of which we have %d\n" self#numObjects; *)
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
