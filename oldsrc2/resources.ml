(* resources.ml
   A hashtable of resources, ie images, sounds, or musics.
   This makes sure I don't have to load everything a gajillion
   times and waste lots of space.
   Configs, Scheme programs, and conversations may also eventually be stored 
   here; we'll see how it works.  Realms too.  Hell, everything.

   Just a hashtable and some loading code, really.  
   I rather love hashtables.  ^_^

   There are two options: either it loads all the given resources immediately,
   or it waits until a resource is demanded before loading it.  Startup
   wait vs. runtime wait.  I think I'll go for the latter, since the overall
   effect is probably better and it'll never happen twice.  Yeah.

   So now I'm still worried about what happens if lots of resources are
   loaded and none are ever freed.  I am NOT going to build a conservative
   GC into this thing!  Ah well, it'll probably be okay; the only thing that
   might cause a problem are the sounds.

   The loadfoo funcs are helper.  The getfoo funcs are the interface.

   Simon Heath
*)
   

let imgtbl = Hashtbl.create 24;;
let soundtbl = Hashtbl.create 24;;
let configtbl = Hashtbl.create 24;;
let fonttbl = Hashtbl.create 8;;

let datadir = ref "./"


let loadImg name =
  try
    let n = Sdlvideo.display_format ~alpha: true 
	      (Sdlloader.load_image (!datadir ^ name)) in
      Sdlvideo.set_color_key n (Sdlvideo.map_RGB n (255,0,255));
      Hashtbl.add imgtbl name (Sdlvideo.display_format ~alpha: true n)
  with
      Sdlloader.SDLloader_exception n -> 
	Util.error "resources.ml: loadImg: Couldn't load %s\n" name
;;

let loadSound name =
  try
    let n = Sdlmixer.loadWAV (!datadir ^ "audio/" ^ name) in
      Hashtbl.add soundtbl name n
  with
      Sdlmixer.SDLmixer_exception n ->
	Util.error "resources.ml: loadSound: Couldn't load %s\n" name
;;

let loadConfig name =
  let n = new Config.config (!datadir ^ name) in
    Hashtbl.add configtbl name n
;;


let loadFont name =
  let n = Sdlttf.open_font (!datadir ^ name) 16 in
    Hashtbl.add fonttbl name n
;;


let getImg name  =
  if not (Hashtbl.mem imgtbl name) then
    loadImg name;
  Hashtbl.find imgtbl name
;;


let getSound name =
  if not (Hashtbl.mem soundtbl name) then
    loadSound name;
  Hashtbl.find soundtbl name
;;


let getConfig name =
  if not (Hashtbl.mem configtbl name) then
    loadConfig name;
  Hashtbl.find configtbl name
;;
    
let getFont name =
  if not (Hashtbl.mem fonttbl name) then
    loadFont name;
  Hashtbl.find fonttbl name
;;
