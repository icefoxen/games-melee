(* input.ml
   Manages input, a la the keyboard.  Loading the key definitions, mainly.
   Mapping keys to actions.  Just uses a bunch of 'refs.

   Now, new and improved after a trip to Turkey!  We include an inputmgr
   object to abstract keys away from actions and let the client and server
   talk to each other about it.

   NOTE: Another thing that must be initialized before use!

   Simon Heath
   20/7/2005
*)

open Sdlkey;;
open Config;;



(* Takes the key state and encodes it as an integer, suitable for sending
   over the wonderful world of the internet.
   Haven't decided yet whether it takes a list of pressed keys or whether
   it checks for them itself. 
*)


(* I love Python.  It helps me build things like these.
   Of course, if this were Lisp I could just write a macro, buuut...  
   Anyway.  These are just here to help tranlate to and from
   the config files.  *)
let key2str = function
    KEY_UNKNOWN -> Util.error "key2str: Unknown key!%s\n" ""
  | KEY_BACKSPACE -> "KEY_BACKSPACE"
  | KEY_TAB -> "KEY_TAB"
  | KEY_CLEAR -> "KEY_CLEAR"
  | KEY_RETURN -> "KEY_RETURN"
  | KEY_PAUSE -> "KEY_PAUSE"
  | KEY_ESCAPE -> "KEY_ESCAPE"
  | KEY_SPACE -> "KEY_SPACE"
  | KEY_EXCLAIM -> "KEY_EXCLAIM"
  | KEY_QUOTEDBL -> "KEY_QUOTEDBL"
  | KEY_HASH -> "KEY_HASH"
  | KEY_DOLLAR -> "KEY_DOLLAR"
  | KEY_AMPERSAND -> "KEY_AMPERSAND"
  | KEY_QUOTE -> "KEY_QUOTE"
  | KEY_LEFTPAREN -> "KEY_LEFTPAREN"
  | KEY_RIGHTPAREN -> "KEY_RIGHTPAREN"
  | KEY_ASTERISK -> "KEY_ASTERISK"
  | KEY_PLUS -> "KEY_PLUS"
  | KEY_COMMA -> "KEY_COMMA"
  | KEY_MINUS -> "KEY_MINUS"
  | KEY_PERIOD -> "KEY_PERIOD"
  | KEY_SLASH -> "KEY_SLASH"
  | KEY_0 -> "KEY_0"
  | KEY_1 -> "KEY_1"
  | KEY_2 -> "KEY_2"
  | KEY_3 -> "KEY_3"
  | KEY_4 -> "KEY_4"
  | KEY_5 -> "KEY_5"
  | KEY_6 -> "KEY_6"
  | KEY_7 -> "KEY_7"
  | KEY_8 -> "KEY_8"
  | KEY_9 -> "KEY_9"
  | KEY_COLON -> "KEY_COLON"
  | KEY_SEMICOLON -> "KEY_SEMICOLON"
  | KEY_LESS -> "KEY_LESS"
  | KEY_EQUALS -> "KEY_EQUALS"
  | KEY_GREATER -> "KEY_GREATER"
  | KEY_QUESTION -> "KEY_QUESTION"
  | KEY_AT -> "KEY_AT"
  | KEY_LEFTBRACKET -> "KEY_LEFTBRACKET"
  | KEY_BACKSLASH -> "KEY_BACKSLASH"
  | KEY_RIGHTBRACKET -> "KEY_RIGHTBRACKET"
  | KEY_CARET -> "KEY_CARET"
  | KEY_UNDERSCORE -> "KEY_UNDERSCORE"
  | KEY_BACKQUOTE -> "KEY_BACKQUOTE"
  | KEY_a -> "KEY_a"
  | KEY_b -> "KEY_b"
  | KEY_c -> "KEY_c"
  | KEY_d -> "KEY_d"
  | KEY_e -> "KEY_e"
  | KEY_f -> "KEY_f"
  | KEY_g -> "KEY_g"
  | KEY_h -> "KEY_h"
  | KEY_i -> "KEY_i"
  | KEY_j -> "KEY_j"
  | KEY_k -> "KEY_k"
  | KEY_l -> "KEY_l"
  | KEY_m -> "KEY_m"
  | KEY_n -> "KEY_n"
  | KEY_o -> "KEY_o"
  | KEY_p -> "KEY_p"
  | KEY_q -> "KEY_q"
  | KEY_r -> "KEY_r"
  | KEY_s -> "KEY_s"
  | KEY_t -> "KEY_t"
  | KEY_u -> "KEY_u"
  | KEY_v -> "KEY_v"
  | KEY_w -> "KEY_w"
  | KEY_x -> "KEY_x"
  | KEY_y -> "KEY_y"
  | KEY_z -> "KEY_z"
  | KEY_DELETE -> "KEY_DELETE"
  | KEY_KP0 -> "KEY_KP0"
  | KEY_KP1 -> "KEY_KP1"
  | KEY_KP2 -> "KEY_KP2"
  | KEY_KP3 -> "KEY_KP3"
  | KEY_KP4 -> "KEY_KP4"
  | KEY_KP5 -> "KEY_KP5"
  | KEY_KP6 -> "KEY_KP6"
  | KEY_KP7 -> "KEY_KP7"
  | KEY_KP8 -> "KEY_KP8"
  | KEY_KP9 -> "KEY_KP9"
  | KEY_KP_PERIOD -> "KEY_KP_PERIOD"
  | KEY_KP_DIVIDE -> "KEY_KP_DIVIDE"
  | KEY_KP_MULTIPLY -> "KEY_KP_MULTIPLY"
  | KEY_KP_MINUS -> "KEY_KP_MINUS"
  | KEY_KP_PLUS -> "KEY_KP_PLUS"
  | KEY_KP_ENTER -> "KEY_KP_ENTER"
  | KEY_KP_EQUALS -> "KEY_KP_EQUALS"
  | KEY_UP -> "KEY_UP"
  | KEY_DOWN -> "KEY_DOWN"
  | KEY_RIGHT -> "KEY_RIGHT"
  | KEY_LEFT -> "KEY_LEFT"
  | KEY_INSERT -> "KEY_INSERT"
  | KEY_HOME -> "KEY_HOME"
  | KEY_END -> "KEY_END"
  | KEY_PAGEUP -> "KEY_PAGEUP"
  | KEY_PAGEDOWN -> "KEY_PAGEDOWN"
  | KEY_F1 -> "KEY_F1"
  | KEY_F2 -> "KEY_F2"
  | KEY_F3 -> "KEY_F3"
  | KEY_F4 -> "KEY_F4"
  | KEY_F5 -> "KEY_F5"
  | KEY_F6 -> "KEY_F6"
  | KEY_F7 -> "KEY_F7"
  | KEY_F8 -> "KEY_F8"
  | KEY_F9 -> "KEY_F9"
  | KEY_F10 -> "KEY_F10"
  | KEY_F11 -> "KEY_F11"
  | KEY_F12 -> "KEY_F12"
  | KEY_F13 -> "KEY_F13"
  | KEY_F14 -> "KEY_F14"
  | KEY_F15 -> "KEY_F15"
  | KEY_NUMLOCK -> "KEY_NUMLOCK"
  | KEY_CAPSLOCK -> "KEY_CAPSLOCK"
  | KEY_SCROLLOCK -> "KEY_SCROLLOCK"
  | KEY_RSHIFT -> "KEY_RSHIFT"
  | KEY_LSHIFT -> "KEY_LSHIFT"
  | KEY_RCTRL -> "KEY_RCTRL"
  | KEY_LCTRL -> "KEY_LCTRL"
  | KEY_RALT -> "KEY_RALT"
  | KEY_LALT -> "KEY_LALT"
  | KEY_RMETA -> "KEY_RMETA"
  | KEY_LMETA -> "KEY_LMETA"
  | KEY_LSUPER -> "KEY_LSUPER"
  | KEY_RSUPER -> "KEY_RSUPER"
  | KEY_MODE -> "KEY_MODE"
  | KEY_COMPOSE -> "KEY_COMPOSE"
  | KEY_HELP -> "KEY_HELP"
  | KEY_PRINT -> "KEY_PRINT"
  | KEY_SYSREQ -> "KEY_SYSREQ"
  | KEY_BREAK -> "KEY_BREAK"
  | _ -> Util.error "str2key: Invalid key; some keys aren't supported.\nComplain if it's a big issue%s\n" ""
;;

let str2key = function
    "KEY_UNKNOWN" -> KEY_UNKNOWN
  | "KEY_BACKSPACE" -> KEY_BACKSPACE
  | "KEY_TAB" -> KEY_TAB
  | "KEY_CLEAR" -> KEY_CLEAR
  | "KEY_RETURN" -> KEY_RETURN
  | "KEY_PAUSE" -> KEY_PAUSE
  | "KEY_ESCAPE" -> KEY_ESCAPE
  | "KEY_SPACE" -> KEY_SPACE
  | "KEY_EXCLAIM" -> KEY_EXCLAIM
  | "KEY_QUOTEDBL" -> KEY_QUOTEDBL
  | "KEY_HASH" -> KEY_HASH
  | "KEY_DOLLAR" -> KEY_DOLLAR
  | "KEY_AMPERSAND" -> KEY_AMPERSAND
  | "KEY_QUOTE" -> KEY_QUOTE
  | "KEY_LEFTPAREN" -> KEY_LEFTPAREN
  | "KEY_RIGHTPAREN" -> KEY_RIGHTPAREN
  | "KEY_ASTERISK" -> KEY_ASTERISK
  | "KEY_PLUS" -> KEY_PLUS
  | "KEY_COMMA" -> KEY_COMMA
  | "KEY_MINUS" -> KEY_MINUS
  | "KEY_PERIOD" -> KEY_PERIOD
  | "KEY_SLASH" -> KEY_SLASH
  | "KEY_0" -> KEY_0
  | "KEY_1" -> KEY_1
  | "KEY_2" -> KEY_2
  | "KEY_3" -> KEY_3
  | "KEY_4" -> KEY_4
  | "KEY_5" -> KEY_5
  | "KEY_6" -> KEY_6
  | "KEY_7" -> KEY_7
  | "KEY_8" -> KEY_8
  | "KEY_9" -> KEY_9
  | "KEY_COLON" -> KEY_COLON
  | "KEY_SEMICOLON" -> KEY_SEMICOLON
  | "KEY_LESS" -> KEY_LESS
  | "KEY_EQUALS" -> KEY_EQUALS
  | "KEY_GREATER" -> KEY_GREATER
  | "KEY_QUESTION" -> KEY_QUESTION
  | "KEY_AT" -> KEY_AT
  | "KEY_LEFTBRACKET" -> KEY_LEFTBRACKET
  | "KEY_BACKSLASH" -> KEY_BACKSLASH
  | "KEY_RIGHTBRACKET" -> KEY_RIGHTBRACKET
  | "KEY_CARET" -> KEY_CARET
  | "KEY_UNDERSCORE" -> KEY_UNDERSCORE
  | "KEY_BACKQUOTE" -> KEY_BACKQUOTE
  | "KEY_a" -> KEY_a
  | "KEY_b" -> KEY_b
  | "KEY_c" -> KEY_c
  | "KEY_d" -> KEY_d
  | "KEY_e" -> KEY_e
  | "KEY_f" -> KEY_f
  | "KEY_g" -> KEY_g
  | "KEY_h" -> KEY_h
  | "KEY_i" -> KEY_i
  | "KEY_j" -> KEY_j
  | "KEY_k" -> KEY_k
  | "KEY_l" -> KEY_l
  | "KEY_m" -> KEY_m
  | "KEY_n" -> KEY_n
  | "KEY_o" -> KEY_o
  | "KEY_p" -> KEY_p
  | "KEY_q" -> KEY_q
  | "KEY_r" -> KEY_r
  | "KEY_s" -> KEY_s
  | "KEY_t" -> KEY_t
  | "KEY_u" -> KEY_u
  | "KEY_v" -> KEY_v
  | "KEY_w" -> KEY_w
  | "KEY_x" -> KEY_x
  | "KEY_y" -> KEY_y
  | "KEY_z" -> KEY_z
  | "KEY_DELETE" -> KEY_DELETE
  | "KEY_KP0" -> KEY_KP0
  | "KEY_KP1" -> KEY_KP1
  | "KEY_KP2" -> KEY_KP2
  | "KEY_KP3" -> KEY_KP3
  | "KEY_KP4" -> KEY_KP4
  | "KEY_KP5" -> KEY_KP5
  | "KEY_KP6" -> KEY_KP6
  | "KEY_KP7" -> KEY_KP7
  | "KEY_KP8" -> KEY_KP8
  | "KEY_KP9" -> KEY_KP9
  | "KEY_KP_PERIOD" -> KEY_KP_PERIOD
  | "KEY_KP_DIVIDE" -> KEY_KP_DIVIDE
  | "KEY_KP_MULTIPLY" -> KEY_KP_MULTIPLY
  | "KEY_KP_MINUS" -> KEY_KP_MINUS
  | "KEY_KP_PLUS" -> KEY_KP_PLUS
  | "KEY_KP_ENTER" -> KEY_KP_ENTER
  | "KEY_KP_EQUALS" -> KEY_KP_EQUALS
  | "KEY_UP" -> KEY_UP
  | "KEY_DOWN" -> KEY_DOWN
  | "KEY_RIGHT" -> KEY_RIGHT
  | "KEY_LEFT" -> KEY_LEFT
  | "KEY_INSERT" -> KEY_INSERT
  | "KEY_HOME" -> KEY_HOME
  | "KEY_END" -> KEY_END
  | "KEY_PAGEUP" -> KEY_PAGEUP
  | "KEY_PAGEDOWN" -> KEY_PAGEDOWN
  | "KEY_F1" -> KEY_F1
  | "KEY_F2" -> KEY_F2
  | "KEY_F3" -> KEY_F3
  | "KEY_F4" -> KEY_F4
  | "KEY_F5" -> KEY_F5
  | "KEY_F6" -> KEY_F6
  | "KEY_F7" -> KEY_F7
  | "KEY_F8" -> KEY_F8
  | "KEY_F9" -> KEY_F9
  | "KEY_F10" -> KEY_F10
  | "KEY_F11" -> KEY_F11
  | "KEY_F12" -> KEY_F12
  | "KEY_F13" -> KEY_F13
  | "KEY_F14" -> KEY_F14
  | "KEY_F15" -> KEY_F15
  | "KEY_NUMLOCK" -> KEY_NUMLOCK
  | "KEY_CAPSLOCK" -> KEY_CAPSLOCK
  | "KEY_SCROLLOCK" -> KEY_SCROLLOCK
  | "KEY_RSHIFT" -> KEY_RSHIFT
  | "KEY_LSHIFT" -> KEY_LSHIFT
  | "KEY_RCTRL" -> KEY_RCTRL
  | "KEY_LCTRL" -> KEY_LCTRL
  | "KEY_RALT" -> KEY_RALT
  | "KEY_LALT" -> KEY_LALT
  | "KEY_RMETA" -> KEY_RMETA
  | "KEY_LMETA" -> KEY_LMETA
  | "KEY_LSUPER" -> KEY_LSUPER
  | "KEY_RSUPER" -> KEY_RSUPER
  | "KEY_MODE" -> KEY_MODE
  | "KEY_COMPOSE" -> KEY_COMPOSE
  | "KEY_HELP" -> KEY_HELP
  | "KEY_PRINT" -> KEY_PRINT
  | "KEY_SYSREQ" -> KEY_SYSREQ
  | "KEY_BREAK" -> KEY_BREAK
  | x -> Util.error "str2key: Undefined key: %s\n" x
;;

(*
let loadKeyDefs () =
  let c = Resources.getConfig "keys.cfg" in
  let getkey f = str2key (c#getStr "keys" f) in
    turnright := getkey "turnright";
    turnleft := getkey "turnleft";
    thrust := getkey "thrust";
    fire := getkey "fire";
    special := getkey "special";
    nextWep := getkey "nextWep";
    prevWep := getkey "prevWep";

    help := KEY_F1;
    pause := KEY_p;
    menu := KEY_ESCAPE;
;;
*)


let thrustmask = 1
and turnlmask = 2
and turnrmask = 4
and firemask = 8
and specialmask = 16
and cruisemask = 32 

and balance2shieldmask = 64
and balance2battmask = 128
and transfer2shieldmask = 256
and transfer2battmask = 512
and incwepmask = 1024
and decwepmask = 2048
;;


class inputmgr cfgfile =
  let cfg = Resources.getConfig cfgfile in

object (self)
  (* Key states 
     Hmm, it might be easier to just keep all these as a bitmap.
  *)
  val mutable keymask = 0

(*
  val mutable thrusts = false
  val mutable turnls = false
  val mutable turnrs = false
  val mutable fires = false
  val mutable specials = false
  val mutable cruises = false

  val mutable balance2shields = false
  val mutable balance2batts = false
  val mutable transfer2shields = false
  val mutable transfer2batts = false
  val mutable incweps = false
  val mutable decweps = false
*)
		   
  (* Key bindings *)
  val thrustb = str2key (cfg#getStr "keys" "thrust")
  val turnlb = str2key (cfg#getStr "keys" "turnleft")
  val turnrb = str2key (cfg#getStr "keys" "turnright")
  val fireb = str2key (cfg#getStr "keys" "fire")
  val specialb = str2key (cfg#getStr "keys" "special")
  val cruiseb = str2key (cfg#getStr "keys" "cruise")

  val balance2shieldb = str2key (cfg#getStr "keys" "balance2shield")
  val balance2battb = str2key (cfg#getStr "keys" "balance2batt")
  val transfer2shieldb = str2key (cfg#getStr "keys" "transfer2shield")
  val transfer2battb = str2key (cfg#getStr "keys" "transfer2batt")
  val incwepb = str2key (cfg#getStr "keys" "incweapon")
  val decwepb = str2key (cfg#getStr "keys" "decweapon")

  val mutable incwepf = false
  val mutable decwepf = false

  val mutable keysHaveChanged = false

  method isKeystateNew = keysHaveChanged

  method isKeyPressed k =
    (keymask land k) > 0
      
  method private pressKey k =
    keymask <- (keymask lor k);

  method private unPressKey k =
    keymask <- (keymask lxor k);

  (* ...rar. 
     Okay.  Either we can send the state each frame, or only when it's
     changed.
     KISS.  Each frame.  But some things SHOULD only be sent when
     it's changed, like balance and wep changes...
     Well, we can handle that there...

     RAWR.  So, what happens is this:
     The clientrealm tells the keystate to poll the keyboard, which sets
     the keystate appropriately.
     The clientrealm then performs the appropriate action on it's player.
     The clientrealm grabs the keystate and sends it to the server.
     The serverrealm decodes the keystate and gives it to the player's
     inputmgr, which sets itself accordingly.
     The player checks the inputmgr and the time, and does the appropriate
     actions on the ship.
  *)
  method encode =
    keymask

  method decode c =
    keymask <- c

  method readKeyStrokes =
    ignore (Sdlevent.poll());

    (* Might want to reset the keymask to 0 here... 
       No, 'cause it has to be set/unset AFTER we do the incwep and
       so on...
       Maybe we build a new keymask and land it with the old one?
       That could work, I think.  Binary logic is so much fun.
       XXX: Do it once we know the dumb way works.
       We can also decide whether to send a new event to the server depending 
       on whether the keymap has changed!
    *)

    let oldkeymask = keymask in
      keymask <- 0;

      if (is_key_pressed thrustb) then
	self#pressKey thrustmask;

      if (is_key_pressed turnrb) then
	self#pressKey turnrmask;

      if (is_key_pressed turnlb) then
	self#pressKey turnlmask;

      if (is_key_pressed fireb) then
	self#pressKey firemask;

      if (is_key_pressed specialb) then
	self#pressKey specialmask;

      (* A bit of cleverness to only register key-down's... 
	 Whenever the key is pressed, if it wasn't pressed last frame,
	 we do the action.  If the key is not pressed, we undo the
	 flag.
      *)

      if (is_key_pressed incwepb) then
	if not incwepf (*((oldkeymask land incwepmask) > 0)*) then (
	  self#pressKey incwepmask;
	  incwepf <- true
	)
	else ()
      else
	incwepf <- false;

      if (is_key_pressed decwepb) then
	if not decwepf (*((oldkeymask land decwepmask) > 0)*) then (
	  self#pressKey decwepmask;
	  decwepf <- true;
	)
	else ()
      else
	decwepf <- false;

      (*
	if (is_key_pressed incwepb) then
	if not ((oldkeymask land incwepmask) > 0) then
	self#pressKey incwepmask;

	if (is_key_pressed decwepb) then
	if not ((oldkeymask land decwepmask) then (
	self#pressKey decwepmask;
	)
	else
	()
	else
	self#unPressKey decwepmask;

      *)
      if (is_key_pressed cruiseb) then
	if not ((oldkeymask land cruisemask) > 0) then
	  self#pressKey cruisemask;


(*      Printf.printf "Key presses done; result: %d\n" keymask; *)
      keysHaveChanged <- not (oldkeymask = keymask);
(*      if keysHaveChanged then
	print_endline "Keys have changed!"; *)



end;;
