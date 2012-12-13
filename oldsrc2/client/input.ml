
(* input.ml
   Manages input, a la the keyboard.  Loading the key definitions, mainly.
   Mapping keys to actions.  Just uses a bunch of 'refs.

   NOTE: Another thing that must be initialized before use!

   Simon Heath
*)

open Sdlkey;;
open Config;;



(* Gameplay keys *)

let turnright = ref KEY_0
and turnleft = ref KEY_0
and thrust = ref KEY_0
and fire = ref KEY_0
and special = ref KEY_0
and nextWep = ref KEY_0
and prevWep = ref KEY_0

(* Interface keys *)
and help = ref KEY_0
and pause = ref KEY_0
and menu = ref KEY_0
;;

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

