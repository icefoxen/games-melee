(* audio.ml
   Sound and music players.
   This also takes care of the music loader and unloader.
   Sound chunks are NOT freed.  They are hung onto by the Resource module.
   I hope I don't need too many...  Probably won't.
   If necessary, we can have a function that plays a sound and then frees
   it, or maybe modify the Resouce module to use weak pointers.
   
   (init_sound channels) MUST be called before any of the functions here!!
   Doing Sdl.init `AUDIO is probably also necessary.
   These are for music and sound respectively, probably.

   Simon Heath
*)

open Resources;;

let channelnum = ref 0;;
let mus = ref (Sdlmixer.load_music (!datadir ^ "audio/testy.ogg"));;

let initSound n =
  Sdl.init_subsystem [`AUDIO];
  Sdlmixer.open_audio ();
  channelnum := Sdlmixer.allocate_channels n;
;;

(* Make sure we don't play more sounds at once than we have channels...
   O'course, your sound might not be played then, but oh well.
*)
let playSound n = 
  if Sdlmixer.num_playing_channel () < !channelnum then 
    Sdlmixer.play_sound (getSound n);
;;

(* Plays a song forever.  Only one song is played at a time.  The song is
   freed after being played.  Waaaah! *)
let playMusic n =
  if Sdlmixer.playing_music () then (
    Sdlmixer.fadeout_music 2.0;
    Sdlmixer.free_music !mus
  );
  mus := Sdlmixer.load_music (!datadir ^ "audio/" ^ n);
  Sdlmixer.play_music ~loops: (-1) !mus
;;
