(* clientmain.ml
   Main client logic and setup.

   Simon Heath
   22/6/2005
*)


open Config;;
open Sdlvideo;;
open Sdlkey;;
open Util;;
open Sdlevent;;



let fillColor surf col =
   fill_rect ~rect: (surface_info surf).clip_rect surf (map_RGB surf col)
;;


let mainloop scr bkg =
  let r = new Realms.realm "testrealm.cfg" in
    r#doMainloop scr;
;;


let main () =

  (* Init... messing up the order of things, esp. config-files, could be bad
  *)
  Sdl.init [`EVERYTHING];
  Sdlwm.set_caption ~title: "Voidrunner Client" ~icon: "";
  Sdlmouse.show_cursor false;

  Sdlttf.init ();
  Audio.initSound 16; 
  Random.self_init ();

  Input.loadKeyDefs ();



  (* Grafix setup *)
  let screen = set_video_mode ~w: !screenx ~h: !screeny ~bpp: 16 
    [`DOUBLEBUF; `SWSURFACE] in
  

  let bkg = create_RGB_surface_format screen [`HWSURFACE] ~w: !screenx ~h: !screeny in

    fill_rect bkg (map_RGB bkg black);
  

    (*set_color_key screen (get_pixel screen ~x: 0 ~y: 0); *)

    (* Mainloop *)

    mainloop screen bkg;


    (* De-init *)
    Sdlttf.quit ();
    Sdl.quit ()    
;;


let _ =
  main ();;

