(* text.ml
   Handles rendering and saving text surfaces.
   Cutting and scaling text to a certain size, all that good stuff.

   Simon Heath
*)

open Sdlvideo;;


let grabText txt fnt col =
  Sdlttf.render_text_solid (Resources.getFont fnt) txt ~fg: col;;


(* Could this create an actual Sprite object with the text in it...? *)
let drawText txt fnt col x y surf = 
  ()
;;

let drawTextAbs txt fnt col x y surf =
  let s = grabText txt fnt col in
  let sx, sy, _ = surface_dims s in
    
    blit_surface ~src: s ~dst: surf 
      ~dst_rect: {r_x = x; r_y = y; r_w = sx; r_h = sy}  ()
;;
