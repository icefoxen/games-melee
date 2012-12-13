(* sprite.ml
   A sprite class.  A sprite is anything that is drawn.

   It can also be animated, frame-by-frame.  You pass one big
   image with all the frames, set the delay and size and initial y value,
   and when it's drawn it slides through all the frames on the X axis 
   (or can be set to specific frames for certain circumstances).
   It's a bit of a kludge.

   At the moment all it does is rotate through the frames, flipping
   to the next one once with each call to #anim.  This is generally called
   by the mainloop one way or another, and thus is dependant on framerate.
   More flexible options could be:
   1) Include a timer so it always animates at a constant rate, and
   2) Have it so a you can provide a LIST of delays so it animates at
   a VARIABLE rate.

   Simon Heath
*)

open Sdlvideo;;
open Config;;
open Resources;;


(* Okay.  The question is, which is more efficient?  Making it so sprites can
   have several animations with the same general parameters (numframes, delay,
   size), or just use several different sprites?
   The former would be more efficient in certain situations...  a "running
   left" animation would have the same parameters as a "running right"
   animation.  However, I don't think it's that big a deal at this point...
*)
class sprite cfgfile =
  let cfg = getConfig cfgfile in
object (self)
  val mutable img = getImg (cfg#getStr "sprite" "image")
  val imgw = cfg#getInt "sprite" "imgw"
  val imgh = cfg#getInt "sprite" "imgh"

  val mutable numframes = cfg#getInt "sprite" "numframes"
  val mutable animdelay = cfg#getInt "sprite" "animdelay"
  val mutable alpha = -1  (* XXX: Not implemented yet! *)
  val mutable currentanimdelay = 0
  val mutable currentframe = 0

  val mutable frame = {r_x = 0; r_y = 0; r_w = 0; r_h = 0}

  val mutable dest = {r_x = 0; r_y = 0; r_w = 0; r_h = 0}

  initializer
    frame.r_w <- imgw / numframes;
    frame.r_h <- imgh;
    dest.r_w <- frame.r_w;
    dest.r_h <- imgh;
	

  method getDest = dest

(* Animation methods *)
  method setFrameX x =
    frame.r_x <- x
  method getFrameX = frame.r_x

  method setFrameY y =
    frame.r_y <- y
  method getFrameY = frame.r_y

  method setFrameW w =
    frame.r_w <- w
  method getFrameW = frame.r_w

  method setFrameH h =
    frame.r_h <- h
  method getFrameH = frame.r_h

  method setFrameCount n =
    numframes <- n;
    frame.r_x <- 0;
    frame.r_w <- imgw / numframes
  method getFrameCount = numframes

  method getCurrentFrame = currentframe
  method setFrame n = 
    currentframe <- n mod numframes;
    frame.r_x <- currentframe * frame.r_w

  method nextFrame =
    currentframe <- (currentframe + 1) mod numframes;
    self#setFrame currentframe

  method prevFrame =
    currentframe <- abs ((currentframe - 1) mod numframes);
    self#setFrame currentframe    

  method anim =
    if currentanimdelay = 0 then (
      currentanimdelay <- animdelay;
      self#nextFrame
    )
    else
      currentanimdelay <- currentanimdelay - 1

  method setDelay n = 
    animdelay <- n;
    currentframe <- 0


(* Imagery methods *)
  method getAlpha =
    get_alpha img;
    
  method setAlpha n =
    set_alpha img n;


  method setImage s =
    img <- s
  method getImage = img

  method moveTo x y =
    dest.r_x <- x;
    dest.r_y <- y

  method getX = dest.r_x
  method setX n =
    dest.r_x <- abs n

  method getY = dest.r_y
  method setY n =
    dest.r_y <- abs n

  method refreshDest =
    dest.r_w <- frame.r_w;
    dest.r_h <- frame.r_h

  method blit dst =
    let dstw, dsth, _ = (surface_dims dst) in
      if dest.r_x > dstw || dest.r_y > dsth then
	()
      else
	blit_surface ~src: img ~src_rect: frame ~dst: dst ~dst_rect: dest ();

      if animdelay > 0 then
	self#anim

  method blitWrap dst =
    let dstw, dsth, _ = (surface_dims dst) in
      if dest.r_x > dstw then
	dest.r_x <- Util.absmod dest.r_x dstw
      else if dest.r_y > dsth then
	dest.r_y <- Util.absmod dest.r_y dsth;

      blit_surface ~src: img ~src_rect: frame ~dst: dst ~dst_rect: dest ();

      if animdelay > 0 then
	self#anim
    

  method print =
    ()
	 

end;;


(* Okay.  To scroll the background, we need to check whether the screen
   is completely in it's bounds.  If it is, we're cool.
   Else, we must take the missing part from the far edge of it and jam it
   in, and if the missing edge is over the size of the screen, move the
   background sprite.

   Might it be simpler to simply have a background surface and view the
   screen rect as scrolling over it?  Well, that's really what happens, so...

   XXX: Screw this, I'm going to have to make a new background object,
   aren't I...  Hmm.

   And actually, things may make more sense if Realms just specify the
   behavior of background and foreground, and the mainloop actually does it.
   Well, maybe.  Not necessarily.
*)

class background cfgfile =

object (self)
inherit sprite cfgfile
  val mutable xwrap = {r_x = 0; r_y = 0; r_w = 0; r_h = 0}
  val mutable ywrap = {r_x = 0; r_y = 0; r_w = 0; r_h = 0}
(* So we have frame, the source rect, and dest, the dest rect...
   We also have xwrap and ywrap, the rects that specify how much the frame
   is wrapping around.
*)

  initializer
    xwrap.r_x <- 0;
    xwrap.r_y <- frame.r_y;
    xwrap.r_w <- 0;
    xwrap.r_h <- frame.r_h;
    ywrap.r_x <- frame.r_x;
    ywrap.r_y <- 0;
    ywrap.r_w <- frame.r_w;
    ywrap.r_h <- 0;


  method blit dst =
    if dest.r_x > frame.r_x - frame.r_w then
      Printf.printf "Wrap!\n";
    let dstw, dsth, _ = (surface_dims dst) in
      if dest.r_x > dstw then (
	xwrap.r_w <- dest.r_x - dstw;
	blit_surface ~src: img ~src_rect: xwrap ~dst: dst ~dst_rect: dest ();
      )
      else if dest.r_y > dsth then
	  ()
      else
	blit_surface ~src: img ~src_rect: frame ~dst: dst ~dst_rect: dest ();

      if animdelay > 0 then
	self#anim



 end;;
