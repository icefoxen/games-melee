(* gameobj.ml
   Game object.  It moves, it accelerates and decellerates, it bounces off
   other things and doesn't go through them, it is affected by gravity.

   ...how much physics do I want to implement in these?  The model really
   ISN'T like Edge of Infinity...
   Yeah, it's a lot simpler.

   ...damn.  What about the ground and such things?

   Simon Heath
   11/3/2005
*)

open Resources;;
open Sprite;;
open Area;;
open Vector;;
open Util;;

let gameObjID = ref (-1);;

let getID () =
  incr gameObjID;
  !gameObjID;
;;

class gameobj cfgfile =
  let cfg = getConfig cfgfile in
object (self)
  val mutable area = new area cfgfile
  val mutable sprite = new sprite cfgfile
  val mutable vector = new vector

  (* Mass = 0. -> immobile *)
  val mutable mass = cfg#getFloat "gameobj" "mass"
  val mutable hits = cfg#getInt "gameobj" "hits"
  val mutable alive = true
  val mutable visible = true
  val mutable id = getID ()
  val mutable typeIndex = -1
  val mutable lastUpdateTime = 0

(* We init and cache.  I'm insane enough to wish this single if wasn't
   necessary.
   We can't just initialize typeIndex to the right thing, since the
   index may not be init'ed when we create a gameobject!
   Hey, I just invented late binding!  *)
  method getTypeIndex =
    if typeIndex < 0 then (
      typeIndex <- Typeindex.str2index cfgfile;
      typeIndex
    )
    else (
      typeIndex
    )

  method getLastUpdate = lastUpdateTime

  method setLastUpdate x = lastUpdateTime <- x




  method acceldm d m =
    vector#accel_by_dirmag d m

  method accelv v =
    vector#add v

  method accel xoff yoff =
    if mass > 0. then
      vector#addxy xoff yoff

  method calculate t =
    if mass > 0. then
      area#moveto (vector#movex area#getx t) (vector#movey area#gety t);
    
  method getEnergy =
    vector#get_m *. mass

  method setVelocityByEnergy e =
    Printf.printf "Old dirmag: %f %f\n" vector#get_d vector#get_m;
    vector#set_by_dirmag (vector#get_d) (e /. mass);
    Printf.printf "New dirmag: %f %f\n" vector#get_d vector#get_m;
    

  (* This is almost right for nonelastic collisions.  I think.
     If two objects collide, they exchange energy and vector.
  *)



  (* XXX: Fuckit
     method doCollision (o : gameobj) =
     let ar = o#getArea in
     let left1 = area#getx
     and left2 = ar#getx
     and bottom1 = area#gety
     and bottom2 = ar#gety in
     let right1 = left1 +. area#getw
     and right2 = ar#getx +. ar#getw
     and top1 = bottom1 +. area#geth
     and top2 = ar#gety +. ar#geth in
     if bottom1 < top2 then
     area#sety (top2 +. 1.)
     else if top1 > bottom2 then
     area#sety (bottom2 -. 1.);
     if right1 > left2 then
     area#setx (left2 +. 1.)
     else if left1 < right2 then
     area#setx (right2 -. 1.)
  *)


  (* This checks if you're colliding with something...  if so, it reverses
     your direction and moves you until you're not, on the basis that this
     will move you away from it.
  *)

  (* XXX: Maybe add a bit of more precise collision-detection here; pixel-
     perfect or bitmask, perhaps --only activated if the bounding-box hits,
     of course.  
     The vector-intersect REALLY doesn't help much.  Maybe if we draw a few
     more lines and see if they intersect... but then we start worrying about
     efficiency.
  *)
  method isColliding (o : gameobj) t =
    let px1 = area#getx
    and py1 = area#gety
    and px2 = (vector#movex area#getx t) 
    and py2 = (vector#movey area#gety t)
    and px3 = o#getArea#getx
    and py3 = o#getArea#gety
    and px4 = o#getVector#movex o#getArea#getx t
    and py4 = o#getVector#movey o#getArea#gety t in
      (doLinesIntersect px1 py1 px2 py2 px3 py3 px4 py4) ||
      (area#isColliding o#getArea);

  (*
    method isInBounds x y w h =
    let left1 = area#getx
    and left2 = x
    and bottom1 = area#gety
    and bottom2 = y
    and right1 = area#getx +. area#getw
    and right2 = x +. w
    and top1 = area#gety +. area#geth
    and top2 = x +. h in
    if bottom1 > top2 then
    false
    else if top1 < bottom2 then
    false
    else if right1 < left2 then
    false
    else if left1 > right2 then
    false
    else
    true
  *)

  (* XXX: This is somewhat horribly wrong...  Getting less so tho.
     We just have to make energy work...  And handling, of course.
  *)
  method impact (other : gameobj) =
    let ovec = other#getVector in
      (* Check for immovable objects *)
      if mass = 0. then
	(* Not quite right, but... *)
	ovec#reverse

      else if other#getMass = 0. then
	vector#reverse
      else (
	let massdiff = mass /. other#getMass
	and diffmass = other#getMass /. mass in
	let sx, sy = (vector#getx *. massdiff), (vector#gety *. massdiff)
	and ox, oy = (ovec#getx *. diffmass), (vector#gety *. diffmass) in
	  vector#setx ox;
	  vector#sety oy;
	  ovec#setx sx;
	  ovec#sety sy;
      );


  (*
    let d = vector#get_d
    and m = vector#get_m in
    vector#set_by_dirmag ovec#get_d ovec#get_m;
    ovec#set_by_dirmag d m;
  *)
  (*
    let othere = other#getEnergy
    and selfe = self#getEnergy
    and tmp = vector in
    vector <- other#getVector;
    other#setVector tmp;
    self#moveTo (area#getx -. 50.) (area#gety -. 50.);
    self#setVelocityByEnergy othere;
    other#setVelocityByEnergy selfe;
  *)

      

  (*      vector#reverse; 
	  while self#isColliding other 1 do
	  print_endline "Backing up...";
	  area#moveto (area#getx -. vector#getx) (area#gety -. vector#gety);
	  done;
	  vector#reverse; 
	  vector <- other#getVector;
	  other#setVector tmp;*)
  (*
    self#setVelocityByEnergy othere;
    other#setVelocityByEnergy selfe;
  *)

  (*      Printf.printf "Selfe: %f Othere: %f\n" self#getEnergy other#getEnergy; *)
  (*
    let ovel = selfe /. other#getMass
    and svel = othere /. mass
    and odir = vector#get_d
    and sdir = ovec#get_d  in
    ovec#set_by_dirmag odir ovel;
    vector#set_by_dirmag sdir svel;
    Printf.printf "Self d: %f m %f\n" vector#get_d vector#get_m;
    Printf.printf "Other: d: %f m %f\n" other#getVector#get_d other#getVector#get_m;
  *)




  (* Okay.  This gets called whenever the given object dies.  It checks to
     see if the object has a death animation, such as an explosion.  If it
     does, then it creates a new particle from the given config file and
     queues it up.
     It can also do things like set score and play sounds, later.

     XXX:
     ...okay.  There's a slight problem here; namely, we use the
     particle system to do the death animations, but particles are
     themselves gameobjs's.  That could lead to just a little bit of endless
     recursion which, while cool in concept, is neither useful or actually
     possible right now.  So either we make particles something different
     that's just like gameobj's or we make some upper class of gameobj's that
     has a death anim or we implement this seperately on various things like
     ships, rocks, shots...  Which I am reluctant to do since I can't stand
     deep object heirarchies with each new level adding one method.
     Now, if we had mix-in's...  We do!  I'm really not sure I want to use
     them though.
     Well hmm, what if we make "die" take a thunk that does whatever death-
     stuff is necessary?  Yeah...  That could work, slightly less messily.
     Kinda.
     ...Okay, I hereby question the sanity of this entire endeavor, and
     will probably make it so that ships and so on just override the "die"
     method.
     If I want sparkly engine trails, then dammit, I can make a sparkly
     bit in the engine-trail sprite.

     Yeah, doing it on a class-by-class basis seems to be the best idea,
     really.  
  *)


  (* XXX: This looks like a good idea right up until you realize it
     creates an infinate loop that I don't know how to break sanely. 
     I mean, I can't really give a gameobject a "hasDied" variable, can I?
     That's just dumb.  "This object dies.  Has it already died?"  
     ...the problem is, it would work... *)
  method makeDeathObjs : gameobj list =
    (*
      let da = new gameobj deathAnim in
      da#moveTo area#getx area#gety;
      [da]
    *)
    [new gameobj cfgfile]
      

  method die = 
    alive <- false;


  (* If damage starts out negative, it's invulnerable. *)
  method damage x =
    if hits < 0 then
      ()
    else (
      hits <- hits - x;
      if hits < 0 then
	self#die
    )


  method setSize w' h' =
    area#setsize w' h'

  method getH = area#geth
  method setH n = 
    area#seth n

  method getW = area#getw
  method setW n = 
    area#setw n

  method getX = area#getx
  method getY = area#gety

  method setX x' =
    area#setx x'

  method setY y' =
    area#sety y'

  method moveTo x' y' =
    area#moveto x' y'

  method getID =
    id

  method setID x = id <- x

  method draw dst scrx scry =
    if visible then (
      let nx = (x2screen area#getx scrx (float_of_int sprite#getFrameW))
      and ny = (y2screen area#gety scry (float_of_int sprite#getFrameH)) in

	(*	Printf.printf "nx: %d ny: %d\n" nx ny;
		Printf.printf "area#gety: %f scry: %f\n" area#gety scry;
		
		let nx = (x2screen area#getx scrx area#getw)
		and ny = (y2screen area#gety scry area#geth) in
	*)
	sprite#moveTo nx ny;
	(* sprite#blit automatically checks whether you're in the screen area *)
	sprite#blit dst
    )

  (* This draws a rectangle around the sprite's bounding box.
     Well, the sprite's area's edges, really.  They're not exactly the same,
     since area#isColliding takes each edge in by 10%.  Close enough.
  *)
  method drawBox dst scrx scry =
    let nx = (x2screen area#getx scrx area#getw)
    and ny = (y2screen area#gety scry area#geth)
    and w = (int_of_float area#getw)
    and h = (int_of_float area#geth) in
      Drawing.drawRect dst nx ny w h Sdlvideo.green;


  (* This draws the object's vector as a line. *)
  method drawVector dst scrx scry =
    let x1 = (x2screen area#getx scrx area#getw)
    and y1 = (y2screen area#gety scry area#geth) in
    let x2 = x1 + (int_of_float vector#getx)
    and y2 = y1 - (int_of_float vector#gety) in
      Drawing.drawLine dst x1 y1 x2 y2 Sdlvideo.green;

  method drawMeta scr scrx scry =
    self#drawBox scr scrx scry;
    self#drawVector scr scrx scry;

  method isAlive = alive
  method isVisible = visible

  method setAlive n =
    alive <- n

  method setVisible n =
    visible <- n

  method getSprite = sprite
  method getArea = area

  method setSprite s =
    sprite <- s

  method setArea a =
    area <- a


  method getHits = hits
  method setHits x = hits <- x

  method getVector = vector
  method setVector v =
    vector <- v

  method getMass = mass
  method setMass m = mass <- m

  method toPacket time =
    let p = {
      Net.oe_id = id;
      Net.oe_type = self#getTypeIndex;
      Net.oe_time = time;
      Net.oe_x = (int_of_float area#getx);
      Net.oe_y = (int_of_float area#gety);
      Net.oe_dx = (int_of_float vector#getx);
      Net.oe_dy = (int_of_float vector#gety);
    } in
      Net.Objecte( p )

  method getConfig = cfgfile

  (* Sprinkles the object randomly in a certain range... *)
  (*
    method sprinkle x y r vectoroffset =
    ()
  *)

  method print =
    area#print;
    sprite#print;
    vector#print;
    Printf.printf "Gameobj: hits: %d visible: %b\n" hits visible

end;;
