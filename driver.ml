(* driver.ml
   This is the class that actually does the mainloop stuff.
   It is completely seperate from rooms and such.
*)


open Util;;
open Sdlkey;;

open Atom;;
open Vector;;



let makeRandomAtom () =
  let xpos = Random.float 800.
  and ypos = Random.float 600.
  and xvel = (Random.float 1.) -. 0.5
  and yvel = (Random.float 1.) -. 0.5 in
  let atom = makeAtom5 xpos ypos in
    atom#applyForce (new vector xvel yvel);
    atom
;;

let makeRandomAtom2 () =
  let xpos = Random.float 800.
  and ypos = Random.float 600.
  and xvel = (Random.float 1.) -. 0.5
  and yvel = (Random.float 1.) -. 0.5 in
  let atom = makeAtom3 xpos ypos in
    atom#applyForce (new vector xvel yvel);
    atom
;;


class driver scr =
object (self)
  val mutable objects : Atom.atom list = []

  val mutable framecount = 0
  val mutable lastframe = 0
  val mutable framestarttime = 0
  val mutable dt = 0
  val mutable maxdt = 0
  val mutable energysum = 0.

  val mutable temp = 0.0

  val mutable continue = true

  val drawer = new Drawer.drawer scr




  initializer
    objects <- Array.to_list (Array.init 50 (fun x -> makeRandomAtom ()));
    (* objects <- objects @
      Array.to_list (Array.init 50 (fun x -> makeRandomAtom2 ())); *)


  method drawObjects : unit =
    drawer#drawAtoms objects;





  method draw =
    drawer#clearBackground;
    drawer#drawForces objects;
    self#drawObjects;

  method doInput =
    ignore (Sdlevent.poll());
    if (is_key_pressed KEY_q) then
      self#stopGame;
    

  method collideObjects =
    let rec loop current rest =
      if rest = [] then
	()
      else (
	List.iter (fun (x : atom) -> current#influenceAtom x dt) rest;
	loop (List.hd rest) (List.tl rest)
      )
    in
      loop (List.hd objects) (List.tl objects)


  method updateTimers =
    lastframe <- framestarttime;
    framestarttime <- Sdltimer.get_ticks ();
    framecount <- framecount + 1;

    (* Max timestep is 75ms.  Any longer and things can fall through
       things. *)
    dt <- min 75 (framestarttime - lastframe);
    maxdt <- max dt maxdt;

  method boundObject x =
    let vec = x#getV in
    let vecx = vec#getX
    and vecy = vec#getY in
      if ((x#getX > 800.) && (vecx > 0.)) or
	((x#getX < 0.) && (vecx < 0.)) then (
	  x#setV (new vector (-.vecx) vecy);
	  x#setTemperature temp;
	);

      if ((x#getY > 600.) && (vecy > 0.)) or
	((x#getY < 0.) && (vecy < 0.)) then (
	  x#setV (new vector vecx (-.vecy));
	  x#setTemperature temp;
	);

  (* Fixed timestep is actually a bonus for simulations... *)
  method calculate =
    List.iter (fun x -> x#calc 1.) objects;
    List.iter self#boundObject objects;


  method getTotalEnergy =
    List.fold_left (fun x y -> x +. y#getKE) 0. objects;

  method reportEnergy e =
    let avgfps = ((float_of_int framecount) *. 1000.) /.
      (float_of_int (Sdltimer.get_ticks ())) in
    let avge = (energysum /. (float_of_int framecount)) in

      Printf.printf "Frame %d, avg fps: %5.1f, KE: %f, avg KE: %f\n" 
	framecount avgfps e avge;



  method mainLoop =
    while continue do

      self#updateTimers;
      self#doInput;
      self#calculate;
      self#collideObjects;
      self#draw;

      let e = self#getTotalEnergy in
	energysum <- energysum +. e;

	if (framecount mod 200) = 0 then
	  self#reportEnergy e;

	flush stdout;
	Sdlvideo.flip scr;
    done;



  method addObject x =
    objects <- x :: objects

  method addObjects x =
    objects <- x @ objects

  method removeObject x =
    objects <- List.filter (fun itm -> itm <> x) objects


  (* This should eventually lead to a main menu or such, but for now
     just quits *)
  method stopGame =
    continue <- false;
    Printf.printf "Max dt: %d\n" dt;

end;;
