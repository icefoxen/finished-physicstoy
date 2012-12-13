(* atom.ml
   A small, round thing that attracts other atoms up to a certain point,
   after which it repels them.

   It might be interesting to play with the force dropoffs... linear, exponential, logathithmic, things that go toward infinity when you get close enough...

*)

open Vector;;

class atom x' y' m attrforce attrrange repforce reprange =
object (self)
  val mass = m
  val af = attrforce
  val ar = attrrange
  val rf = repforce
  val rr = reprange

  val mutable velvector = new vector 0. 0.

  val mutable posvector = new vector x' y'

  val mutable accelvector = new vector 0. 0.
  val mutable prevaccelvector = new vector 0. 0.

  val size = reprange /. 3.

  method getM  = m

  method getV = velvector
  method setV v = velvector <- v

  method getX = posvector#getX
  method getY = posvector#getY
  method setX x' = posvector#setX x'
  method setY y' = posvector#setY y'

  method setXY x' y' = 
    posvector#setXY x' y';

  method getKE =
    0.5 *. m *. (velvector#getMagnitude *. velvector#getMagnitude)

  method getRepRange = rr
  method getAttRange = ar

  method getSize = size



  (* Optimization: Sum up all forces before dividing by mass *)
  method applyForce fvec =
    accelvector <- accelvector#add fvec


  (* Apparently this is called "Very basic Euler integration"
     p = pold + v*t, v = vold + accel.

     More accurate to do:
     p = pold + v*t + 0.5*accel*t^2.

     This is still inaccurate; it falls down badly when the change
     in acceleration is very high... as in during fast collisions.

     Then there's Beeman's Algorithm:
     p = pold + v*t + (2/3)a*t^2 - (1/6)lasta*t^2 
  *)
  method calc dt =
    accelvector <- accelvector#div mass;
    velvector <- velvector#add accelvector;

    let tsquared = dt *. dt in
    let term1 = (accelvector#mul ((2. /. 3.) *. tsquared))
    and term2 = (prevaccelvector#mul ((1. /. 6.) *. tsquared)) in
      posvector <- ((posvector#add (velvector#mul dt))#add term1)#sub term2;

      (*
	let ke = (accelvector#mul 0.5)#mul (dt *. dt) in
	posvector <- (posvector#add (velvector#mul dt))#add ke;
      *)

      prevaccelvector#setXY accelvector#getX accelvector#getY;
      accelvector#setXY 0. 0.;


  (* This is the method that pulls or bumps other atoms. *)
  (* Note that this won't work on the first try; the sign stuff is hard to
     get right
  *)
  method influenceAtom (atom : atom) dt =
    let ox = atom#getX
    and oy = atom#getY in
    let dt = float_of_int dt in

    let dx = posvector#getX -. ox
    and dy = posvector#getY -. oy in


    (* Sqrt is expensive! *)
    let dsquared = (dx *. dx) +. (dy *. dy) in
    let arsquared = ar *. ar
    and rrsquared = rr *. rr in

      (*      Printf.printf "D^2: %f  AR^2: %f  RR^2: %f\n" dsquared arsquared rrsquared; *)

      (* Apply attractive force *)

      if dsquared < arsquared then (
        (* Forces decrease strength with range *)
        let proportion = af *. (arsquared /. dsquared) *. (dt /. 1000.) in
        let forcevector = (new vector dx dy)#getUnitVector#mul proportion in
          self#applyForce forcevector#invert;
          atom#applyForce forcevector;
      );


      (* Apply repulsive force... just the same as above, in the opposite
         direction *)

      if dsquared < rrsquared then (
        let proportion = rf *. (rrsquared /. dsquared) *. (dt /. 1000.) in
        let forcevector = (new vector dx dy)#getUnitVector#mul proportion in
          self#applyForce forcevector;
          atom#applyForce forcevector#invert;
      );

      
  (* I THINK this KINDA works.  Not proportional though *)
  method setTemperature t =
    (*    Printf.printf "Setting temperature: %f " velvector#getMagnitude; *)
    let dt = t -. velvector#getMagnitude in
      if dt = 0. then
	() (* Do nothing *)
      else if dt > 0. then (* t > magnitude *)
	velvector <- velvector#mul (1.1)
      else (* t < magnitude *)
	velvector <- velvector#mul (0.9);

      (* Impose a max speed *)
      if velvector#getMagnitude > 50. then (
	print_endline "Smegging fast atom!";
	velvector <- new vector 1. 1.;
      )

(*    Printf.printf "now: %f\n" velvector#getMagnitude; *)

end;;


(* Mass doesn't matter much.
   Stable things seem to form best when the repulsive force is stronger
   than the attractive force (by any amount), and only slightly smaller in
   range.

   If the attractive force is ANY stronger than the repulsive, it
   runs away and everything accelerates insanely.  

Mjolnir says this is because imprecisions build up in the way I calculate
position; discrete steps instead of a true smooth (difficult-to-simulate) motion
end up taking things closer to other things than they should really be,
resulting in a big force repelling them.  Reducing the timestep or using a
better algorithm are the only obvious ways to make this better.

   A 1:3 ratio of attractive to repulsive force seems to be about the limit.
   The relative size of each field also matters. 

   If the repulsive force is much stronger than the attractive, you seem
   to get hexagonal cells better.
*)


let makeAtom1 x y =
   new atom x y 10. 1. 50. 30. 20.
;;

let makeAtom2 x y =
   new atom x y 10. 1. 30. 10. 20.
;;

let makeAtom3 x y =
   new atom x y 10. 1. 40. 2. 30.
;;


let makeAtom4 x y =
   new atom x y 20. 5. 150. 50. 150.
;;

let makeAtom5 x y =
   new atom x y 10. 40. 70. 25. 40.
;;
