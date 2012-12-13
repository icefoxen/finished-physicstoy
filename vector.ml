(* vector.ml
   Vector class!

*)


class vector givenx giveny =
object (self)
  val mutable x = givenx
  val mutable y = giveny

  method getX = x
  method getY = y

  method setX x' = x <- x'
  method setY y' = y <- y'
  method setXY x' y' =
    x <- x';
    y <- y'

  method getMagnitude =
    sqrt (x *. x) +. (y *. y)

  method getUnitVector =
    self#div self#getMagnitude

  method add (v : vector) =
    new vector (x +. v#getX) (y +. v#getY)

  method sub (v : vector) =
    new vector (x -. v#getX) (y -. v#getY)

  method mul term =
    new vector (x *. term) (y *. term)

  method div term =
    new vector (x /. term) (y /. term)

  method invert =
    new vector (-.x) (-.y)
end;;
