type ('b, 'a) t

type finished
type unfinished

val run : init:'a -> (finished, 'a) t -> 'a Frp.Behavior.t

val for_
  : float
  -> ('a -> float -> 'a)
  -> (unfinished, 'a) t

val forever : ('a -> float -> 'a) -> (finished, 'a) t

val cycle : (unfinished, 'a) t -> (finished, 'a) t

val (>>)
  : (unfinished, 'a) t
  -> ('b, 'a) t
  -> ('b, 'a) t

val stay_for : float -> (unfinished, 'a) t

val stay_forever : (finished, 'a) t

val quadratic
  : float
  -> final:float
  -> (unfinished, float) t

val linear
  : float
  -> final:float
  -> (unfinished, float) t

val jump_to : 'a -> (unfinished, 'a) t

