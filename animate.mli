module Sequence : sig
  type ('b, 'a) t

  type finished
  type unfinished

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

  val run : init:'a -> (finished, 'a) t -> 'a Frp.Behavior.t

  val quadratic
    : float
(*     -> init:float  *)
    -> final:float
    -> (unfinished, float) t

  val jump_to : 'a -> (unfinished, 'a) t

  (*
  val quadratic_cont
    : float
    -> final:float
    -> (unfinished, float) t

  val continue
    : ('a -> Time.t -> Time.Span.t -> 'a)
    -> (unfinished, 'a) t
  (first (fun t -> 0.), of_ms 500)
  +> (continue (fun x0 t0 t -> x0 +. t), of_ms 300)
  +> for_ever (fun x0 t0 t -> x0)
  *)
end

(*
val sequence
  : (float *. 'a Frp.Behavior.t) array (* TODO: Consider using time instead of float *)
  -> 'a Frp.Behavior.t
*)
