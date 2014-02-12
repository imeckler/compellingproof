module Easing : sig
  type ('a, 'b) t

  type free
  type fixed

  val linear    : Time.Span.t -> (free, free) t
  val quadratic : Time.Span.t -> (free, free) t
  val cubic     : Time.Span.t -> (free, free) t
  val quintic   : Time.Span.t -> (free, free) t

  val anchor_left  : float -> (free, 'b) t -> (fixed, 'b) t
  val anchor_right : float -> ('a, free) t -> ('a, fixed) t

  val glue_r : ('a, fixed) t -> (free, 'b) t -> ('a, 'b) t
  val glue_l : ('a, free) t -> (fixed, 'b) t -> ('a, 'b) t

  val interpret : (fixed, fixed) t -> (Time.Span.t -> float)

  val behavior : (fixed, fixed) t -> float -> float Frp.Behavior.t
end

module Sequence : sig
  type ('b, 'a) t

  type finished
  type unfinished

  val for_
    : float
    -> ('a -> float -> 'a)
    -> (unfinished, 'a) t

  val forever : ('a -> float -> 'a) -> (finished, 'a) t

  val (&>)
    : (unfinished, 'a) t
    -> ('b, 'a) t
    -> ('b, 'a) t

  val stay_for : float -> (unfinished, 'a) t

  val stay_forever : (finished, 'a) t

  val run : init:'a -> (finished, 'a) t -> 'a Frp.Behavior.t

  val quadratic
    : float
    -> init:float 
    -> final:float
    -> (unfinished, float) t

  (*
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
