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

(*
val sequence
  : (float *. 'a Frp.Behavior.t) array (* TODO: Consider using time instead of float *)
  -> 'a Frp.Behavior.t
*)
