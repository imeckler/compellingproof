module Control : sig
  type 'a t

  module When : sig
    type 'a t = [ `Fresh of 'a | `Stale of 'a ]
  end

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val ap : ('a -> 'b) t -> 'a t -> 'b t

  val clicks : Jq.Event.Mouse.Click.t When.t t

  val drags : Jq.Event.Mouse.Drag.t When.t t

  val drag_point : (int * int) -> (int * int) When.t t

  val slider : string -> float When.t t
  
  val ticks : float -> Time.t When.t t
end

type 'a t

val (+>) : ('a -> 'b) t -> 'a Control.t -> 'b t

val create : Jq.t -> Jq.t -> 'a -> 'a t

(* val run : 'a t -> unit *)

