module Control : sig
  type 'a t

  val clicks : Input.Mouse.Click.t Frp.Stream.t t

  val drags_with : button:Input.Mouse.Button.t -> (int * int) Frp.Stream.t t

  val drag_point_with : init:(int * int) -> button:Input.Mouse.Button.t -> (int * int) Frp.Behavior.t t

  val continuous_slider : string -> float Frp.Behavior.t t

  val step_slider : int -> int Frp.Behavior.t t

  val incr_decr : ?bot:int -> ?top:int -> unit -> int Frp.Behavior.t t
end

type 'a t

val (+>) : ('a -> 'b) t -> 'a Control.t -> 'b t

val create : width:int -> height:int -> Jq.t -> 'a -> 'a t

val run : Draw.t t -> Frp.Subscription.t

(* val run : 'a t -> unit *)

