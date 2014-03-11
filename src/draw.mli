module Point : sig
  type 'a t = 'a * 'a
end

module Angle : sig
  type t

  val rotate
    : about:float Point.t
    -> float Point.t
    -> t
    -> float Point.t

  val pi : float

  val cos : t -> float
  val sin : t -> float

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : float -> t -> t

  val to_degrees : t -> float

  val to_radians : t -> float

  val of_degrees : float -> t

  val of_radians : float -> t
end

module Color : sig
  type t = { r : int; g : int; b : int; alpha : float }

  val of_rgb : ?alpha:float -> r:int -> g:int -> b:int -> t

  val white : t
  val black : t
  val red   : t
  val blue  : t
end

module Transform : sig
  type t =
    | Matrix    of float * float * float * float * float * float
    | Translate of float * float
    | Scale     of float * float
    | Rotate    of Angle.t * float Point.t
    | Skew_x    of float
    | Skew_y    of float
end

module Property : sig
  (* TODO: Add more properties *)
  module Stroke : sig
    module Linecap : sig
      type t = [ `Butt | `Square | `Round ]
    end

    module Linejoin : sig
      type t = [ `Miter | `Round | `Bevel ]
    end
  end

  type t

  val fill : Color.t -> t

  val stroke
    : ?cap:Stroke.Linecap.t
    -> ?join:Stroke.Linejoin.t
    -> Color.t
    -> int
    -> t
end

module Segment : sig
  type t

  val arc : Angle.t -> Angle.t -> [`long | `short] -> float -> t

  val line_to : float Point.t -> t

  val move_to : float Point.t -> t
end

type t

val circle
  : ?props:(Property.t Frp.Behavior.t array)
  -> float Frp.Behavior.t
  -> float Point.t Frp.Behavior.t
  -> t

val rect
  : ?props:(Property.t Frp.Behavior.t array)
  -> width:float Frp.Behavior.t
  -> height:float Frp.Behavior.t
  -> float Point.t Frp.Behavior.t
  -> t

(* The value [(a, b)] of the mask behavior should satisfy 
   [0. <= a <= b <= 1.]. It is used to specify which portion
   of the path should be visible.
*)
val path
  : ?props:(Property.t Frp.Behavior.t array)
  -> ?mask:(float * float) Frp.Behavior.t
  -> anchor:(float Point.t Frp.Behavior.t)
  -> Segment.t array Frp.Behavior.t
  -> t

val polygon
  : ?props:(Property.t Frp.Behavior.t array)
  -> float Point.t array Frp.Behavior.t (* TODO: Think about this *)
  -> t

val text
  : ?props:(Property.t Frp.Behavior.t array)
  -> string Frp.Behavior.t
  -> float Point.t Frp.Behavior.t
  -> t

(* Inject an already constructed node. This function does not check to
   make sure the node is valid.
*)
val svg : Jq.Dom.t -> t

val transform : t -> Transform.t Frp.Behavior.t -> t

val pictures : t array -> t

val dynamic : t Frp.Behavior.t -> t

val render : t -> (Jq.Dom.t * Frp.Subscription.t)

