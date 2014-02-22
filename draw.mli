module Point : sig
  type t = { x : float ; y : float }
end

module Angle : sig
  type t

  val rotate
    : about:Point.t
    -> Point.t
    -> t
    -> Point.t

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
    | Rotate    of Angle.t * Point.t
    | Skew_x    of float
    | Skew_y    of float
end

module Property : sig
  (* TODO: Add more properties *)
  module Linecap : sig
    type t = [ `Butt | `Square | `Round ]
  end

  module Linejoin : sig
    type t = [ `Miter | `Round | `Bevel ]
  end

  type t

  val fill : Color.t -> t

  val stroke
    : ?cap:Linecap.t
    -> ?join:Linejoin.t
    -> Color.t
    -> int
    -> t
end

type t

val circle
  : ?props:(Property.t Frp.Behavior.t array)
  -> float Frp.Behavior.t
  -> Point.t Frp.Behavior.t
  -> t

val rect
  : ?props:(Property.t Frp.Behavior.t array)
  -> width:float Frp.Behavior.t
  -> height:float Frp.Behavior.t
  -> Point.t Frp.Behavior.t
  -> t

module Segment : sig
  type t

  val arc : Angle.t -> Angle.t -> [`long | `short] -> float -> t

  val line_to : Point.t -> t

  val move_to : Point.t -> t
end

val path
  : ?props:(Property.t Frp.Behavior.t array)
  -> Point.t array Frp.Behavior.t (* TODO: Think about this *)
  -> t

val path
  : ?props:(Property.t Frp.Behavior.t array)
  -> anchor:(Point.t Frp.Behavior.t)
  -> Segment.t array Frp.Behavior.t
  -> t

val polygon
  : ?props:(Property.t Frp.Behavior.t array)
  -> Point.t array Frp.Behavior.t (* TODO: Think about this *)
  -> t

val text
  : ?props:(Property.t Frp.Behavior.t array)
  -> string Frp.Behavior.t
  -> Point.t Frp.Behavior.t
  -> t

val transform : t -> Transform.t Frp.Behavior.t -> t

val pictures : t array -> t

val dynamic : t Frp.Behavior.t -> t

val render : t -> (Jq.Dom.t * Frp.Subscription.t)

