module Point : sig
  type t = { x : float ; y : float }
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
    | Rotate    of float * Point.t
    | Skew_x    of float
    | Skew_y    of float
end

module Property : sig
  (* TODO: Add more properties *)
  type t =
    | Fill of Color.t
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

val path
  : ?props:(Property.t Frp.Behavior.t array)
  -> Point.t array Frp.Behavior.t (* TODO: Think about this *)
  -> t

val polygon
  : ?props:(Property.t Frp.Behavior.t array)
  -> Point.t array Frp.Behavior.t (* TODO: Think about this *)
  -> t

val transform : t -> Transform.t Frp.Behavior.t -> t

val pictures : t array -> t

val dynamic : t Frp.Behavior.t -> t

val render : t -> (Jq.Dom.t * Frp.Subscription.t)
