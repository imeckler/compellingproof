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

  val acos : float -> t
  val asin : float -> t
  val atan : float -> t

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : float -> t -> t

  val to_degrees : t -> float

  val to_radians : t -> float

  val of_degrees : float -> t

  val of_radians : float -> t

  val about : center:float Point.t -> float Point.t -> t
end

module Transform : sig
  type t =
    | Matrix    of float * float * float * float * float * float
    | Translate of float * float
    | Scale     of float * float
    | Rotate    of Angle.t * float Point.t
    | Skew_x    of float
    | Skew_y    of float

  val translate : float * float -> t
  val scale : float -> float -> t
  val rotate : ?about:float Point.t -> Angle.t -> t
  val skew_x : float -> t
  val skew_y : float -> t
end

module Property : sig
  type t

  val any : name:string -> value:string -> t
end

module Color : sig
  type t = { r : int; g : int; b : int; alpha : float }

  val of_rgb : ?alpha:float -> int -> int -> int -> t

  val random : unit -> t

  val white : t
  val black : t
  val red   : t
  val green : t
  val blue  : t
  val none  : t
end

(* TODO: Add more properties *)
module Stroke : sig
  type t

  module Linecap : sig
    type t = [ `Butt | `Square | `Round ]
  end

  module Linejoin : sig
    type t = [ `Miter | `Round | `Bevel ]
  end

  val create
    : ?cap:Linecap.t
    -> ?join:Linejoin.t
    -> Color.t
    -> int
    -> t
end

module Segment : sig
  type t

  val arc : Angle.t -> Angle.t -> [`long | `short] -> float -> t

  val line_to : float Point.t -> t

  val arc_to : float Point.t -> [`long | `short] -> [`pos | `neg] -> float -> t

  val move_to : float Point.t -> t
end

module Name : sig
  type t

  val create : unit -> t

  val clicks : t -> Jq.Event.Mouse.Click.t Frp.Stream.t

  val drags  : t -> (int * int) Frp.Stream.t
end

type t

type 'k with_shape_args
  = ?name:Name.t
  -> ?fill:Color.t Frp.Behavior.t
  -> ?stroke:Stroke.t Frp.Behavior.t
  -> ?props:(Property.t Frp.Behavior.t array)
  -> 'k

val circle
  : (float Frp.Behavior.t -> float Point.t Frp.Behavior.t -> t)
    with_shape_args

val rect
  :(width:float Frp.Behavior.t
  -> height:float Frp.Behavior.t
  -> float Point.t Frp.Behavior.t
  -> t)
  with_shape_args

(* The value [(a, b)] of the mask behavior should satisfy 
   [0. <= a <= b <= 1.]. It is used to specify which portion
   of the path should be visible.
*)
val path
  : (?mask:(float * float) Frp.Behavior.t
  -> anchor:(float Point.t Frp.Behavior.t)
  -> Segment.t array Frp.Behavior.t
  -> t)
  with_shape_args

val path_string
  :(?mask:(float * float) Frp.Behavior.t
  -> string Frp.Behavior.t
  -> t)
  with_shape_args

val polygon
  :(float Point.t array Frp.Behavior.t (* TODO: Think about this *)
  -> t)
  with_shape_args

val text
  :(string Frp.Behavior.t
  -> float Point.t Frp.Behavior.t
  -> t)
  with_shape_args

val image
  : width:int Frp.Behavior.t
  -> height:int Frp.Behavior.t
  -> string Frp.Behavior.t
  -> t

val svg_file : string -> t

(* Inject an already constructed node. This function does not check to
   make sure the node is valid.
*)

val svg : Jq.Dom.t -> t

val transform : t -> Transform.t array Frp.Behavior.t -> t

val translate : t -> (float * float) Frp.Behavior.t -> t

val pictures : t array -> t

val dynamic : t Frp.Behavior.t -> t

val render : t -> (Jq.Dom.t * Frp.Subscription.t)

val render_svg_node : width:int -> height:int -> t -> (Jq.Dom.t * Frp.Subscription.t)

(* val render_canvas : t -> Jq.Dom.t -> unit *)

