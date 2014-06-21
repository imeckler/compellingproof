include module type of Oak_common

module Resource : sig
  type 'a t

  val with_many
    : string array
    -> (Image.t array -> 'a Frp.Stream.t)
    -> 'a Frp.Stream.t

  val (+>) : (Image.t -> 'a) t -> string -> 'a t

  val run : 'a Frp.Stream.t t -> 'a Frp.Stream.t
end 

module Form : sig
  type t

  val filled : Color.t -> Shape.t -> t

  val textured : Image.t -> Shape.t -> t

  val gradient : Color.Gradient.t -> Shape.t -> t

  val outlined : Line_style.t -> Shape.t -> t

  val traced : Line_style.t -> Path.t -> t

  val sprite : int -> int -> (int * int) -> Image.t -> t

  val move : float * float -> t -> t

  val move_x : float -> t -> t

  val move_y : float -> t -> t

  val scale : float -> t -> t

  (* TODO: Angles are in radians. Change this to use Angle.t *)
  val rotate : float -> t -> t

  val alpha : float -> t -> t

  val group : t array -> t

  val transform_group : Transform.t -> t array -> t

  val draw
    : width:int -> height:int
    -> Dom_html.element Js.t
    -> t Frp.Behavior.t
    -> Frp.Subscription.t
end

