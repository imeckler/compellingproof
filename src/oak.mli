open Core

module Resource : sig
  type 'a t

  val with_many
    : string array
    -> (Image.t array -> 'a Frp.Stream.t)
    -> 'a Frp.Stream.t

  val (+>) : (Image.t -> 'a) t -> string -> 'a t

  val run : 'a Frp.Stream.t t -> 'a Frp.Stream.t
end 

module Line_cap : sig
  type t = [`Flat | `Round | `Padded]
end

module Line_join : sig
  type t = [`Smooth | `Sharp of float | `Clipped]
end

module Line_style : sig
  type t =
    { color : Color.t
    ; width : float
    ; cap : Line_cap.t
    ; join : Line_join.t
    ; dashing : int array
    ; dash_offset : int
    }

  val default : t

  val solid  : Color.t -> t
  val dashed : Color.t -> t
  val dotted : Color.t -> t
end

module Fill_style : sig
  type t =
    | Solid of Color.t
    | Texture of Image.t
    | Grad of Color.Gradient.t
end

module Path : sig
  type t

  val create : (float * float) array -> t
end

module Shape : sig
  type t

  val polygon : (float * float) array -> t

  val rect : float -> float -> t

  val square : float -> t

  val oval : float -> float -> t

  val circle : float -> t

  val ngon : int -> float -> t
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

  val group : [`Array of t array | `Iterator of t Iterator.t] -> t

  val transform_group : Affine.t -> [`Array of t array | `Iterator of t Iterator.t] -> t

  val draw
    : width:int -> height:int
    -> Dom_html.element Js.t
    -> t Frp.Behavior.t
    -> Frp.Subscription.t
end

