open Core

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

module Transform : sig
  type t =
    { translation : float * float
    ; matrix      : float * float * float * float
    }

  val identity : t

  val compose : t -> t -> t
end


