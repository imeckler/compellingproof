module Line_cap : sig
  type t = [`Flat | `Round | `Padded]
end

module Line_join : sig
  type t = [`Smooth | `Sharp of float | `Clipped]
end

module Image : sig
  type t = Dom_html.imageElement Js.t
  val load : string -> t Frp.Stream.t
end

module Fill_style : sig
  type t =
    | Solid of Color.t
    | Texture of Image.t
    | Grad of Color.Gradient.t
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


module Path : sig
  type t = (float * float) array

  val create : (float * float) array -> t
end

module Shape : sig
  type t = (float * float) array

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

module Image_style : sig
  type t = Plain | Fitted | Cropped of int * int | Tiled
end

module Direction : sig
  type t = Up | Down | Left | Right | In | Out
end

module Alignment : sig
  type t = Left | Right | Center | Justify | Initial | Inherit

  val to_string : t -> string
end

module Position : sig
  type pos = Absolute of int | Relative of float

  val pos_to_js_string : pos -> Js.js_string Js.t

  type t =
    { horizontal : [`P | `Z | `N]
    ; vertical   : [`P | `Z | `N]
    ; x : pos
    ; y : pos
    }

  val middle       : t
  val top_left     : t
  val top_right    : t
  val bottom_left  : t
  val bottom_right : t
  val mid_left     : t
  val mid_right    : t
  val mid_top      : t
  val mid_bottom   : t
end

module Text : sig
  module Style : sig
    module Line : sig
      type t = Under | Over | Through
    end

    type t =
      { typeface : string array
      ; height   : float option
      ; color    : Color.t
      ; bold     : bool
      ; italic   : bool
      ; line     : Line.t option
      }

    val default : t
  end

  type t

  val of_string : string -> t

  val style : Style.t -> t -> t

  val to_html : t -> string
end


