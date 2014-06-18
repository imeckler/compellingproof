module Image_style : sig
  type t = Plain | Fitted | Cropped of int * int | Tiled
end

module Position : sig
  type pos = Absolute of int | Relative of float

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

module Direction : sig
  type t = Up | Down | Left | Right | In | Out
end

module Alignment : sig
  type t = Left | Right | Center | Justify | Initial | Inherit
end

type t
type element = t

val collage : int -> int -> Oak_form.t array -> t

val width  : t -> int
val height : t -> int
val size   : t -> int

val with_width  : int -> t -> t
val with_height : int -> t -> t
val with_size   : width:int -> height:int -> t -> t
val with_color  : Color.t -> t -> t

val spacer : width:int -> height:int -> t

val empty  : t

val link   : string -> t -> t

val image  : ?style:Image_style.t -> width:int -> height:int -> string -> t

val container : width:int -> height:int -> Position.t -> t -> t

val flow : Direction.t -> t array -> t

val (---) : t -> t -> t
val (|||) : t -> t -> t

val layers : t array -> t

