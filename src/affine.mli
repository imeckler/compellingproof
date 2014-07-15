type t =
  { translation : Vector.t
  ; matrix      : Matrix.t
  }

val identity : t

val compose : t -> t -> t

