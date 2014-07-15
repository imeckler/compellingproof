type t = float * float * float * float

val mul : t -> t -> t

val apply : t -> Vector.t -> Vector.t

val identity : t

module Infix : sig
  val ( * ) : t -> t -> t
end
