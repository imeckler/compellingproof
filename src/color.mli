type t = { r : int; g : int; b : int; alpha : float }
type color = t

val of_rgb : ?alpha:float -> int -> int -> int -> t

val random : unit -> t

val white : t
val black : t
val red   : t
val green : t
val blue  : t
val none  : t

module Gradient : sig
  type t =
    | Linear of (float * float) * (float * float) * (float * color) array
    | Radial of (float * float) * float * (float * float) * float * (float * color) array
end
