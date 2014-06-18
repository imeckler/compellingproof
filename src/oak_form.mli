open Oak_common

type t

val filled : Color.t -> Shape.t -> t

val textured : string -> Shape.t -> t

val gradient : Color.Gradient.t -> Shape.t -> t

val outlined : Line_style.t -> Shape.t -> t

val traced : Line_style.t -> Path.t -> t

val sprite : int -> int -> (int * int) -> string -> t

val move : float * float -> t -> t

val move_x : float -> t -> t

val move_y : float -> t -> t

val scale : float -> t -> t

(* TODO: Angles are in radians. Change this to use Angle.t *)
val rotate : float -> t -> t

val alpha : float -> t -> t

val group : t array -> t

val transform_group : Transform.t -> t array -> t

