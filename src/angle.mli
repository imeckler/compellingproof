type t

val rotate
  : about:float Point.t
  -> float Point.t
  -> t
  -> float Point.t

val pi : float

val cos : t -> float
val sin : t -> float
val tan : t -> float
val cot : t -> float

val acos : float -> t
val asin : float -> t
val atan : float -> t
val atan2 : float -> float -> t

val (+) : t -> t -> t
val (-) : t -> t -> t
val ( * ) : float -> t -> t

val to_degrees : t -> float

val to_radians : t -> float

val of_degrees : float -> t

val of_radians : float -> t

val about : center:float Point.t -> float Point.t -> t

