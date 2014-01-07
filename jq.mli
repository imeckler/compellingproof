type t

val create : string -> t

val append : t -> t -> unit

val css : t -> (string * string) array -> unit
