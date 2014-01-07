type t

val create : string -> t

val append : t -> t -> unit

val css : t -> (string * string) array -> unit

val on : t -> string -> (Dom_html.event Js.t -> unit) -> unit

val clicks : t -> (int * int) Frp.Stream.t

val drags : t -> (int * int) Frp.Stream.t

