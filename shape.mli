module Pos : sig
  type t = { x : float; y : float }
end

module Color : sig
  type t

  val of_rgb : r:int -> g:int -> b:int -> t

  val render : t -> string
end

module Property : sig
  type 'a t

  val render : t -> string
end

type t

val render : t -> Jq.t

