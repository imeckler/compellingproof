module Node : sig
  type ('a, 'b) t
end

module Change : sig
  type ('a, 'b) t =
    | Add_arc    of ('a, 'b) Node.t * ('a, 'b) Node.t * 'b
    | Remove_arc of ('a, 'b) Node.t * ('a, 'b) Node.t
end

type ('a, 'b) t

val get : ('a, 'b) Node.t -> ('a, 'b) t -> 'a option

val empty : ('a, 'b) t

val add_nodes : 'a array -> ('a, 'b) t -> (('a, 'b) Node.t array * ('a, 'b) t)

val successors : ('a, 'b) Node.t -> ('a, 'b) t -> (('a, 'b) Node.t * 'b) array option

val successors_exn : ('a, 'b) Node.t -> ('a, 'b) t -> (('a, 'b) Node.t * 'b) array

val predecessors : ('a, 'b) Node.t -> ('a, 'b) t -> (('a, 'b) Node.t * 'b) array option

val predecessors_exn : ('a, 'b) Node.t -> ('a, 'b) t -> (('a, 'b) Node.t * 'b) array

val nodes : ('a, 'b) t -> ('a, 'b) Node.t array
