module Node : sig
  type ('a, 'b) t
end

module Change : sig
  type ('a, 'b) t =
    | Add_arc     of ('a, 'b) Node.t * ('a, 'b) Node.t * 'b
    | Remove_arc  of ('a, 'b) Node.t * ('a, 'b) Node.t
    | Remove_node of ('a, 'b) Node.t
end

type ('a, 'b) t

val get : ('a, 'b) Node.t -> ('a, 'b) t -> 'a option

val get_exn : ('a, 'b) Node.t -> ('a, 'b) t -> 'a

(* All operations are externally pure. The unit argument is necessary to avoid
 * the value restriction *)
val empty : unit -> ('a, 'b) t

val add_node : 'a -> ('a, 'b) t -> (('a, 'b) Node.t * ('a, 'b) t)

val add_nodes : 'a array -> ('a, 'b) t -> (('a, 'b) Node.t array * ('a, 'b) t)

val change : ('a, 'b) Change.t array -> ('a, 'b) t -> ('a, 'b) t

val successors : ('a, 'b) Node.t -> ('a, 'b) t -> (('a, 'b) Node.t * 'b) array option

val successors_exn : ('a, 'b) Node.t -> ('a, 'b) t -> (('a, 'b) Node.t * 'b) array

val predecessors : ('a, 'b) Node.t -> ('a, 'b) t -> (('a, 'b) Node.t * 'b) array option

val predecessors_exn : ('a, 'b) Node.t -> ('a, 'b) t -> (('a, 'b) Node.t * 'b) array

val nodes : ('a, 'b) t -> ('a, 'b) Node.t array

val map_nodes : ('a, 'b) t -> f:('a -> 'c) -> ('c, 'b) t

val map_arcs : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

val fold_nodes
  : ('a, 'b) t
  -> init:'accum
  -> f:('accum -> ('a, 'b) Node.t -> 'a -> 'accum)
  -> 'accum

val iter_nodes : ('a, 'b) t -> f:(('a, 'b) Node.t -> 'a -> unit) -> unit

val iter_arcs : ('a, 'b) t -> f:(('a, 'b) Node.t -> ('a, 'b) Node.t -> 'b -> unit) -> unit

val length : ('a, 'b) t -> int
