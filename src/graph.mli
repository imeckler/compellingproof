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

(* Throws exception if the adjacency list references a vertex
 * which isn't given a value in the list *)
val of_adjacency_lists : ('a * (int * 'b) list) list -> ('a, 'b) t

val get : ('a, 'b) t -> ('a, 'b) Node.t -> 'a option

val get_exn : ('a, 'b) t -> ('a, 'b) Node.t -> 'a


(* All operations are externally pure. The unit argument is necessary to avoid
 * the value restriction *)
val empty : unit -> ('a, 'b) t

val add_node : ('a, 'b) t -> 'a -> (('a, 'b) Node.t * ('a, 'b) t)

val add_nodes : ('a, 'b) t -> 'a array -> (('a, 'b) Node.t array * ('a, 'b) t)

val change : ('a, 'b) t -> ('a, 'b) Change.t array -> ('a, 'b) t

val successors : ('a, 'b) t -> ('a, 'b) Node.t -> (('a, 'b) Node.t * 'b) array option

val successors_exn : ('a, 'b) t -> ('a, 'b) Node.t -> (('a, 'b) Node.t * 'b) array

val predecessors : ('a, 'b) t -> ('a, 'b) Node.t -> (('a, 'b) Node.t * 'b) array option

val predecessors_exn : ('a, 'b) t -> ('a, 'b) Node.t -> (('a, 'b) Node.t * 'b) array

val nodes : ('a, 'b) t -> ('a, 'b) Node.t array

val map_nodes : ('a, 'b) t -> f:('a -> 'c) -> ('c, 'b) t

val map_arcs : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

val fold_nodes
  : ('a, 'b) t
  -> init:'accum
  -> f:('accum -> ('a, 'b) Node.t -> 'a -> 'accum)
  -> 'accum

val fold_arcs
  : ('a, 'b) t
  -> init:'accum
  -> f:('accum -> ('a, 'b) Node.t -> ('a, 'b) Node.t -> 'b -> 'accum)
  -> 'accum

val iter_nodes : ('a, 'b) t -> f:(('a, 'b) Node.t -> 'a -> unit) -> unit

val iter_arcs : ('a, 'b) t -> f:(('a, 'b) Node.t -> ('a, 'b) Node.t -> 'b -> unit) -> unit

val length : ('a, 'b) t -> int

type ('a, 'b) graph = ('a, 'b) t

module Mutable : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t

  val of_adjacency_lists : ('a * (int * 'b) list) list -> ('a, 'b) t

  val get : ('a, 'b) t -> ('a, 'b) Node.t -> 'a option

  val get_exn : ('a, 'b) t -> ('a, 'b) Node.t -> 'a

  val add_node : ('a, 'b) t -> 'a -> ('a, 'b) Node.t

  val add_nodes : ('a, 'b) t -> 'a array -> ('a, 'b) Node.t array

  val remove_node : ('a, 'b) t -> ('a, 'b) Node.t -> unit

  val add_arc : ('a, 'b) t -> src:('a, 'b) Node.t -> dst:('a, 'b) Node.t -> 'b -> unit

  val remove_arc : ('a, 'b) t -> src:('a, 'b) Node.t -> dst:('a, 'b) Node.t -> unit

  val successors : ('a, 'b) t -> ('a, 'b) Node.t -> (('a, 'b) Node.t * 'b) array option

  val successors_exn : ('a, 'b) t -> ('a, 'b) Node.t -> (('a, 'b) Node.t * 'b) array

  val predecessors : ('a, 'b) t -> ('a, 'b) Node.t -> (('a, 'b) Node.t * 'b) array option

  val predecessors_exn : ('a, 'b) t -> ('a, 'b) Node.t -> (('a, 'b) Node.t * 'b) array

  val nodes : ('a, 'b) t -> ('a, 'b) Node.t array

  val map_nodes : ('a, 'b) t -> f:('a -> 'c) -> ('c, 'b) t

  val map_arcs : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

  val fold_nodes
    : ('a, 'b) t
    -> init:'accum
    -> f:('accum -> ('a, 'b) Node.t -> 'a -> 'accum)
    -> 'accum

  val fold_arcs
    : ('a, 'b) t
    -> init:'accum
    -> f:('accum -> ('a, 'b) Node.t -> ('a, 'b) Node.t -> 'b -> 'accum)
    -> 'accum

  val iter_nodes : ('a, 'b) t -> f:(('a, 'b) Node.t -> 'a -> unit) -> unit

  val iter_arcs : ('a, 'b) t -> f:(('a, 'b) Node.t -> ('a, 'b) Node.t -> 'b -> unit) -> unit

  val length : ('a, 'b) t -> int

  val freeze : ('a, 'b) t -> ('a, 'b) graph
end

(* Experimental interface *)

module Builder : sig
  type ('k, 'a, 'b) t

  (* TODO: Think about if there's any downside to keeping Builder.node
   * different from Node.t *)

  type ('a, 'b) node

  val new_node : 'a -> (('a, 'b) node, 'a, 'b) t

  val add_arc : ('a, 'b) node -> ('a, 'b) node -> 'b -> (unit, 'a, 'b) t

  val remove_arc : ('a, 'b) node -> ('a, 'b) node -> (unit, 'a, 'b) t

  include Monad.S3 with type ('k, 'a, 'b) t := ('k, 'a, 'b) t

  val replicate : int -> ('k, 'a, 'b) t -> ('k list, 'a, 'b) t

  val replicate_ignore : int -> ('k, 'a, 'b) t -> (unit, 'a, 'b) t

  val run : ('a, 'b) graph -> ('k, 'a, 'b) t -> ('a, 'b) graph
end

val draw
  : ?charge_constant :float
  -> ?spring_constant : float
  -> width : int
  -> height : int
  -> Jq.t
  -> ('a, 'b) t
  -> unit

