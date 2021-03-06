type t

module Dom : sig
  type t = Dom_html.element Js.t

  val append : t -> t -> unit

  val set_attr : t -> name:string -> value:string -> unit

  val sink_attr : t -> name:string -> value:string Frp.Behavior.t -> Frp.Subscription.t

  val set_html : t -> string -> unit

  val sink_html : t -> string Frp.Behavior.t -> Frp.Subscription.t

  val empty : t -> unit

  val svg_node : string -> (string * string) array -> t
end

val body : t

val to_dom_node : t -> Dom.t option

val wrap : Dom.t -> t

val create : string -> t

val jq : string -> t option

val find : string -> t option

val find_descendants : t -> string -> t array

val children : t -> Dom.t array

val append : t -> t -> unit

(** [insert_after t1 t2] inserts t2 into the dom after t1 *)
val insert_after  : t -> t -> unit
(** [insert_after t1 t2] inserts t2 into the dom before t1 *)
val insert_before : t -> t -> unit

val empty : t -> unit

val width : t -> int

val height : t -> int

(* Returns (left, top) *)
val offset : t -> (int * int)

val set_attr : t -> name:string -> value:string -> unit

val sink_attr : t -> name:string -> value:string Frp.Behavior.t -> Frp.Subscription.t

val css : t -> (string * string) array -> unit

module Event : sig
  type removal_token

  val on : t -> string -> (Dom_html.event Js.t -> unit) -> removal_token

  val off : t -> removal_token -> unit
end
