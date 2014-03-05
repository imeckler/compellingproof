open Core

type ('a, 'b) node =
  { id    : int
  ; value : 'a
  ; succs : (int * 'b) array
  }

module Node = struct
  type ('a, 'b) t = int
end

type ('a, 'b) t =
  { nodes        : ('a, 'b) node Inttbl.t
  ; available_id : int
  }

(* For internal purposes *)
let create () = { nodes = Inttbl.create (); available_id = 0 }
let copy t = { nodes = Inttbl.copy t.nodes; available_id = t.available_id }

let empty = create ()

let add_nodes arr t =
  let nodes' = Inttbl.copy t.nodes in
  let len    = Array.length arr in
  let i_0    = t.available_id in
  let rec loop i =
    if i >= len
    then ()
    else begin
      Inttbl.add nodes' ~key:(i_0 + i) ~data:(arr.(i));
      loop (i + 1)
    end
  in
  loop 0;
  ( Array.init len ~f:(fun i -> i_0 + i)
  , { nodes = nodes'; available_id = i_0 + len }
  )

let get v t = Option.map ~f:(fun x -> x.value) (Inttbl.find t.nodes v)

(* For internal purposes *)
let get_node v t = Inttbl.find t.nodes v

let successors v t = Option.map ~f:(fun x -> x.succs) (Inttbl.find t.nodes v)

let successors_exn v t = match Inttbl.find t.nodes v with
  | None   -> failwith "Graph.successors_exn: Node not in graph"
  | Some x -> x.succs

let predecessors v t =
  match Inttbl.find t.nodes v with
  | None -> None
  | Some _ -> 
    let q = Queue.create () in
    Inttbl.iter t.nodes ~f:(fun ~key ~data ->
      match Array.find (data.succs) ~f:(fun (v', _) -> v' = v) with
      | None -> ()
      | Some (_, x) -> Queue.enqueue q (data.id, x)
    );
    Some (Queue.to_array q)

let predecessors_exn v t = match predecessors v t with
  | None   -> failwith "Graph.predecessors_exn: Node not in graph"
  | Some x -> x

let nodes t = Inttbl.keys t.nodes

module Change = struct
  type ('a, 'b) t =
    | Add_arc    of ('a, 'b) Node.t * ('a, 'b) Node.t * 'b
    | Remove_arc of ('a, 'b) Node.t * ('a, 'b) Node.t
end

let unsafe_push (x : 'a) (arr : 'a array) = 
  ignore ((Obj.magic arr : 'a Js.js_array Js.t)##push(x))

let change cs t =
  let nodes' = Inttbl.copy t.nodes in
  Array.iter cs ~f:(let open Change in function
    | Add_arc (u, v, e) ->
      begin match Inttbl.find nodes' u, Inttbl.find nodes' v with
      | Some un, Some _ -> unsafe_push (v, e) un.succs
      | _, _            -> failwith "Graph.Add_arc: Nodes not in graph"
      end
    | Remove_arc (u, v) ->
      begin match Inttbl.find nodes' u, Inttbl.find nodes' v with
      | 
  )

