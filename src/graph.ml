open Core

type ('a, 'b) node =
  { value : 'a
  ; succs : 'b Inttbl.t
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
let copy_nodes nodes =
  let nodes' = Inttbl.create () in
  Inttbl.iter nodes ~f:(fun ~key ~data -> 
    Inttbl.add nodes' ~key ~data:({data with succs = Inttbl.copy data.succs}) 
  );
  nodes'

let empty = create

let add_node x t =
  let nodes' = copy_nodes t.nodes in
  let new_node = {value = x; succs = Inttbl.create ()} in
  Inttbl.add nodes' ~key:t.available_id ~data:new_node;
  (t.available_id, { nodes = nodes'; available_id = t.available_id + 1})

let add_nodes arr t =
  let nodes' = copy_nodes t.nodes in
  let len    = Array.length arr in
  let i_0    = t.available_id in
  let rec loop i =
    if i >= len
    then ()
    else begin
      let new_node = {value = arr.(i); succs = Inttbl.create () } in
      Inttbl.add nodes' ~key:(i_0 + i) ~data:new_node;
      loop (i + 1)
    end
  in
  loop 0;
  ( Array.init len ~f:(fun i -> i_0 + i)
  , { nodes = nodes'; available_id = i_0 + len }
  )

let get v t = Option.map ~f:(fun x -> x.value) (Inttbl.find t.nodes v)

let get_exn v t = (Inttbl.find_exn t.nodes v).value

(* For internal purposes *)
let get_node v t = Inttbl.find t.nodes v

let successors v t = Option.map ~f:(fun x -> Inttbl.to_array x.succs) (Inttbl.find t.nodes v)

let successors_exn v t = match Inttbl.find t.nodes v with
  | None   -> failwith "Graph.successors_exn: Node not in graph"
  | Some x -> Inttbl.to_array x.succs

let unsafe_push (x : 'a) (arr : 'a array) = 
  ignore ((Obj.magic arr : 'a Js.js_array Js.t)##push(x))

let predecessors v t =
  match Inttbl.find t.nodes v with
  | None -> None
  | Some _ -> 
    let preds = [||] in
    Inttbl.iter t.nodes ~f:(fun ~key ~data ->
      match Inttbl.find data.succs v with
      | None   -> ()
      | Some x -> unsafe_push (key, x) preds
    );
    Some preds

let predecessors_exn v t = match predecessors v t with
  | None   -> failwith "Graph.predecessors_exn: Node not in graph"
  | Some x -> x

let nodes t = Inttbl.keys t.nodes

let length t = Inttbl.length t.nodes

let map_nodes t ~f =
  let nodes' = Inttbl.create () in
  Inttbl.iter t.nodes ~f:(fun ~key ~data -> 
    Inttbl.add nodes' ~key ~data:({value = f data.value; succs = Inttbl.copy data.succs}) 
  );
  { t with nodes = nodes' }

let map_arcs t ~f =
  let nodes' = Inttbl.create () in
  Inttbl.iter t.nodes ~f:(fun ~key ~data -> 
    let new_node = 
      { value = data.value
      ; succs = Inttbl.map ~f data.succs
      }
    in
    Inttbl.add nodes' ~key ~data:new_node
  );
  { t with nodes = nodes' }

let fold_nodes t ~init ~f =
  Inttbl.fold t.nodes ~init ~f:(fun acc ~key ~data ->
    f acc key data.value
  )

let iter_nodes t ~f =
  Inttbl.iter t.nodes ~f:(fun ~key ~data -> f key data.value)

let iter_arcs t ~f =
  Inttbl.iter t.nodes ~f:(fun ~key:v1 ~data ->
    Inttbl.iter data.succs ~f:(fun ~key:v2 ~data ->
      f v1 v2 data
    )
  )

module Change = struct
  type ('a, 'b) t =
    | Add_arc     of ('a, 'b) Node.t * ('a, 'b) Node.t * 'b
    | Remove_arc  of ('a, 'b) Node.t * ('a, 'b) Node.t
    | Remove_node of ('a, 'b) Node.t
end

let change cs t =
  let nodes' = copy_nodes t.nodes in
  Array.iter cs ~f:(let open Change in function
    | Add_arc (u, v, e) ->
      begin match Inttbl.find nodes' u, Inttbl.find nodes' v with
      | Some un, Some _ -> Inttbl.add ~key:v ~data:e un.succs
      | _, _            -> failwith "Graph.Change.Add_arc: Nodes not in graph"
      end
    | Remove_arc (u, v) ->
      begin match Inttbl.find nodes' u, Inttbl.find nodes' v with
      | Some un, Some _ -> Inttbl.remove un.succs v
      | _, _            -> failwith "Graph.Change.Remove_arc: Nodes not in graph"
      end
    | Remove_node u ->
      begin match Inttbl.find nodes' u with
      | Some un -> begin
          Inttbl.remove nodes' u;
          Inttbl.iter nodes' ~f:(fun ~key ~data -> Inttbl.remove data.succs u)
        end
      | _       -> failwith "Graph.Change.Remove_node : Node not in graph"
      end
  );
  { nodes = nodes'; available_id = t.available_id }

