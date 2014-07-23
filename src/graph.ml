open Core

type ('a, 'b) node =
  { value : 'a
  ; succs : 'b Inttbl.t
  }

module Node = struct
  type ('a, 'b) t = int
end

type ('a, 'b) t =
  { nodes                : ('a, 'b) node Inttbl.t
  ; mutable available_id : int
  }

(* For internal purposes *)
let create () = { nodes = Inttbl.create (); available_id = 0 }

let copy_nodes nodes =
  let nodes' = Inttbl.create () in
  Inttbl.iter nodes ~f:(fun ~key ~data -> 
    Inttbl.add nodes' ~key ~data:({data with succs = Inttbl.copy data.succs}) 
  );
  nodes'

let copy t = { nodes = copy_nodes t.nodes; available_id = t.available_id }

let empty = create

let add_node t x =
  let nodes' = copy_nodes t.nodes in
  let new_node = {value = x; succs = Inttbl.create ()} in
  Inttbl.add nodes' ~key:t.available_id ~data:new_node;
  (t.available_id, { nodes = nodes'; available_id = t.available_id + 1})

let add_nodes t arr =
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

let get t v = Option.map ~f:(fun x -> x.value) (Inttbl.find t.nodes v)

let get_exn t v = (Inttbl.find_exn t.nodes v).value

(* For internal purposes *)
let get_node t v = Inttbl.find t.nodes v

let successors t v = Option.map ~f:(fun x -> Inttbl.to_array x.succs) (Inttbl.find t.nodes v)

let successors_exn t v = match Inttbl.find t.nodes v with
  | None   -> failwith "Graph.successors_exn: Node not in graph"
  | Some x -> Inttbl.to_array x.succs

let unsafe_push (x : 'a) (arr : 'a array) = 
  ignore ((Obj.magic arr : 'a Js.js_array Js.t)##push(x))

let predecessors t v =
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

let predecessors_exn t v = match predecessors t v with
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

let fold_arcs t ~init ~f =
  Inttbl.fold t.nodes ~init ~f:(fun acc ~key:v1 ~data:n1 ->
    Inttbl.fold n1.succs ~init:acc ~f:(fun a ~key:v2 ~data ->
      f a v1 v2 data
    )
  )

let iter_nodes t ~f =
  Inttbl.iter t.nodes ~f:(fun ~key ~data -> f key data.value)

let iter_arcs t ~f =
  Inttbl.iter t.nodes ~f:(fun ~key:v1 ~data:n1 ->
    Inttbl.iter n1.succs ~f:(fun ~key:v2 ~data ->
      f v1 v2 data
    )
  )

module Change = struct
  type ('a, 'b) t =
    | Add_arc     of ('a, 'b) Node.t * ('a, 'b) Node.t * 'b
    | Remove_arc  of ('a, 'b) Node.t * ('a, 'b) Node.t
    | Remove_node of ('a, 'b) Node.t
end

let add_arc u v e nodes =
  match Inttbl.find nodes u, Inttbl.find nodes v with
  | Some un, Some _ -> Inttbl.add ~key:v ~data:e un.succs
  | _, _            -> failwith "Graph.add_arc: Nodes not in graph"

let remove_arc u v nodes =
  match Inttbl.find nodes u, Inttbl.find nodes v with
  | Some un, Some _ -> Inttbl.remove un.succs v
  | _, _            -> failwith "Graph.remove_arc: Nodes not in graph"

let remove_node u nodes =
  match Inttbl.find nodes u with
  | Some un -> begin
      Inttbl.remove nodes u;
      Inttbl.iter nodes ~f:(fun ~key ~data -> Inttbl.remove data.succs u)
    end
  | _       -> failwith "Graph.remove_node: Node not in graph"

let change t cs =
  let nodes' = copy_nodes t.nodes in
  Array.iter cs ~f:(let open Change in function
    | Add_arc (u, v, e) -> add_arc u v e nodes'
    | Remove_arc (u, v) -> remove_arc u v nodes'
    | Remove_node u -> remove_node u nodes'
  );
  { nodes = nodes'; available_id = t.available_id }

type ('a, 'b) graph = ('a, 'b) t

let add_node_mutate t x =
  let new_node = {value = x; succs = Inttbl.create () } in
  let key = t.available_id in
  Inttbl.add t.nodes ~key ~data:new_node;
  t.available_id <- t.available_id + 1;
  key

module Builder = struct
  type ('a, 'b) node = ('a, 'b) Node.t

  module F = struct
    type ('k, 'a, 'b) t =
      | Add_arc    of ('a, 'b) Node.t * ('a, 'b) Node.t * 'b * 'k
      | Remove_arc of ('a, 'b) Node.t * ('a, 'b) Node.t * 'k
      | New_node   of 'a * (('a, 'b) Node.t -> 'k)

    let map t ~f = match t with
      | Add_arc (u, v, e, k) -> Add_arc (u, v, e, f k)
      | Remove_arc (u, v, k) -> Remove_arc (u, v, f k)
      | New_node (x, cont) -> New_node (x, (fun v -> f (cont v)))
  end

  include Monad.Free3(F)

  let rec replicate n t =
    if      n < 0 then failwith "Graph.Builder.replicate: n < 0"
    else if n = 0 then return []
    else t >>= fun x -> replicate (n - 1) t >>= fun xs -> return (x :: xs)

  let rec replicate_ignore n t =
    if      n < 0 then failwith "Graph.Builder.replicate_ignore: n < 0"
    else if n = 0 then return ()
    else t >>= fun _ -> replicate_ignore (n - 1) t

  let run g =
    let g' = copy g in
    let rec go = let open F in function
      | Pure x                      -> ()
      | Free (Add_arc (u, v, e, k)) -> add_arc u v e g'.nodes; go k
      | Free (Remove_arc (u, v, k)) -> remove_arc u v g'.nodes; go k
      | Free (New_node (x, cont))   -> go (cont (add_node_mutate g' x))
    in
    fun fx -> go fx; g'

  let new_node x = Free (F.New_node (x, fun v -> Pure v))

  let add_arc u v e = Free (F.Add_arc (u, v, e, Pure ()))

  let remove_arc u v = Free (F.Remove_arc (u, v, Pure ()))
end

let of_adjacency_lists adjs = let open Builder in let open Monad_infix in
  begin
    all (List.map ~f:(fun (x, _) -> new_node x) adjs) >>= fun nodes ->
      let nodes_arr = Array.of_list nodes in
      List.map2_exn nodes adjs ~f:(fun v (_, arcs) ->
        List.map arcs ~f:(fun (u_i, edge_value) ->
          add_arc v nodes_arr.(u_i) edge_value)
        |> all)
      |> all
  end |> run (empty ())

module Mutable = struct
  type ('a, 'b) t = ('a, 'b) graph

  let create             = create
  let of_adjacency_lists = of_adjacency_lists
  let length             = length
  let nodes              = nodes
  let get                = get
  let get_exn            = get_exn
  let successors         = successors
  let successors_exn     = successors_exn
  let predecessors       = predecessors
  let predecessors_exn   = predecessors_exn
  let map_nodes          = map_nodes
  let map_arcs           = map_arcs
  let fold_nodes         = fold_nodes
  let fold_arcs          = fold_arcs
  let iter_nodes         = iter_nodes
  let iter_arcs          = iter_arcs

  let add_node               = add_node_mutate
  let add_nodes t xs         = Array.map ~f:(fun x -> add_node t x) xs
  let add_arc t ~src ~dst e  = add_arc src dst e t.nodes
  let remove_arc t ~src ~dst = remove_arc src dst t.nodes
  let remove_node t node     = remove_node node t.nodes

  let freeze = copy
end

let () = Random.self_init ()

module Mk_draw (M : sig
  val charge_constant : float

  val spring_constant : float

  val width : int

  val height : int
end) = struct
  let charge_accel p1 p2 =
    let d = Vector.sub p2 p1 in
    let c = (Vector.norm d ** 2.) in
    if c = 0.
    then (100., 100.)
    else Vector.scale (-. M.charge_constant /. c) d

  let spring_accel p1 p2 : (float * float) =
    Vector.scale M.spring_constant (Vector.sub p2 p1)

  let side_force_constant = 0.00001

  let side_force_x h p (a, b) =
    side_force_constant *. (atan (b /. (a -. p)) -. atan ((b -. h) /. (a -. p)))

  let side_force_y h p (a, b) =
    let f y = log ((a -. p) ** 2. +. (b -. y) ** 2.) in
    -. (side_force_constant /. 2.) *. (f h -. f 0.)

  let side_force h p pt = (side_force_x h p pt, side_force_y h p pt)

  let horiz_force_x w p (a, b) =
    let f x = log ((a -. x) ** 2. +. (b -. p) ** 2.) in
    (side_force_constant /. 2.) *. (f 0. -. f w)

  let horiz_force_y w p (a, b) =
    side_force_constant *. (atan (a /. (b -. p)) -. atan ((a -. w) /. (b -. p)))

  let horiz_force w p pt = (horiz_force_x w p pt, horiz_force_y w p pt)

  let friction = Vector.scale (-0.001)

  let clip_pos w h (x, y) = (min (w -. 5.) (max x 5.), min (h -. 5.) (max y 5.))

  (* TODO: Find out why orthogonals was wrong
  * let v2 = Vector.(add o1 (add v (scale 20. diff))) in *)
  let edge_triangle pos1 pos2 = let open Draw in
    polygon ~fill:(Frp.Behavior.return Color.black)
      (Frp.Behavior.zip_with pos1 pos2 ~f:(fun u v ->
        let diff = Vector.(normed (sub u v)) in
        let v1 = Vector.(add v (scale 10. diff)) in
        let v2 = Angle.(rotate Vector.(add v (scale 20. diff)) ~about:v1 (of_degrees 30.)) in
        let v3 = Angle.(rotate Vector.(add v (scale 20. diff)) ~about:v1 (of_degrees (-30.))) in
        [| v1; v2; v3 |]
      ))

  let draw container g =
    let w', h'     = float_of_int M.width, float_of_int M.height in
    let data_g     = map_nodes g ~f:(fun _ -> 
      Frp.Behavior.(return (Random.float w', Random.float h'), return (0., 0.))) in
    let data_g = map_nodes g ~f:(fun _ ->
      Arrow.first (Frp.latest ~init:(Random.float w', Random.float h')) (Frp.Stream.create' ()),
      Arrow.first (Frp.latest ~init:(0.,0.)) (Frp.Stream.create' ())
    ) in
    let get_pos v = Frp.Behavior.peek (fst (fst (get_exn data_g v))) in

    let spring_accels p0 vs =
      Array.fold vs ~init:(0., 0.) ~f:(fun acc (v2, _) -> 
        Vector.add acc (spring_accel p0 (get_pos v2))
      )
    in

    (* TODO: change this to be a fold over the delta stream *)
    let update delta =
      let delta = Time.Span.to_ms delta in
      iter_nodes data_g ~f:(fun v1 ((posb1, pos_trigger), (velb1, vel_trigger)) ->
        let pos1 = Frp.Behavior.peek posb1 in
        let charge_acc =
          fold_nodes data_g ~init:(0., 0.) ~f:(fun acc v2 ((posb2, _), _) ->
            if v1 <> v2
            then Vector.add acc (charge_accel pos1 (Frp.Behavior.peek posb2))
            else acc
          )
        in
        let forces = [|
          charge_acc;
          spring_accels pos1 (successors_exn data_g v1);
          spring_accels pos1 (predecessors_exn data_g v1);
          (side_force_x h' 0. pos1, 0.);
          (side_force_x h' w' pos1, 0.);
          (0. , horiz_force_y w' 0. pos1);
          (0., horiz_force_y w' h' pos1);
          friction (Frp.Behavior.peek velb1)
        |]
        in
        let force = Array.fold ~init:(0., 0.) ~f:Vector.add forces in
        let open Frp.Behavior in
        let curr_vel = peek velb1 in
        let pos' = Vector.add (peek posb1) (Vector.scale delta curr_vel) |> clip_pos w' h' in 

        vel_trigger (Vector.add curr_vel (Vector.scale delta force));
        pos_trigger pos';
      )
    in

    let drawing = let open Draw in
      let circs = 
        fold_nodes data_g ~init:[] ~f:(fun cs v ((pos_b, _), _) ->
          circle (Frp.Behavior.return 10.) pos_b
          :: cs
        ) |> Array.of_list
      in
      let edges =
        fold_arcs data_g ~init:[] ~f:(fun es v1 v2 _ ->
          let pos1 = fst (fst (get_exn data_g v1)) in
          let pos2 = fst (fst (get_exn data_g v2)) in
          path ~stroke:(Frp.Behavior.return (Stroke.create Color.black 2))
            ~anchor:pos1
            (Frp.Behavior.map pos2 ~f:(fun p -> [|Segment.line_to p|]))
          :: edge_triangle pos1 pos2
          :: es
        ) |> Array.of_list
      in
      pictures (Array.append circs edges)
    in
    begin
      let (elt, sub) = Draw.render drawing in
      let svg        = Jq.Dom.svg_node "svg" [| 
        "width", string_of_int M.width; "height", string_of_int M.height |] in
      Jq.Dom.append svg elt;

      begin match Jq.to_dom_node container with
        | None   -> print "Graph.draw: Empty Jq.t"
        | Some t -> Jq.Dom.append t svg
      end;

      Frp.Stream.(iter (deltas 30.) ~f:update) |> ignore;
    end
end

let draw ?(charge_constant=0.01) ?(spring_constant=0.000001) ~width ~height =
  let module M = Mk_draw(struct
    let charge_constant = charge_constant
    let spring_constant = spring_constant
    let width           = width
    let height          = height
  end)
  in M.draw

module Sigma = struct
  module Node = struct
    type ('a, 'b) t = Js.js_string Js.t
  end

  module Arc = struct
    class type ['a, 'b] arc = object
      method data   : 'b Js.readonly_prop
      method source : ('a, 'b) Node.t Js.readonly_prop
      method target : ('a, 'b) Node.t Js.readonly_prop
    end

    type ('a, 'b) t = ('a, 'b) arc Js.t

    let data t   = t##data
    let source t = t##source
    let target t = t##target
  end

  type dummy
  type graph

  module Controller : sig
    type t
    val create : unit -> t
    val get_graph : t -> graph Js.t
  end = struct
    type t = dummy Js.t
    let create () : t =
      jsnew (Js.Unsafe.variable "sigma") ()

    let get_graph (t : t) = Js.Unsafe.get t (Js.string "graph")
  end

  type ('a, 'b) t =
    { sigma       : Controller.t
    ; dummy       : ('a * 'b) ref option
    ; mutable uid : int
    }

  let get_uid t =
    let id = t.uid in
    t.uid <- t.uid + 1;
    (Obj.magic id : Js.number Js.t)##toString()

  let create () : ('a, 'b) t =
    { sigma = Controller.create ()
    ; uid   = 0
    ; dummy = None
    }

  let get_graph t = Controller.get_graph t.sigma

  type ('a, 'b) node_obj
  let node_id (node_obj : ('a, 'b) node_obj) : ('a, 'b) Node.t =
    Js.Unsafe.get node_obj (Js.string "id")

  let node_data (node_obj : ('a, 'b) node_obj) : 'a =
    Js.Unsafe.get node_obj (Js.string "data")

  let node_objs (t : ('a, 'b) t) : ('a, 'b) node_obj array =
    Js.to_array (Js.Unsafe.meth_call (get_graph t) "nodes" [||])

  let nodes (t : ('a, 'b) t) : ('a, 'b) Node.t array = Array.map ~f:node_id (node_objs t)

  let iter_nodes t ~f = Array.iter (node_objs t) ~f:(fun node ->
    f (node_id node) (node_data node))

  let arcs t = Js.to_array (Js.Unsafe.meth_call (get_graph t) "arcs" [||])

  let opt_to_arr = function
    | None -> [||]
    | Some x -> [|x|]

  let opt_prop name conv xo = match xo with
    | None -> [||]
    | Some x -> [|name, Js.Unsafe.inject (conv x)|]

  let add_node ?color ?pos ?size ?label t x =
    let id = get_uid t in
    Js.Unsafe.(meth_call (get_graph t) "addNode" [|
      obj (
        [|("id", inject id); ("data", inject x)|]
        @@ opt_prop "color" (Js.string -| Color.to_css_string) color
        @@ opt_prop "x" (Js.number_of_float -| fst) pos
        @@ opt_prop "y" (Js.number_of_float -| snd) pos
        @@ opt_prop "size" ident size
        @@ opt_prop "label" Js.string label)|]);
    id

  let find_node t node =
    Js.Optdef.to_option (
      Js.Unsafe.(meth_call (get_graph t) "nodes" [|inject node|]))

  let get t node = Option.map (find_node t node) ~f:(fun node_obj ->
    Js.Unsafe.get node_obj (Js.string "data"))

  let get_exn t node = match get t node with
    | Some x -> x | None -> failwith "Graph.Sigma.get_exn: Node not found"

  type 'a update = [`Color of Color.t | `Label of string | `Size of float | `Data of 'a]
  let update_node t node update =
    match find_node t node with
    | None -> `Not_found
    | Some node_obj ->
      let prop_name, value = Js.Unsafe.(match update with
        | `Color c -> "color", inject (Js.string (Color.to_css_string c))
        | `Label s -> "label", inject (Js.string s)
        | `Size f  -> "size", inject f
        | `Data x  -> "data", inject x)
      in
      Js.Unsafe.set node_obj (Js.string prop_name) value;
      `Ok
  ;;

  let update_node_exn t node update =
    match update_node t node update with
    | `Ok -> () | `Not_found -> failwith "Graph.Sigma.update_node_exn: Not found"

  let add_arc ?color ?size t ~src ~dst data =
    let id = get_uid t in
    try Js.Unsafe.(meth_call (get_graph t) "addEdge" [|
      obj (
        [|("id", inject id); ("source", inject src); ("target", inject dst)|]
        @@ opt_prop "color" (Js.string -| Color.to_css_string) color
        @@ opt_prop "size" ident size)|]);
      `Ok
    with _e -> `Not_found
  ;;

  let add_arc_exn ?color ?size t ~src ~dst data =
    match add_arc ?color ?size t ~src ~dst data with
    | `Ok -> () | `Not_found -> failwith "Graph.Sigma.add_arc_exn: Not found"

  let remove_node t node =
    try (Js.Unsafe.(meth_call (get_graph t) "dropNode" [|inject node|]); `Ok)
    with _e -> `Not_found

  let remove_node_exn t node = match remove_node t node with
    | `Ok -> () | `Not_found -> failwith "Graph.Sigma.remove_node_exn: Not found"

  let clear t =
    Js.Unsafe.meth_call (get_graph t) "clear" [||]

  let refresh t =
    Js.Unsafe.meth_call t.sigma "refresh" [||]

  let start_force_layout t =
    Js.Unsafe.meth_call t.sigma "startForceAtlas2" [||]

  let stop_force_layout t =
    Js.Unsafe.meth_call t.sigma "stopForceAtlas2" [||]

  let display ?(mode=`Canvas) t container =
    let mode_str = Js.string (match mode with
      | `Canvas -> "canvas" | `WebGL -> "webgl")
    in
    Js.Unsafe.(meth_call t.sigma "addRenderer" [| 
      obj [|
        "container", inject container;
        "type", inject mode_str
      |]|]);
    refresh t
  ;;
end

