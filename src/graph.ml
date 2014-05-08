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
  ; mutable available_id : int 
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

let change cs t =
  let nodes' = copy_nodes t.nodes in
  Array.iter cs ~f:(let open Change in function
    | Add_arc (u, v, e) -> add_arc u v e nodes'
    | Remove_arc (u, v) -> remove_arc u v nodes'
    | Remove_node u -> remove_node u nodes'
  );
  { nodes = nodes'; available_id = t.available_id }


type ('a, 'b) gph = ('a, 'b) t

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

  let add_node_mutate x t =
    let new_node = {value = x; succs = Inttbl.create () } in
    let key = t.available_id in
    Inttbl.add t.nodes ~key ~data:new_node;
    t.available_id <- t.available_id + 1;
    key

  let run g =
    let g' = copy g in
    let rec go = let open F in function
      | Pure x                      -> ()
      | Free (Add_arc (u, v, e, k)) -> add_arc u v e g'.nodes; go k
      | Free (Remove_arc (u, v, k)) -> remove_arc u v g'.nodes; go k
      | Free (New_node (x, cont))   -> go (cont (add_node_mutate x g'))
    in
    fun fx -> go fx; g'

  let new_node x = Free (F.New_node (x, fun v -> Pure v))

  let add_arc u v e = Free (F.Add_arc (u, v, e, Pure ()))

  let remove_arc u v = Free (F.Remove_arc (u, v, Pure ()))
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
    polygon ~props:[|Frp.Behavior.return (Property.fill Color.black)|] 
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
    let get_pos v = Frp.Behavior.peek (fst (fst (get_exn v data_g))) in

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
          spring_accels pos1 (successors_exn v1 data_g);
          spring_accels pos1 (predecessors_exn v1 data_g);
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
          let pos1 = fst (fst (get_exn v1 data_g)) in
          let pos2 = fst (fst (get_exn v2 data_g)) in
          path 
            ~props:[| Frp.Behavior.return (Property.stroke Color.black 2) |]
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


