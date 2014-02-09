open Core

module Point = struct
  type t = { x : float ; y : float }
end

module Color = struct
  type t = { r : int; g : int; b : int; alpha : float }

  let of_rgb ?(alpha=0.0) ~r ~g ~b = {r; g; b; alpha}

  let render {r; g; b; alpha} =
    Printf.sprintf "rgba(%d,%d,%d,%f)" r g b alpha

  let white = { r = 255; g = 255; b = 255; alpha = 1.0 }
  let black = { r = 0; g = 0; b = 0; alpha = 1.0 }
  let red = { r = 255; g = 0; b = 0; alpha = 1.0 }
  let blue = { r = 0; g = 0; b = 255; alpha = 1.0 }
end

module Transform = struct
  type t =
    | Matrix    of float * float * float * float * float * float
    | Translate of float * float
    | Scale     of float * float
    | Rotate    of float * Point.t
    | Skew_x    of float
    | Skew_y    of float

  let render = function
    | Matrix (a, b, c, d, e, f) ->
        Printf.sprintf "matrix(%f,%f,%f,%f,%f,%f)" a b c d e f
    | Translate (x, y)         -> Printf.sprintf "translate(%f %f)" x y
    | Scale (x, y)             -> Printf.sprintf "scale(%f %f)" x y
    | Rotate (a, {Point.x; y}) -> Printf.sprintf "rotate(%f %f %f)" a x y
    | Skew_x s                 -> Printf.sprintf "skewX(%f)" s
    | Skew_y s                 -> Printf.sprintf "skewY(%f)" s
end

module Property = struct
  (* TODO: Add more properties *)
  type t =
    | Fill of Color.t

  let render = function
    | Fill c -> "fill:" ^ Color.render c
end

type t =
  | Circle    of Property.t Frp.Behavior.t array * float Frp.Behavior.t * Point.t Frp.Behavior.t
  | Transform of t * Transform.t Frp.Behavior.t
  | Polygon   of Property.t Frp.Behavior.t array * Point.t array Frp.Behavior.t
  | Path      of Property.t Frp.Behavior.t array * Point.t array Frp.Behavior.t
(*   | Bezier
  | Beside    of t list
  | Stack     of t list *)
  | Pictures  of t array
  | Rect      of Property.t Frp.Behavior.t array * Point.t Frp.Behavior.t * float Frp.Behavior.t * float Frp.Behavior.t
  | Dynamic   of t Frp.Behavior.t

let circle ?(props=[||]) r center = Circle (props, r, center)

let rect ?(props=[||]) ~width ~height corner = Rect (props, corner, width, height) 

let polygon ?(props=[||]) pts = Polygon (props, pts)

let path ?(props=[||]) pts = Path (props, pts)

let transform t trans = Transform (t, trans)

let pictures ts = Pictures ts

let dynamic tb = Dynamic tb

let render_properties ps = String.concat_array ~sep:"," (Array.map ps ~f:Property.render)

let sink_attrs elt ps =
  Array.map ~f:(fun (name, value) -> Jq.Dom.sink_attr elt ~name ~value) ps
  |> Frp.Subscription.concat

let rec render =
  let x_beh = Frp.Behavior.map ~f:(fun {Point.x;_} -> string_of_float x) in
  let y_beh = Frp.Behavior.map ~f:(fun {Point.y; _} -> string_of_float y) in
  let render_pt {Point.x; y} = string_of_float x ^ "," ^ string_of_float y in
  let render_pts pts = String.concat_array ~sep:" " (Array.map pts ~f:render_pt) in
  let open Frp in function
  | Circle (ps, r, center) -> 
    let {Point. x; y} = Behavior.peek center in
    let elt = Jq.Dom.svg_node "circle" [|
      "cx"   , string_of_float x;
      "cy"   , string_of_float y;
      "r"    , string_of_float (Frp.Behavior.peek r);
      "style", render_properties (Array.map ~f:Frp.Behavior.peek ps)
    |]
    in
    (* TODO: Properties *)
    (elt, sink_attrs elt [|
      "cx", x_beh center;
      "cy", y_beh center;
      "r", Frp.Behavior.map ~f:string_of_float r
    |])

  | Transform (t, trans) ->
    let (elt, sub) = render t in
    let trans_sub  = Jq.Dom.sink_attr elt
      ~name: "transform"
      ~value:(Frp.Behavior.map ~f:Transform.render trans)
    in
    (elt, Frp.Subscription.merge trans_sub sub)

  | Path _ -> failwith "idk"

  | Polygon (props, pts) ->
      let open Frp.Behavior in
      let elt = Jq.Dom.svg_node "polygon" [|
        "style" , render_properties (Array.map ~f:peek props);
        "points", String.concat_array ~sep:"," (Array.map ~f:render_pt (peek pts))
      |]
      in
      let sub = Jq.Dom.sink_attr elt
        ~name:"points"
        ~value:(Frp.Behavior.map ~f:render_pts pts)
      in
      (elt, sub)

  | Pictures pics ->
    let elts = Array.map ~f:render pics in
    let elt = Jq.Dom.svg_node "g" [||] in 
    Array.iter ~f:(fun (e, _) -> Jq.Dom.append elt e) elts;
    (elt, Frp.Subscription.concat (Array.map ~f:snd elts))

  | Rect (ps, corner, wb, hb) -> let open Frp.Behavior in
    let {Point. x; y} = peek corner in
    let elt = Jq.Dom.svg_node "rect" [|
      "x"     , string_of_float x;
      "y"     , string_of_float y;
      "width" , string_of_float (peek wb);
      "height", string_of_float (peek hb);
      "style" , render_properties (Array.map ~f:peek ps)
    |] in
    let subs = sink_attrs elt [|
      "x"     , x_beh corner;
      "y"     , y_beh corner;
      "width" , Frp.Behavior.map ~f:string_of_float wb;
      "height", Frp.Behavior.map ~f:string_of_float hb;
    |]
    in
    (elt, subs)

  | Dynamic tb ->
    let container  = Jq.Dom.svg_node "g" [||] in
    let (elt, sub) = render (Frp.Behavior.peek tb) in
    let last_sub   = ref sub in
    let dyn_sub    = Frp.Stream.iter (Frp.Behavior.changes tb) ~f:(fun t ->
      Frp.Subscription.cancel !last_sub;
      Jq.Dom.empty container;
      let (elt, sub) = render t in
      Jq.Dom.append container elt;
      last_sub := sub;
    )
    in
    (container, Subscription.(merge dyn_sub (make (fun () -> cancel !last_sub))))

