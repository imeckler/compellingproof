open Core

module Point = struct
(*   type 'a t = { x : 'a ; y : 'a } *)

  type 'a t = 'a * 'a

  let render (x, y) = string_of_float x ^ "," ^ string_of_float y

  let render_many pts = String.concat_array ~sep:" " (Array.map pts ~f:render)
end

module Color = struct
  type t = { r : int; g : int; b : int; alpha : float }

  let of_rgb ?(alpha=1.0) ~r ~g ~b () = {r; g; b; alpha}

  let render {r; g; b; alpha} =
    Printf.sprintf "rgba(%d,%d,%d,%f)" r g b alpha

  let white = { r = 255; g = 255; b = 255; alpha = 1.0 }
  let black = { r = 0; g = 0; b = 0; alpha = 1.0 }
  let red   = { r = 255; g = 0; b = 0; alpha = 1.0 }
  let blue  = { r = 0; g = 0; b = 255; alpha = 1.0 }
  let none  = { blue with alpha = 0. }
end

module Angle = struct
  type t = float (* Internally we use degrees *)

  let pi = 4. *. atan 1.

  let to_degrees x = x

  let to_radians = let c = (2. *. pi) /. 360. in fun x -> c *. x

  let of_degrees x = x

  let of_radians = let c = 360. /. (2. *. pi) in fun x -> c *. x

  let rotate ~about:(a, b) (x, y) angle =
    let angle = to_radians angle in
    let x', y' = x -. a, y -. b in
    let x'' = (x' *. cos angle) -. (y' *. sin angle) in
    let y'' = (x' *. sin angle) +. (y' *. cos angle) in
    (x'' +. a, y'' +. b)

  let cos x = cos (to_radians x)

  let sin x = sin (to_radians x)

  let (+) = (+.)
  let (-) = (-.)
  let ( * ) = ( *. )
end

module Transform = struct
  type t =
    | Matrix    of float * float * float * float * float * float
    | Translate of float * float
    | Scale     of float * float
    | Rotate    of Angle.t * float Point.t
    | Skew_x    of float
    | Skew_y    of float

  let render = function
    | Matrix (a, b, c, d, e, f) ->
        Printf.sprintf "matrix(%f,%f,%f,%f,%f,%f)" a b c d e f
    | Translate (x, y)   -> Printf.sprintf "translate(%f %f)" x y
    | Scale (x, y)       -> Printf.sprintf "scale(%f %f)" x y
    | Rotate (a, (x, y)) -> Printf.sprintf "rotate(%f %f %f)" a x y
    | Skew_x s           -> Printf.sprintf "skewX(%f)" s
    | Skew_y s           -> Printf.sprintf "skewY(%f)" s
end

module Property = struct
  (* TODO: This whole system needs to be redone. Decide if all properties should
   * be together in one record.
   *)

  module Stroke = struct
    module Linecap = struct
      type t = [ `Butt | `Square | `Round ]

      let render = function
        | `Butt   -> "butt"
        | `Square -> "square"
        | `Round  -> "round"
    end

    module Linejoin = struct
      type t = [ `Miter | `Round | `Bevel ]

      let render = function
        | `Miter -> "miter"
        | `Round -> "round"
        | `Bevel -> "bevel"
    end

    type t =
      { cap         : Linecap.t
      ; join        : Linejoin.t
(*       ; mask        : (float * float) option *)
      ; width       : int
      ; color       : Color.t
      }

  end

  type t =
    | Fill   of Color.t
    | Stroke of Stroke.t
    | Dash_array of float array
    (* TODO: This is a hack for before the thing gets rewritten *)
    | Any of string * string

  let fill c = Fill c

  let any ~name ~value = Any (name, value)

  let stroke ?(cap=`Butt) ?(join=`Miter) color width =
    Stroke {Stroke.cap; join; color; width}

  (* TODO: Fix the masking so that a mask can be applied to a dashed path *)

  let render = 
    function
    | Fill c -> "fill:" ^ Color.render c
    | Stroke {Stroke.cap; join; width; color} ->
      String.concat_array ~sep:";"
      [| "stroke:"          ^ Color.render color
       ; "stroke-width:"    ^ string_of_int width
       ; "stroke-linecap:"  ^ Stroke.Linecap.render cap
       ; "stroke-linejoin:" ^ Stroke.Linejoin.render join
      |]
    | Dash_array xs ->
      "stroke-dasharray:" 
      ^ String.concat_array ~sep:" " (Array.map ~f:string_of_float xs)
    | Any (name, value) -> Printf.sprintf "%s:%s" name value
end

module Segment = struct
  type t =
    | Line_to of float Point.t
    | Move_to of float Point.t
    | Arc     of Angle.t * Angle.t * [`long | `short] * float

  let line_to x = Line_to x

  let move_to x = Move_to x

  let arc a1 a2 l r = Arc (a1, a2, l, r)

  let render = function
    | Line_to (x, y) -> Printf.sprintf "l%f %f" x y
    | Move_to (x, y) -> Printf.sprintf "m%f %f" x y
    | Arc (a1, a2, l, r) -> let open Point in
        let flag = match l with `long -> 1 | `short -> 0 in
        let ctr = (-. Angle.cos a1 *. r, Angle.sin a1 *. r) in
        let (x, y) = Angle.(rotate ~about:ctr (0., 0.) (a2 -. a1)) in
        Printf.sprintf "a%f,%f 0 %d,1 %f,%f" r r flag x y

  let render_many ts = String.concat_array ~sep:" " (Array.map ~f:render ts)
end

type t =
  | Circle    of Property.t Frp.Behavior.t array 
               * float Frp.Behavior.t 
               * float Point.t Frp.Behavior.t

  | Transform of t * Transform.t Frp.Behavior.t

  | Polygon   of Property.t Frp.Behavior.t array 
               * float Point.t array Frp.Behavior.t

  | Path      of Property.t Frp.Behavior.t array
               * float Point.t Frp.Behavior.t
               * (float * float) Frp.Behavior.t option
               * Segment.t array Frp.Behavior.t

  (* TODO: This is a hack to use until I figure out a better interface *)
  | Path_str  of Property.t Frp.Behavior.t array
               * (float * float) Frp.Behavior.t option
               * string Frp.Behavior.t

  | Text      of Property.t Frp.Behavior.t array 
               * float Point.t Frp.Behavior.t 
               * string Frp.Behavior.t

  | Pictures  of t array

  | Rect      of Property.t Frp.Behavior.t array 
               * float Point.t Frp.Behavior.t 
               * float Frp.Behavior.t
               * float Frp.Behavior.t

  | Dynamic   of t Frp.Behavior.t

  | Svg       of Jq.Dom.t

let circle ?(props=[||]) r center = Circle (props, r, center)

let rect ?(props=[||]) ~width ~height corner = Rect (props, corner, width, height) 

let polygon ?(props=[||]) pts = Polygon (props, pts)

let path ?(props=[||]) ?mask ~anchor segs = Path (props, anchor, mask, segs)

let path_string ?(props=[||]) ?mask strb = Path_str (props, mask, strb)

let text ?(props=[||]) str corner = Text (props, corner, str)

let svg e = Svg e

let transform t trans = Transform (t, trans)

let pictures ts = Pictures ts

let dynamic tb = Dynamic tb

let render_properties ps = String.concat_array ~sep:";" (Array.map ps ~f:Property.render)

let sink_attrs elt ps =
  Array.map ~f:(fun (name, value) -> Jq.Dom.sink_attr elt ~name ~value) ps
  |> Frp.Subscription.concat

let rec render =
  let x_beh = Frp.Behavior.map ~f:(fun (x, _) -> string_of_float x) in
  let y_beh = Frp.Behavior.map ~f:(fun (_, y) -> string_of_float y) in
  let zip_props ps_b = Frp.Behavior.zip_many ps_b ~f:render_properties in

  let path_mask elt segs mask props =
    let get_length () : float = Js.Unsafe.meth_call elt "getTotalLength" [||] in
    let path_length =
      Frp.Stream.map (Frp.Behavior.changes segs) ~f:(fun _ -> get_length ())
      |> Frp.scan ~init:(get_length ()) ~f:(fun _ x -> x)
    in
    let props' = match mask with
      | None -> props
      | Some mask -> begin
        let dash_array = Frp.Behavior.zip_with path_length mask ~f:(fun l (a, b) ->
          Property.Dash_array [|0. ; l *. a; l *. (b -. a); l |]
        )
        in
        Array.append props [|dash_array|]
      end
    in
    Jq.Dom.sink_attr elt ~name:"style" ~value:(zip_props props')
  in

  let open Frp in function
  
  | Svg elt -> (elt, Frp.Subscription.empty)

  | Text (ps, corner, text) ->
    let elt = Jq.Dom.svg_node "text" [||] in
    let sub = Jq.Dom.sink_html elt text in
    (elt, sink_attrs elt [|
      "x", x_beh corner;
      "y", y_beh corner;
      "style", zip_props ps
    |] |> Frp.Subscription.merge sub)

  | Circle (ps, r, center) -> 
    let elt = Jq.Dom.svg_node "circle" [||] in
    (elt, sink_attrs elt [|
      "cx", x_beh center;
      "cy", y_beh center;
      "r", Frp.Behavior.map ~f:string_of_float r;
      "style", zip_props ps
    |])

  | Transform (t, trans) ->
    let (elt, sub) = render t in
    let trans_sub  = Jq.Dom.sink_attr elt
      ~name: "transform"
      ~value:(Frp.Behavior.map ~f:Transform.render trans)
    in
    (elt, Frp.Subscription.merge trans_sub sub)

  | Path_str (props, mask, path_strb) ->
    let elt = Jq.Dom.svg_node "path" [||] in
    let sub = Jq.Dom.sink_attr elt ~name:"d" ~value:path_strb in
    (elt, Frp.Subscription.merge sub (path_mask elt path_strb mask props))

  | Path (props, anchor, mask, segs) ->
    let elt = Jq.Dom.svg_node "path" [||] in
    let sub = Jq.Dom.sink_attr elt
      ~name:"d"
      ~value:(Frp.Behavior.zip_with anchor segs ~f:(fun (x, y) sgs -> 
        Printf.sprintf "M%f,%f %s" x y (Segment.render_many sgs)
      ))
    in
    (elt, Frp.Subscription.merge sub (path_mask elt segs mask props))

  | Polygon (props, pts) ->
      let open Frp.Behavior in
      let elt = Jq.Dom.svg_node "polygon" [|
        "style" , render_properties (Array.map ~f:peek props);
        "points", String.concat_array ~sep:"," (Array.map ~f:Point.render (peek pts))
      |]
      in
      let sub = Jq.Dom.sink_attr elt
        ~name:"points"
        ~value:(Frp.Behavior.map ~f:Point.render_many pts)
      in
      (elt, sub)

  | Pictures pics ->
    let elts = Array.map ~f:render pics in
    let elt = Jq.Dom.svg_node "g" [||] in 
    Array.iter ~f:(fun (e, _) -> Jq.Dom.append elt e) elts;
    (elt, Frp.Subscription.concat (Array.map ~f:snd elts))

    (* TODO: Add properties. *)
  | Rect (ps, corner, wb, hb) -> let open Frp.Behavior in
    let (x, y) = peek corner in
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
    Jq.Dom.append container elt;
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

