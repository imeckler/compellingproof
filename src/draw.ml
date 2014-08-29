open Core

module Point = struct
  include Point

  let render (x, y) = string_of_float x ^ "," ^ string_of_float y

  let render_many pts = String.concat_array ~sep:" " (Array.map pts ~f:render)
end

module Transform = struct
  type t =
    | Affine    of Affine.t
    | Translate of float * float
    | Scale     of float * float
    | Rotate    of Angle.t * float Point.t
    | Skew_x    of float
    | Skew_y    of float

  let translate (x, y) = Translate (x, y)

  let scale ?about sx sy = match about with
    | None -> Scale (sx, sy)
    | Some point ->
        Affine (let open Affine in
          compose {identity with translation = point} (
            compose {identity with matrix = (sx, 0., 0., sy)} (
              ({identity with translation = Vector.scale (-1.) point}))))

  let rotate ?(about=(0., 0.)) angle = Rotate (angle, about)
  let skew_x c = Skew_x c
  let skew_y c = Skew_y c

  let render = function
    | Affine {Affine.matrix=(a, b, c, d); translation = (e, f)} ->
        Printf.sprintf "matrix(%f,%f,%f,%f,%f,%f)" a b c d e f
    | Translate (x, y)   -> Printf.sprintf "translate(%f %f)" x y
    | Scale (x, y)       -> Printf.sprintf "scale(%f %f)" x y
    | Rotate (a, (x, y)) -> Printf.sprintf "rotate(%f %f %f)" (Angle.to_degrees a) x y
    | Skew_x s           -> Printf.sprintf "skewX(%f)" s
    | Skew_y s           -> Printf.sprintf "skewY(%f)" s

  let render_many ts =
    String.concat_array ~sep:" " (Array.map ~f:render ts)
end

module Property = struct
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
      ; width       : float
      ; color       : Color.t
      }

    let create ?(cap=`Butt) ?(join=`Miter) color width =
      {cap; join; color; width}
  end

  type t =
    | Fill   of Color.t
    | Stroke of Stroke.t
    | Dash_array of float array
    (* TODO: This is a hack for before the thing gets rewritten *)
    | Any of string * string

  let fill c = Fill c

  let stroke s = Stroke s

  let any ~name ~value = Any (name, value)

  (* TODO: Fix the masking so that a mask can be applied to a dashed path *)

  let render = 
    function
    | Fill c -> "fill:" ^ Color.to_css_string c
    | Stroke {Stroke.cap; join; width; color} ->
      String.concat_array ~sep:";"
      [| "stroke:"          ^ Color.to_css_string color
       ; "stroke-width:"    ^ string_of_float width
       ; "stroke-linecap:"  ^ Stroke.Linecap.render cap
       ; "stroke-linejoin:" ^ Stroke.Linejoin.render join
      |]
    | Dash_array xs ->
      "stroke-dasharray:" 
      ^ String.concat_array ~sep:" " (Array.map ~f:string_of_float xs)
    | Any (name, value) -> Printf.sprintf "%s:%s" name value
end

module Stroke = Property.Stroke

module Segment = struct
  type t =
    | Line_to of float Point.t
    | Move_to of float Point.t
    | Arc_to  of float Point.t * [`long | `short] * [`pos | `neg] * float
    | Arc     of Angle.t * Angle.t * [`long | `short] * float

  (* This should all be absolute. Relative is too confusing. Use transform to anchor if necessary *)
  let line_to x = Line_to x

  let move_to x = Move_to x

  let arc a1 a2 l r = Arc (a1, a2, l, r)

  let arc_to pt l sweep r = Arc_to (pt, l, sweep, r)

  let large_arc_flag_num = function `long -> 1 | `short -> 0

  let sweep_flag_num = function `neg -> 1 | `pos -> 0

  let render = function
    | Line_to (x, y) -> Printf.sprintf "L%f %f" x y
    | Move_to (x, y) -> Printf.sprintf "M%f %f" x y
    | Arc_to ((x, y), l, sweep, r) ->
        Printf.sprintf "A%f,%f 0 %d,%d %f,%f" r r (large_arc_flag_num l) (sweep_flag_num sweep) x y
    | Arc (a1, a2, l, r) -> let open Point in
        let flag = large_arc_flag_num l in
        let ctr = (-. Angle.cos a1 *. r, Angle.sin a1 *. r) in
        let (x, y) = Angle.(rotate ~about:ctr (0., 0.) (a2 - a1)) in
        Printf.sprintf "a%f,%f 0 %d,1 %f,%f" r r flag x y

  let render_many ts = String.concat_array ~sep:" " (Array.map ~f:render ts)
end

module Name = struct
  type t =
    { mutable on_render : Jq.t -> Frp.Subscription.t
    }

  let create () =
    { on_render = fun _ -> Frp.Subscription.empty }

  let extend_on_render t f =
    let curr = t.on_render in
    t.on_render <- (fun elt -> Frp.Subscription.merge (curr elt) (f elt))

  let copy_stream ~from trigger =
    Frp.Stream.iter from ~f:trigger

  (* TODO: This is a mess *)
  let jq_stream mk_stream t =
    let sub = ref Frp.Subscription.empty in
    let s, trigger = Frp.Stream.create' () ~start:(fun _ ->
      fun () -> Frp.Subscription.cancel !sub)
    in
    let sink_events elt = sub := copy_stream ~from:(mk_stream elt) trigger; !sub in
    extend_on_render t sink_events;
    s

  let clicks t = jq_stream Input.Mouse.clicks_on t

  let drags_with t ~button = jq_stream (Input.Mouse.drags_with ~button) t

  let init e t = t.on_render (Jq.wrap e)
end

type shape_config =
  { name : Name.t option
  }

(* TODO: Make a separate constructor for Named *)
type t =
  | Circle    of shape_config
               * Property.t Frp.Behavior.t array 
               * float Frp.Behavior.t 
               * float Point.t Frp.Behavior.t

  | Transform of t * Transform.t array Frp.Behavior.t

  | Polygon   of shape_config
               * Property.t Frp.Behavior.t array 
               * float Point.t array Frp.Behavior.t

  | Path      of shape_config
               * Property.t Frp.Behavior.t array
               * float Point.t Frp.Behavior.t
               * (float * float) Frp.Behavior.t option
               * Segment.t array Frp.Behavior.t

  (* TODO: This is a hack to use until I figure out a better interface *)
  | Path_str  of shape_config
               * Property.t Frp.Behavior.t array
               * (float * float) Frp.Behavior.t option
               * string Frp.Behavior.t

  | Text      of shape_config
               * Property.t Frp.Behavior.t array 
               * float Point.t Frp.Behavior.t 
               * string Frp.Behavior.t

  | Rect      of shape_config
               * Property.t Frp.Behavior.t array 
               * float Point.t Frp.Behavior.t 
               * float Frp.Behavior.t
               * float Frp.Behavior.t

  | Pictures  of t array

  (* TODO: Think about if we should pass x and y attributes here too *)
  | Image     of string Frp.Behavior.t * int Frp.Behavior.t * int Frp.Behavior.t

  | Clip      of t * t (* clippedDrawing * clipPath *)

  | Dynamic   of t Frp.Behavior.t

  | Svg       of Jq.Dom.t

type 'k with_shape_args =
  ?name:Name.t
  -> ?fill:Color.t Frp.Behavior.t
  -> ?stroke:Stroke.t Frp.Behavior.t
  -> ?props:(Property.t Frp.Behavior.t array)
  -> 'k

let add_opt_prop f xo arr = Option.value_map xo ~default:arr ~f:(fun x ->
  Array.append [|Frp.Behavior.map ~f x|] arr)

let add_opt_props fill stroke props =
  add_opt_prop Property.fill fill (add_opt_prop Property.stroke stroke props)

let circle ?name ?fill ?stroke ?(props=[||]) r center =
  Circle ({name}, add_opt_props fill stroke props, r, center)

let rect ?name ?fill ?stroke ?(props=[||]) ~width ~height corner =
  Rect ({name}, add_opt_props fill stroke props, corner, width, height) 

let polygon ?name ?fill ?stroke ?(props=[||]) pts =
  Polygon ({name}, add_opt_props fill stroke props, pts)

let path ?name ?fill ?stroke ?(props=[||]) ?mask ~anchor segs =
  Path ({name}, add_opt_props fill stroke props, anchor, mask, segs)

let path_string ?name ?fill ?stroke ?(props=[||]) ?mask strb =
  Path_str ({name}, add_opt_props fill stroke props, mask, strb)

let text ?name ?fill ?stroke ?(props=[||]) str corner =
  Text ({name}, add_opt_props fill stroke props, corner, str)

let svg e = Svg e

let transform t trans = Transform (t, trans)

let translate t move =
  Transform (t, Frp.Behavior.map move~f:(fun (x,y) -> [|Transform.Translate (x,y)|]))

let image ~width ~height url = Image (url, width, height)

let clip ~by t = Clip (t, by)

let crop ~width ~height corner t = clip t ~by:(rect ~width ~height corner)

let pictures ts = Pictures ts

let empty = pictures [||]

let dynamic tb = Dynamic tb

let render_properties ps = String.concat_array ~sep:";" (Array.map ps ~f:Property.render)

let sink_attrs elt ps =
  Array.map ~f:(fun (name, value) -> Jq.Dom.sink_attr elt ~name ~value) ps
  |> Frp.Subscription.concat

let parse_svg_string str =
  let div = Dom_html.(createDiv document) in
  div##innerHTML <- str;
  Js.Opt.to_option ((div##getElementsByTagName(Js.string "svg"))##item(0))

let svg_file url =
  let req = XmlHttpRequest.create () in
  let response, trigger = Frp.Stream.create' () in
  req##onreadystatechange <- (Js.wrap_callback (fun () ->
    match req##readyState with
    | XmlHttpRequest.DONE -> trigger (req##responseText)
    | _ -> ()));
  let drawing_beh =
    Frp.Stream.map ~f:(fun x -> Some x) response
    |> Frp.latest ~init:None
    |> Frp.Behavior.map ~f:(fun str_opt -> let open Option in let open Monad_infix in
         value_map (str_opt >>= parse_svg_string) ~default:(pictures [||])
          ~f:(fun e -> Svg (Obj.magic e)))
  in
  (* TODO: Think about whether we should be forcing "pulling" as here, or
   * force pushing (i.e., allow an argument to Stream.create which indicates that
   * updates should be pushed to off_listeners as well *)
  Frp.Behavior.force_updates drawing_beh;
  req##_open(Js.string "GET", Js.string url, Js._true);
  req##send(Js.Opt.return (Js.string ""));
  dynamic drawing_beh

module Cache : sig
(*   val load : string -> [`Ok | `Error] Frp.Stream.t *)
  val load : string -> unit

  val get : string -> t option

  val get_exn : string -> t
end = struct
  let cache = Stringtbl.create ()

  let load url =
    let drawing = svg_file url in
    Stringtbl.add cache ~key:url ~data:drawing

  let get url = Stringtbl.find cache url

  let get_exn url =
    match get url with
    | Some x -> x
    | _      -> failwith (Printf.sprintf "Draw.Cache.get_exn: %s not found" url)
end

let rec to_string = function
  | Circle _             -> "Circle"
  | Transform (t, trans) -> Printf.sprintf "Transform(%s)" (to_string t)
  | Polygon _            -> "Polygon"
  | Path _               -> "Path"
  | Path_str _           -> "Path_str"
  | Text _               -> "Text"
  | Rect _               -> "Rect"
  | Pictures ts          -> Printf.sprintf "Pictures(%s)" (String.concat_array (Array.map ~f:to_string ts))
  | Image _              -> "Image"
  | Dynamic t            -> Printf.sprintf "Dynamic (%s)" (to_string (Frp.Behavior.peek t))
  | Clip (t, by)         -> Printf.sprintf "Clip(%s, %s)" (to_string t) (to_string by)
  | Svg _                -> Printf.sprintf "Svg"

let create_uid =
  let uid = ref 0 in
  fun () -> incr uid; !uid

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
  let mk_name_sub name_opt elt = 
    Option.value_map name_opt ~f:(fun name -> Name.init elt name) ~default:Frp.Subscription.empty
  in

  let pixel_str_of_int n = string_of_int n ^ "px" in

  let open Frp in function
  
  | Svg elt -> (elt, Frp.Subscription.empty)

  | Text ({name}, ps, corner, text) ->
    let elt      = Jq.Dom.svg_node "text" [||] in
    let name_sub = mk_name_sub name elt in
    let text_sub = Jq.Dom.sink_html elt text in
    let rest_sub = sink_attrs elt [|
      "x", x_beh corner;
      "y", y_beh corner;
      "style", zip_props ps
    |] 
    in
    (elt, Frp.Subscription.concat [|name_sub; text_sub; rest_sub|])

  | Circle ({name}, ps, r, center) -> 
    let elt = Jq.Dom.svg_node "circle" [||] in
    let name_sub = mk_name_sub name elt in
    let rest_sub = sink_attrs elt [|
      "cx", x_beh center;
      "cy", y_beh center;
      "r", Frp.Behavior.map ~f:string_of_float r;
      "style", zip_props ps
    |]
    in
    (elt, Frp.Subscription.merge name_sub rest_sub)

  | Transform (t, trans) ->
    let (elt, sub) = render t in
    let container  = Jq.Dom.svg_node "g" [||] in
    Jq.Dom.append container elt;
    let trans_sub  = Jq.Dom.sink_attr container
      ~name: "transform"
      ~value:(Frp.Behavior.map ~f:Transform.render_many trans)
    in
    (container, Frp.Subscription.merge trans_sub sub)

  | Path_str ({name}, props, mask, path_strb) ->
    let elt = Jq.Dom.svg_node "path" [||] in
    let sub = Frp.Subscription.concat [|
      Jq.Dom.sink_attr elt ~name:"d" ~value:path_strb;
      path_mask elt path_strb mask props;
      mk_name_sub name elt
    |] in
    (elt, sub)

  | Path ({name}, props, anchor, mask, segs) ->
    let elt = Jq.Dom.svg_node "path" [||] in
    let sub = Frp.Subscription.concat [|
      Jq.Dom.sink_attr elt
        ~name:"d"
        ~value:(Frp.Behavior.zip_with anchor segs ~f:(fun (x, y) sgs -> 
          Printf.sprintf "M%f,%f %s" x y (Segment.render_many sgs)
        ));
      path_mask elt segs mask props;
      mk_name_sub name elt
    |] in
    (elt, sub)

  | Polygon ({name}, props, pts) ->
      let open Frp.Behavior in
      let elt = Jq.Dom.svg_node "polygon" [|
        "style" , render_properties (Array.map ~f:peek props);
        "points", String.concat_array ~sep:"," (Array.map ~f:Point.render (peek pts))
      |]
      in
      let sub = Frp.Subscription.concat [|
        Jq.Dom.sink_attr elt
          ~name:"points"
          ~value:(Frp.Behavior.map ~f:Point.render_many pts);
        mk_name_sub name elt
      |] in
      (elt, sub)

    (* TODO: Add properties. *)
  | Rect ({name}, ps, corner, wb, hb) -> let open Frp.Behavior in
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

  | Image (urlb, widthb, heightb) -> let open Frp.Behavior in
    let elt = Jq.Dom.svg_node "image" [||] in
    let sub = sink_attrs elt [|
      "xlink:href", urlb;
      "width", Frp.Behavior.map ~f:pixel_str_of_int widthb;
      "height", Frp.Behavior.map ~f:pixel_str_of_int heightb
    |]
    in
    (elt, sub)

  | Clip (t, by) ->
    let clip_id              = Printf.sprintf "clip%d" (create_uid ()) in
    let container            = Jq.Dom.svg_node "g" [||] in
    let sub_container        = Jq.Dom.svg_node "g" [|"clip-path", Printf.sprintf "url(#%s)" clip_id|] in
    let clip_holder          = Jq.Dom.svg_node "clipPath" [|"id", clip_id|] in
    let (clip_elt, clip_sub) = render by in
    let (t_elt, t_sub)       = render t in
    Jq.Dom.append clip_holder clip_elt;
    Jq.Dom.append container clip_holder;
    Jq.Dom.append sub_container t_elt;
    Jq.Dom.append container sub_container;
    (container, Frp.Subscription.merge clip_sub t_sub)

  | Pictures pics ->
    let elts = Array.map ~f:render pics in
    let elt = Jq.Dom.svg_node "g" [||] in 
    Array.iter ~f:(fun (e, _) -> Jq.Dom.append elt e) elts;
    (elt, Frp.Subscription.concat (Array.map ~f:snd elts))

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
      last_sub := sub)
    in
    (container, Subscription.(merge dyn_sub (make (fun () -> cancel !last_sub))))

let render_svg_node ~width ~height t =
  let elt, sub = render t in
  let svg = Jq.Dom.svg_node "svg" [| "width", string_of_int width; "height", string_of_int height |] in
  Jq.Dom.append svg elt;
  (svg, sub)

