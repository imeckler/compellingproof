open Core
open Either
open Oak_common

(* Some pains had to be taken to handle mutual recursion *)

class type oak_style_declaration = object
  inherit Dom_html.cssStyleDeclaration
  method pointerEvents : Js.js_string Js.t Js.prop
end

class type oak_element = object
  inherit Dom_html.element
  method oakHoverHandler
    : (unit, Dom_html.mouseEvent Js.t) Dom_html.event_listener Js.optdef Js.prop
  method hoverOverId : Dom.event_listener_id Js.optdef Js.prop
  method hoverOutId : Dom.event_listener_id Js.optdef Js.prop
  method hoverTriggered : bool Js.prop

  method oakClickHandler
    : (unit, Dom_html.mouseEvent Js.t) Dom_html.event_listener Js.optdef Js.prop
  method clickId : Dom.event_listener_id Js.optdef Js.prop

  method style_ : oak_style_declaration Js.t Js.prop
end

type properties =
  { id      : int
  ; width   : int
  ; height  : int
  ; opacity : float
  ; color   : Color.t option
  ; href    : string
  ; tag     : string
  ; hover   : (unit, Dom_html.mouseEvent Js.t) Dom_html.event_listener option
  ; click   : (unit, Dom_html.mouseEvent Js.t) Dom_html.event_listener option
  }

type basic_form =
  | Path of Line_style.t * Path.t
  | Shape of (Line_style.t, Fill_style.t) Either.t * Shape.t
  | Image of int * int * (int * int) * Image.t (* TODO: Let's see how this works *)
  | Group of Transform.t * form array

and form = 
  { theta : float
  ; scale : float
  ; x     : float
  ; y     : float
  ; alpha : float
  ; form  : basic_form
  }

and basic_element =
  | Image of Image_style.t * int * int * string
  | Container of Position.t * element
  | Flow of Direction.t * element array
  | Spacer
  | Raw_HTML of string * Alignment.t option (* * (string, t) Either.t array *)
  | Collage of int * int * form array

and element = { props : properties; element : basic_element }

let round x : int =
  Js.Unsafe.(meth_call (variable "Math") "round" [|inject (Js.number_of_float x)|])

module Form = struct
  type t = form

  let basic form =
    { theta = 0.
    ; scale = 1.
    ; x     = 0.
    ; y     = 0.
    ; alpha = 1.
    ; form
    }

  let fill style shape = basic (Shape (InR style, shape))

  let filled color shape = fill (Fill_style.Solid color) shape

  let textured img shape = fill (Fill_style.Texture img) shape

  let gradient grad shape = fill (Fill_style.Grad grad) shape

  let outlined style shape = basic (Shape (InL style, shape))

  let traced style path = basic (Path (style, path))

  let sprite w h pos url = basic (Image (w, h, pos, url))

  let transform_group trans fs = basic (Group (trans, fs))

  let group fs = transform_group Transform.identity fs

  let move (x, y) f = { f with x = f.x +. x; y = f.y +. y }

  let move_x x f = { f with x = f.x +. x }

  let move_y y f = { f with y = f.y +. y }

  let scale s f = { f with scale = f.scale *. s }

  let rotate t f = { f with theta = f.theta +. t }

  let alpha a f = { f with alpha = a }

  let collage = Oak_element.collage

end

(* TODO: Gonna shelf Element for now to get Form done. Perhaps I'll return
 * to it someday *)
module Element = struct
  let guid = let x = ref 0 in (fun () -> (incr x; !x))

  type t = element

  let new_element width height element =
    let props =
      { id = guid ()
      ; width; height
      ; opacity = 1.
      ; color = None
      ; href = ""; tag = ""
      ; hover = None; click = None
      }
    in
    {props; element}

  let collage w h forms = new_element w h (Collage (w, h, forms))

  let html_dims = 
    let get_style t : Dom_html.cssStyleDeclaration Js.t = Js.Unsafe.(
      meth_call Dom_html.window "getComputedStyle" [|inject t; inject (Js.Opt.empty)|])
    in
    let parse_dim (s : Js.js_string Js.t) =
      int_of_float (ceil Js.Unsafe.(fun_call (variable "parseFloat") [|inject (s##slice(0, -2))|])) in
    fun ?width html ->
    let div = Dom_html.(createDiv document) in
    div##innerHTML <- (Js.string html);
    (match width with 
      | Some w -> (div##style)##width <- (Js.string (string_of_int w ^ "px"))
      | None -> ());
    (div##style)##visibility <- (Js.string "hidden");
    (div##style)##cssFloat   <- (Js.string "left");
    Dom.appendChild (Dom_html.document##body) div;
    let style = get_style div in
    let dims = (parse_dim (style##width), parse_dim (style##height)) in
    Dom.removeChild Dom_html.document div;
    dims


  let of_text alignment text =
    let html = Text.to_html text in
    let (w, h) = html_dims html in
    let prim = Raw_HTML (html, Some alignment) in
    new_element w h prim

  let spacer ~width ~height = new_element width height Spacer

  let empty = spacer 0 0

  let width e = e.props.width

  let height e = e.props.height

  let size e = (e.props.width, e.props.height)

  let with_width new_width {props; element} =
    let props' = match element with
      | Image (_, w, h, _) ->
        {props with height = round (float_of_int new_width *. float_of_int h /. float_of_int w)}
      | Raw_HTML (html, _) ->
        {props with height = snd (html_dims ~width:new_width html) } 
      | _ -> props
    in
    {element; props = {props' with width = new_width}}

  let with_height new_height {props; element} =
    let props' = match element with
      | Image (_, w, h, _) ->
        {props with width = round (float_of_int new_height *. float_of_int w /. float_of_int h)}
      | _ -> props
    in
    {element; props = {props' with height = new_height}}

  let with_size ~width ~height t = with_height height (with_width width t)

  let with_opacity opacity {element; props} = {element; props = {props with opacity}}

  let with_color c {element; props} = {element; props = {props with color = Some c}}

  let link href {element; props} = { element; props = {props with href}}

  let image ?(style=Image_style.Plain) ~width ~height src =
    new_element width height (Image (style, width, height, src))

  let container ~width ~height pos t =
    new_element width height (Container (pos, t))

  let sum ns = Array.fold ~init:0 ~f:(+) ns

  let flow dir ts =
    if Array.length ts = 0 then empty
    else
      let ws = Array.map ~f:width ts in
      let hs = Array.map ~f:height ts in
      let new_flow w h = new_element w h (Flow (dir, ts)) in
      let open Direction in match dir with
      | Up   | Down  -> new_flow (Array.maximum ws) (sum hs)
      | Left | Right -> new_flow (sum ws) (Array.maximum hs)
      | In   | Out   -> new_flow (Array.maximum ws) (Array.maximum hs)

  let (---) hi lo =
    new_element (max (width hi) (width lo)) (height hi + height lo)
      (Flow (Direction.Down, [|hi; lo|]))

  let (|||) left right =
    new_element (width left + width right) (max (height left) (height right))
      (Flow (Direction.Right, [|left; right|]))

  let layers ts = flow Direction.Out ts

end

module Render_form = struct
  type canvas_ctx = Dom_html.canvasRenderingContext2D Js.t

  let trace (ctx : canvas_ctx)  (path : Path.t) closed =
    if Array.length path > 1
    then begin
      (match path.(0) with (x, y) -> ctx##moveTo(x, y));
      for i = 1 to Array.length path - 1 do
        match path.(i) with (x, y) -> ctx##lineTo(x, y)
      done;
      if closed
      then match path.(0) with (x, y) -> ctx##lineTo(x, y)
    end
  ;;

  let custom_line_help ctx style path =
    if Array.length path > 1
    then 
      let pattern    = style.Line_style.dashing in
      let p_len      = Array.length pattern in
      let (x0, y0)   = path.(Array.length path - 1) in
      let rec loop i (x0, y0) pindex draw =
        if i < 0
        then ()
        else
          let (x1, y1) = path.(i) in
          let dx = x1 -. x0 in let dy = y1 -. y0 in
          let rec loop2 segment_length remaining (dx, dy) draw =
            failwith ""
          in
          failwith ""
(*           loop2 () (sqrt (dx *. dx +. dy *. dy)) *)
      in
      ctx##moveTo(x0, y0);
      failwith ""
  ;;

  let line ctx style path closed =
    if Array.length style.Line_style.dashing = 0 then trace ctx path closed
    else custom_line_help ctx style path;
    ctx##scale(1., -1.);
    ctx##stroke()
  ;;

  let draw_line (ctx : canvas_ctx) style path closed =
    ctx##lineWidth <- style.Line_style.width;
    ctx##lineCap <- Js.string (match style.Line_style.cap with
      | `Flat -> "butt" | `Round -> "round" | `Padded -> "square");
    ctx##lineJoin <- Js.string (match style.Line_style.join with
      | `Smooth -> "round" | `Sharp _ -> "miter" | `Clipped -> "bevel");
    ctx##miterLimit <- (match style.Line_style.join with
      | `Sharp x -> x | _ -> 10.);
    (* TODO: Check this *)
    ctx##strokeStyle <- Js.string (Color.to_css_string style.Line_style.color);
    line ctx style path closed
  ;;

  let texture ctx img =
    ctx##createPattern(img, Js.string "repeat")
  ;;

  let gradient (ctx : canvas_ctx) grad =
    let g, stops = match grad with
      | Color.Gradient.Linear ((x, y), (a, b), colors) ->
        ctx##createLinearGradient(x, -.y, a, -.b), colors
      | Color.Gradient.Radial ((x, y), s, (a, b), t, colors) ->
        ctx##createRadialGradient(x, -.y, s, a, -.b, t), colors
    in
    Array.iter stops ~f:(fun (x, c) -> g##addColorStop(x, Js.string (Color.to_css_string c)));
    g
  ;;

  let draw_shape ctx style path =
    trace ctx path false (* TODO: Idk about false here *);
    Fill_style.(match style with
      | Solid color -> ctx##fillStyle          <- Js.string (Color.to_css_string color)
      | Texture img -> ctx##fillStyle_pattern  <- texture ctx img
      | Grad g      -> ctx##fillStyle_gradient <- gradient ctx g);
    ctx##scale(1., -1.);
    ctx##fill()
  ;;

  let draw_image ctx (w, h, (src_x, src_y), img) =
    let src_x = float_of_int src_x in let src_y = float_of_int src_y in
    let w = float_of_int w in let h = float_of_int h in
    let dest_x = -.w /. 2. in let dest_y = -.h /. 2. in
    ctx##scale(1., -1.);
    ctx##drawImage_full(img, src_x, src_y, w, h, dest_x, dest_y, w, h)
  ;;

  let transform (ctx : canvas_ctx) {Transform.translation=(x,y); matrix=(a,b,c,d)} =
    ctx##transform(a, b, c, d, x, y)
  ;;

  let make_canvas w h =
    let canvas = Dom_html.(createCanvas document) in
    (canvas##style)##width    <- Js.string (Printf.sprintf "%dpx" w);
    (canvas##style)##height   <- Js.string (Printf.sprintf "%dpx" h);
    (canvas##style)##display  <- Js.string "block";
    (canvas##style)##position <- Js.string "absolute";
    canvas##width  <- w;
    canvas##height <- h;
    canvas
  ;;

  let rec render_form (ctx : canvas_ctx) {x;y;theta;scale;alpha;form} =
    ctx##save();
      if x <> 0. || y <> 0. then ctx##translate(x, y);
      if theta <> 0. then ctx##rotate(theta);
      if scale <> 1. then ctx##scale(scale, scale);
      if alpha <> 1. then ctx##globalAlpha <- Js.to_float (ctx##globalAlpha) *. alpha;
      ctx##beginPath();
      Form.(match form with
        | Path (style, path)       -> draw_line ctx style path false
        | Image (w, h, pos, img)   -> draw_image ctx (w, h, pos, img)
        | Shape (InR style, shape) -> draw_shape ctx style shape
        | Shape (InL style, shape) -> draw_line ctx style shape true
        | Group (trans, fs)        ->
          (transform ctx trans; Array.iter ~f:(render_form ctx) fs));
    ctx##restore()
  ;;

  let draw ~width ~height div form_b =
    let canvas = make_canvas width height in
    let ctx = canvas##getContext(Dom_html._2d_) in
    Dom.appendChild div canvas;
    render_form ctx (Frp.Behavior.peek form_b);
    Frp.Stream.iter (Frp.Behavior.changes form_b)
      ~f:(fun form -> render_form ctx form)
  ;;

end

module Render_element = struct
  let new_element tag : oak_element Js.t =
    let e = Dom_html.document##createElement(Js.string tag) in
    (e##style)##padding <- Js.string "0";
    (e##style)##margin  <- Js.string "0";
    Obj.magic e
  ;;

  let add_hover (e : oak_element Js.t) handler =
    (e##style_)##pointerEvents <- Js.string "auto";
    e##oakHoverHandler         <- Js.Optdef.return handler;
    e##hoverTriggered          <- false;
    let mk_handler cond hover_triggered = Dom.handler (fun evt ->
      if not (cond evt)
      then (
        e##hoverTriggered <- hover_triggered;
        Js.Optdef.iter (e##oakHoverHandler) (fun h -> ignore (Dom.invoke_handler h () evt));
        (Obj.magic evt)##stopPropagation()
      );
      Js._false)
    in
    let over = mk_handler (fun _ -> e##hoverTriggered) true in
    let out  = mk_handler (fun evt -> Js.Unsafe.(meth_call e "contains" [|inject (evt##toElement)|])) false in
    (* TODO: Check relatedTarget property as well *)
    let over_id = Dom.addEventListener (Js.Unsafe.obj [||]) (Dom.Event.make "mouseover") over Js._false in
    let out_id  = Dom.addEventListener (Js.Unsafe.obj [||]) (Dom.Event.make "mouseout") out Js._false in
    e##hoverOverId <- Js.Optdef.return over_id;
    e##hoverOutId  <- Js.Optdef.return out_id
  ;;

  let add_click (e : oak_element Js.t) handler =
    (e##style_)##pointerEvents <- Js.string "auto";
    e##oakClickHandler <- Js.Optdef.return handler;
    let click = Dom.handler (fun evt ->
      Js.Optdef.iter (e##oakClickHandler) (fun h -> ignore (Dom.invoke_handler h () evt));
      (Obj.magic evt)##stopPropagation())
    in
    e##clickId <- Js.Optdef.return (
      Dom.addEventListener (Js.Unsafe.obj [||]) (Dom.Event.make "click") click Js._false)

  let remove_hover (e : oak_element Js.t) = 
    Js.Optdef.iter (e##hoverOverId) Dom.removeEventListener;
    Js.Optdef.iter (e##hoverOutId) Dom.removeEventListener 
  ;;

  let remove_click (e : oak_element Js.t) =
    Js.Optdef.iter (e##clickId) Dom.removeEventListener

  let wrap_in_anchor node url =
    let a : Dom_html.anchorElement Js.t = Obj.magic (new_element "a") in
    let a_style = a##style in
    a##href           <- Js.string url;
    a_style##width    <- Js.string "100%";
    a_style##height   <- Js.string "100%";
    a_style##top      <- Js.string "0";
    a_style##left     <- Js.string "0";
    a_style##display  <- Js.string "block";
    a_style##position <- Js.string "absolute";
    (Obj.magic a_style)##pointerEvents <- Js.string "auto";
    (node##style)##position <- Js.string "relative";
    Dom.appendChild node a
  ;;

  let update_props (node : oak_element Js.t) curr next =
    if next.props.width <> curr.props.width
    then (node##style)##width <- (Js.string (Printf.sprintf "%dpx" next.props.width));

    if next.props.height <> curr.props.height
    then (node##style)##height <- (Js.string (Printf.sprintf "%dpx" next.props.height));

    if next.props.opacity <> curr.props.opacity
    then (node##style)##opacity <- (Js.Optdef.return ((Js.number_of_float next.props.opacity)##toString()));

    let next_color = Js.string
      (Option.value_map ~default:"" ~f:Color.to_css_string next.props.color) in
    if (node##style)##backgroundColor <> next_color
    then (node##style)##backgroundColor <- (
      if next_color = Js.string "" then Js.string "transparent" else next_color);

    if next.props.tag <> curr.props.tag
    then node##id <- Js.string next.props.tag;

    if next.props.href <> curr.props.href
    then (match curr.props.href with
      | "" -> wrap_in_anchor node next.props.href
      | _ -> (Obj.magic (node##lastChild))##href <- Js.string next.props.href);
    (* TODO: Why does elm use a property called "lastNode"? runtime/Render/Element.js:392 *)
    (match next.props.hover with
    | None -> remove_hover node
    | Some h ->
        if Js.Optdef.test (node##oakHoverHandler)
        then node##oakHoverHandler <- Js.Optdef.return h
        else add_hover node h);
    (match next.props.click with
    | None -> remove_click node
    | Some h ->
        if Js.Optdef.test (node##oakClickHandler)
        then node##oakClickHandler <- Js.Optdef.return h
        else add_click node h)
  ;;

  let set_props {props = {width; height; color; opacity; tag; href; hover; click}; element} (node : oak_element Js.t) =
    (node##style)##width <- Js.string (Printf.sprintf "%dpx" width);
    (node##style)##height <- Js.string (Printf.sprintf "%dpx" height);
    if opacity <> 1. then (node##style)##opacity <- Js.Optdef.return ((Js.number_of_float opacity)##toString());
    Option.iter color ~f:(fun c -> (node##style)##backgroundColor <- Js.string (Color.to_css_string c));
    (match tag with "" -> () | _ ->  node##id <- Js.string tag);
    (match href with "" -> () | _ -> wrap_in_anchor node href);
    Option.iter ~f:(add_hover node) hover;
    Option.iter ~f:(add_click node) click
  ;;

  let unsafe_set o s v = Js.Unsafe.set o (Js.string s) v

  let image =
    let plain_image url =
      let img = Dom_html.(createImg document) in
      img##src <- Js.string url;
      (Obj.magic img)##name <- Js.string url;
      (img##style)##display <- Js.string "block";
      img
    in
    let tiled_image url =
      let div = new_element "div" in
      (div##style)##backgroundImage <- Js.string (Printf.sprintf "url(%s)" url);
      div
    in
    let fitted_image w h url =
      let div = new_element "div" in
      let set_bg_size vendor =
        unsafe_set (div##style) (vendor ^ "BackgroundSize") (Js.string "cover")
      in
      (div##style)##background <- Js.string (Printf.sprintf "url(%s) no-repeat center" url);
      set_bg_size "webkit";
      set_bg_size "Moz";
      set_bg_size "O";
      unsafe_set (div##style) "backgroundSize" (Js.string "cover");
      div
    in
    let cropped_image x y img_w img_h props_w props_h url =
      let e = new_element "div" in
      (e##style)##overflow <- Js.string "hidden";
      let img : Dom_html.imageElement Js.t = Obj.magic (new_element "img") in
      img##onload <- Dom.full_handler (fun this _ ->
        let sw = float_of_int props_w /. float_of_int img_w in
        let sh = float_of_int props_h /. float_of_int img_h in
        (img##style)##width <- Js.string (Printf.sprintf "%dpx"
          (round (float_of_int (this##width) *.sw)));
        (img##style)##width <- Js.string (Printf.sprintf "%dpx"
          (round (float_of_int (this##height) *.sh)));
        (img##style)##marginLeft <- Js.string (Printf.sprintf "%dpx"
          (round (float_of_int (-x) *. sw)));
        (img##style)##marginTop <- Js.string (Printf.sprintf "%dpx"
          (round (float_of_int (-y) *. sh)));
        Js._false);
      img##src <- Js.string url;
      (Obj.magic img)##name <- Js.string url;
      Dom.appendChild e img;
      e
    in
    fun elt_width elt_height style img_w img_h url -> let open Image_style in match style with
      | Plain          -> Obj.magic (plain_image url)
      | Fitted         -> fitted_image elt_width elt_height url
      | Tiled          -> tiled_image url
      | Cropped (x, y) -> cropped_image x y img_w img_h elt_width elt_height url
  ;;

  let raw_html html align = 
    let div = new_element "div" in
    let style = div##style_ in
    div##innerHTML <- Js.string html;
    Option.iter align ~f:(fun a -> style##textAlign <- Js.string (Alignment.to_string a));
    style##visibility    <- Js.string "visible";
    style##pointerEvents <- Js.string "auto";
    div
  ;;

  let set_pos {Position.vertical; horizontal; x=pos_x; y=pos_y} 
      {element; props = {width; height;_}} (node : oak_element Js.t)
    =
    let node_style = node##style in
    node_style##position <- Js.string "absolute";
    node_style##margin <- Js.string "auto";
    let transform_str a axis dir1 dir2 dim pos = match a with
      | `P -> (
          unsafe_set node_style dir1 (Position.pos_to_js_string pos);
          Js.Unsafe.(meth_call node_style "removeProperty" [|inject (Js.string dir2)|]);
          "")
      | `Z -> Printf.sprintf "translate%c(%dpx) " axis (-dim / 2)
      | `N -> (
          unsafe_set node_style dir2 (Position.pos_to_js_string pos);
          Js.Unsafe.(meth_call node_style "removeProperty" [|inject (Js.string dir2)|]);
          "")
    in
    let transform_str =
        transform_str horizontal 'X' "right" "left" width pos_x
      ^ transform_str vertical 'Y' "bottom" "top" height pos_y
    in
    if transform_str <> "" then (let set = Js.Unsafe.set in
      let trans = Js.string transform_str in
      unsafe_set node_style "transform" trans;
      unsafe_set node_style "msTransform" trans;
      unsafe_set node_style "MozTransform" trans;
      unsafe_set node_style "webkitTransform" trans;
      unsafe_set node_style "OTransform" trans
    )
  ;;

  let go_dir =
    let go_down e  = e in
    let go_out e   = (e##style)##position <- Js.string "absolute"; e in
    let go_right e = (e##style)##cssFloat <- Js.string "left"; e in let open Direction in
    function
      | Up   | Down  -> go_down
      | Left | Right -> go_right
      | In   | Out   -> go_out

  let should_reverse = Direction.(function Up | Left | In -> true | _ -> false)

  let rec flow = let open Direction in
    let iter_backwards arr ~f =
      let rec loop i = if i < 0 then () else (f arr.(i); loop (i - 1)) in
      loop (Array.length arr - 1)
    in
    fun dir ts ->
      let container = new_element "div" in
      (match dir with Out -> (container##style_)##pointerEvents <- Js.string "none" | _ -> ());
      let go = go_dir dir in
      let f child = Dom.appendChild container (go (render child)) in
      if should_reverse dir
      then iter_backwards ts ~f
      else Array.iter ts ~f;
      container
  and container pos t =
    let e = render t in
    set_pos pos t e;
    let div = new_element "div" in
    (div##style)##position <- Js.string "relative";
    (div##style)##overflow <- Js.string "hidden";
    Dom.appendChild div e;
    div
  and make_element {props; element} = match element with
    | Image (style, w, h, url)  -> image props.width props.height style w h url
    | Flow (dir, ts)            -> flow dir ts
    | Container (pos, t)        -> container pos t
    | Spacer                    -> new_element "div"
    | Raw_HTML (html, align)    -> raw_html html align
(*     | Collage (w, h, forms)     ->  *)
  and render t : oak_element Js.t = let e = make_element t in (set_props t e; e)

  let unsafe_opt_value (x : 'a Js.opt) : 'a = Obj.magic x

  let rec update =
    let replace_with_next node next =
      Dom.replaceChild (unsafe_opt_value (node##parentNode)) (render next) node
    in
    fun node curr next ->
    let node : oak_element Js.t =
      if node##tagName = Js.string "a" then Obj.magic (node##firstChild) else node in

    if curr.props.id = next.props.id
    then update_props node curr next
    else begin
      let cont = match curr.element, next.element with
      | Spacer, Spacer                             -> `Continue

      | Raw_HTML (c_html, _), Raw_HTML (n_html, _) ->
        (* TODO: Why not update alignment? *)
        (if c_html <> n_html then node##innerHTML <- Js.string n_html; `Continue)

      | Image (c_sty, c_w, c_h, c_url), Image (n_sty, n_w, n_h, n_url) ->
        (match n_sty with
        | Image_style.Plain -> (
            if c_url <> n_url then unsafe_set node "src" (Js.string n_url);
            `Continue)
        | _ ->
            if next.element <> curr.element
            || next.props.width <> curr.props.width
            || next.props.height <> curr.props.height
            then (replace_with_next node next; `Stop)
            else `Continue)

      | Flow (c_dir, c_ts), Flow (n_dir, n_ts) ->
        Direction.(match c_dir, n_dir with
        | Up, Up | Down, Down | Left, Left | Right, Right | In, In | Out, Out -> 
          let kids = node##childNodes in
          if Array.length n_ts <> kids##length
          then (replace_with_next node next; `Stop)
          else
            let go         = go_dir n_dir in
            let rev        = should_reverse n_dir in
            let n          = Array.length n_ts in
            let rec loop i =
              if i < 0 then ()
              else (
                update (Obj.magic (kids##item(if rev then n - i - 1 else i)))
                  c_ts.(i) n_ts.(i);
                ignore (go (Obj.magic (kids##item(i))));
                loop (i - 1))
            in
            (loop (n - 1); `Continue)
        | _ -> replace_with_next node next; `Stop)
      | Container (c_pos, c_t), Container (n_pos, n_t) -> (
          let child = Obj.magic (node##firstChild) in
          update child c_t n_t;
          set_pos n_pos n_t child;
          `Continue)
      in ()
    end


    (*
    else if not (same_constructor curr.element next.element)
    then ignore (
      (unsafe_opt_value (node##parentNode))##replaceChild(
        (render next :>  Dom.node Js.t), (node :> Dom.node Js.t)))
    else begin
      let cont =
        match next.element with
        | Spacer -> `Continue
        | Raw_HTML (html, _) -> 
          if html <> (match
    end
*)


  let render _ _ = failwith ""

end
