open Core

let guid = let x = ref 0 in (fun () -> (incr x; !x))

module Image_style = struct
  type t = Plain | Fitted | Cropped of int * int | Tiled
end

module Position = struct
  type pos = Absolute of int | Relative of float

  type t =
    { horizontal : [`P | `Z | `N]
    ; vertical   : [`P | `Z | `N]
    ; x : pos
    ; y : pos
    }

  let middle      = {horizontal = `Z; vertical = `Z; x = Relative 0.5; y = Relative 0.5}
  let top_left    = {horizontal = `N; vertical=`P; x = Absolute 0; y = Absolute 0}
  let top_right   = {top_left with horizontal = `P}
  let bottom_left = {top_left with vertical = `N}
  let bottom_left = {bottom_left with horizontal = `P}
  let mid_left    = {middle with horizontal = `N; x = Absolute 0}
  let mid_right   = {mid_left with horizontal = `P}
  let mid_top     = {middle with vertical = `P; y = Absolute 0}
  let mid_bottom  = {mid_top with vertical = `N}
end

module Direction = struct
  type t = Up | Down | Left | Right | In | Out
end

module Alignment = struct
  type t = Left | Right | Center | Justify | Initial | Inherit

  let to_string = function
    | Left    -> "left"
    | Right   -> "right"
    | Center  -> "center"
    | Justify -> "justify"
    | Initial -> "initial"
    | Inherit -> "inherit"
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

type prim =
  | Image of Image_style.t * int * int * string
  | Container of Position.t * t
  | Flow of Direction.t * t array
  | Spacer
  | Raw_HTML of string * Alignment.t option * (string, t) Either.t array
(*     | Custom *)
and t = { props : properties; element : prim }

type element = t

let same_constructor t1 t2 = match t1, t2 with
  | Image _, Image _
  | Container _, Container _
  | Flow _, Flow _ 
  | Spacer, Spacer
  | Raw_HTML _, Raw_HTML _ -> true
  | _ -> false

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

let html_dims ?width html args =
  let div = Dom_html.(createDiv document) in
  div##innerHTML <- (Js.string html);
  (match width with 
    | Some w -> (div##style)##width <- (Js.string (string_of_int w ^ "px"))
    | None -> ());
  (div##style)##visibility <- (Js.string "hidden");
  (div##style)##cssFloat   <- (Js.string "left");
  Dom.appendChild (Dom_html.document##body) div;
  Array.iteri args ~f:(fun i arg -> let span = Dom_html.document in ());
  (failwith "", failwith "")

module Text = struct
  module Style = struct
    module Line = struct
      type t = Under | Over | Through

      let to_string = function
        | Under   -> "underline"
        | Over    -> "overline"
        | Through -> "line-through"
    end

    type t =
      { typeface : string array
      ; height   : float option
      ; color    : Color.t
      ; bold     : bool
      ; italic   : bool
      ; line     : Line.t option
      }

    let default =
      { typeface = [||]
      ; height   = None
      ; color    = Color.black
      ; bold     = false
      ; italic   = false
      ; line     = None
      }
  end

  type t =
    { link  : string option
    ; style : Style.t
    ; text  : string
    }

  let escape =
    let mk_regexp s = jsnew Js.regExp (Js.string s) in
    let double_quote = mk_regexp "\"" in
    let single_quote = mk_regexp "'" in
    let l_angle      = mk_regexp "<" in
    let r_angle      = mk_regexp ">" in
    let newline      = mk_regexp "\n" in
    let replace regexp r s = s##replace(regexp, Js.string r) in
    fun s ->
      Js.to_string (
        replace double_quote "&#34;" (
        replace single_quote "&#39;" (
        replace l_angle "&#60;" (
        replace r_angle "&#62;" (
        replace newline "<br/>" (Js.string s))))))
      (* TODO: Figure out if I need to write a makeSpaces function (libraries/Native/Text.js) *)

  let of_string s = { link = None; style = Style.default; text = escape s }

  let to_html {link; style = {Style.typeface; height; color; bold; italic; line}; text} =
    let style_str =
      Printf.sprintf "color:%s%s%s%s%s"
        (if Array.length typeface > 0
        then "font-family:" ^ String.concat_array ~sep:"," typeface ^ ";"
        else "")
        (match height with Some h -> Printf.sprintf "font-size:%fpx;" h | None -> "")
        (if bold then "font-weight:bold;" else "")
        (if italic then "font-style:italic;" else "")
        (match line with Some l -> "text-decoration:" ^ Style.Line.to_string l ^ ";" | None -> "")
    in
    let inner = match link with
      | None -> text
      | Some href -> Printf.sprintf "<a href=\"%s\">%s</a>" href text
    in
    Printf.sprintf "<span style=\"%s\">%s</span>" style_str inner

  let aligned alignment text =
    let html = to_html text in let args = [||] in
    let (w, h) = html_dims html args in
    let prim = Raw_HTML (html, Some alignment, [||]) in
    new_element w h prim

  let style style t = {t with style}
end

let spacer ~width ~height = new_element width height Spacer

let empty = spacer 0 0

let width e = e.props.width

let height e = e.props.height

let size e = (e.props.width, e.props.height)

let round x : int =
  Js.Unsafe.(meth_call (variable "Math") "round" [|inject (Js.number_of_float x)|])

let with_width new_width {props; element} =
  let props' = match element with
    | Image (_, w, h, _) ->
      {props with height = round (float_of_int new_width *. float_of_int h /. float_of_int w)}
    | Raw_HTML (html, _, args) ->
      {props with height = snd (html_dims ~width:new_width html args) } 
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

module Render : sig
  val render : t Frp.Behavior.t -> Dom_html.element Js.t -> unit
end = struct

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

  let new_element tag =
    let e = Dom_html.document##createElement(Js.string tag) in
    (e##style)##padding <- Js.string "0";
    (e##style)##margin  <- Js.string "0";
    e
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
        Js.Unsafe.set (div##style) (Js.string (vendor ^ "BackgroundSize")) (Js.string "cover")
      in
      (div##style)##background <- Js.string (Printf.sprintf "url(%s) no-repeat center" url);
      set_bg_size "webkit";
      set_bg_size "Moz";
      set_bg_size "O";
      Js.Unsafe.set (div##style) (Js.string "backgroundSize") (Js.string "cover");
      div
    in
    let cropped_image (x, y) img_w img_h props_w props_h url =
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
    fun style w h url -> match style with
      | 
    

  let make_element {props; element} = match element with
    | Image -> image props 

  let render t = set_props t (make_element t)

  let unsafe_opt_value (x : 'a Js.opt) : 'a = Obj.magic x

  let update node curr next =
    let node : oak_element Js.t =
      if node##tagName = Js.string "a" then Obj.magic (node##firstChild) else node in
    if curr.props.id = next.props.id
    then update_props node curr next
    else if not (same_constructor curr.element next.element)
    then (unsafe_opt_value (node##parentNode))##replaceChild(render next, node)


  let render _ _ = failwith ""

end

