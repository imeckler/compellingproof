open Core
open Core.Either

module Line_cap = struct
  type t = [`Flat | `Round | `Padded]
end

module Line_join = struct
  type t = [`Smooth | `Sharp of float | `Clipped]
end

module Fill_style = struct
  type t =
    | Solid of Color.t
    | Texture of string
    | Grad of Color.Gradient.t
end

module Line_style = struct
  type t =
    { color : Color.t
    ; width : float
    ; cap : Line_cap.t
    ; join : Line_join.t
    ; dashing : int array
    ; dash_offset : int
    }

  let default =
    { color = Color.black
    ; width = 1.
    ; cap = `Flat
    ; join = `Sharp 10.
    ; dashing = [||]
    ; dash_offset = 0
    }

  let solid color = { default with color }

  let dashed color = { default with color; dashing = [|8; 4|] }

  let dotted color = { default with color; dashing = [|3; 3|] }
end

module Path = struct
  type t = (float * float) array
end

module Shape = struct
  type t = (float * float) array

  let polygon points = points

  let rect w h =
    let hw = w /. 2. in let hh = h /. 2. in
    [| (-. hw, -. hh); (-. hw, hh); (hw, hh); (hw, -. hh) |]

  let square s = rect s s

  let pi = 4. *. atan 1.

  let oval w h =
    let n  = 50 in
    let t  = 2. *. pi /. float_of_int n in
    let hw = w /. 2. in
    let hh = h /. 2. in
    let f i = let i = float_of_int i in (hw *. cos (t *. i), hh *. sin (t *. i)) in
    List.init n ~f

  let circle r = let d = 2. *. r in oval d d

  let ngon n r =
    let t = 2. *. pi /. float_of_int n in
    let f i = let i = float_of_int i in (r *. cos (t *. i), r *. sin (t *. i)) in
    List.init n ~f

end

module Transform = struct
  type t =
    { translation : int * int
    ; matrix      : int * int * int * int
    }

  let identity = 
    { translation = (0, 0)
    ; matrix = (1, 0, 0, 1)
    }
end

module Form = struct
  type basic =
    | Path of Line_style.t * Path.t
    | Shape of (Line_style.t, Fill_style.t) Either.t * Shape.t
    | Image of int * int * (int * int) * string
    | Group of Transform.t * t array
  and t = 
    { theta : float
    ; scale : float
    ; x     : float
    ; y     : float
    ; alpha : float
    ; form  : basic
    }

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

  let textured url shape = fill (Fill_style.Texture url) shape

  let gradient grad shape = fill (Fill_style.Grad grad) shape

  let outlined style shape = basic (Shape (InL style, shape))

  let traced style path = basic (Path (style, path))

  let sprite w h pos url = basic (Image (w, h, pos, url))

  let tranform_group trans fs = basic (Group (trans, fs))

  let group fs = tranform_group Transform.identity fs

  let move (x, y) f = { f with x = f.x +. x; y = f.y +. y }

  let move_x x f = { f with x = f.x +. x }

  let move_y y f = { f with y = f.y +. y }

  let scale s f = { f with scale = f.scale *. s }

  let rotate t f = { f with theta = f.theta +. t }

  let alpha a f = { f with alpha = a }

  let collage = failwith "" (* TODO *)
end

module Render = struct
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
          loop2 () (sqrt (dx *. dx +. dy *. dy))
      in
      ctx##moveTo(x0, y0);
      loop (Array.length path - 2)

  let line ctx style path closed =
    if Array.length style.Line_style.dashing = 0 then trace ctx path closed
    else custom_line_help ctx style path;
    ctx##scale(1., -1.);
    ctx##stroke()

  let render_color {Color.r; g; b; alpha} =
    Printf.sprintf "rgba(%d,%d,%d,%f)" r g b alpha

  let draw_line =
    fun (ctx : canvas_ctx) style path closed ->
    ctx##lineWidth <- style.Line_style.width;
    ctx##lineCap <- Js.string (match style.Line_style.cap with
      | `Flat -> "butt" | `Round -> "round" | `Padded -> "square");
    ctx##lineJoin <- Js.string (match style.Line_style.join with
      | `Smooth -> "round" | `Sharp _ -> "miter" | `Clipped -> "bevel");
    ctx##miterLimit <- (match style.Line_style.join with
      | `Sharp x -> x | _ -> 10.);
    (* TODO: Check this *)
    ctx##strokeStyle <- Js.string (render_color style.Line_style.color);
    line ctx style path closed

  let texture redo ctx src = (* TODO *) ()

  let gradient (ctx : canvas_ctx) grad =
    let g, stops = match grad with
      | Color.Gradient.Linear ((x, y), (a, b), colors) ->
        ctx##createLinearGradient(x, -.y, a, -.b), colors
      | Color.Gradient.Radial ((x, y), s, (a, b), t, colors) ->
        ctx##createRadialGradient(x, -.y, s, a, -.b, t), colors
    in
    Array.iter stops ~f:(fun (x, c) -> g##addColorStop(x, Js.string (render_color c)));
    g

  let draw_shape redo ctx style path =
    trace ctx path false

end

