open Core
open Core.Either

module Line_cap = struct
  type t = [`Flat | `Round | `Padded]
end

module Line_join = struct
  type t = [`Smooth | `Sharp of float | `Clipped]
end

module Image = struct
  type t = Dom_html.imageElement Js.t

  let load url =
    let (s, trigger) = Frp.Stream.create' () in
    let img = Dom_html.(createImg document) in
    img##onload <- Dom.handler (fun _ -> trigger img; Js._false);
    img##src    <- Js.string url;
    s
  ;;
end

module Fill_style = struct
  type t =
    | Solid   of Color.t
    | Texture of Image.t
    | Grad    of Color.Gradient.t
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

  let dotted color = {default with color; dashing = [|3; 3|] }
end

module Path = struct
  type t = (float * float) array

  let create t = t
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
    Array.init n ~f

  let circle r = let d = 2. *. r in oval d d

  let ngon n r =
    let t = 2. *. pi /. float_of_int n in
    let f i = let i = float_of_int i in (r *. cos (t *. i), r *. sin (t *. i)) in
    Array.init n ~f

end

module Transform = struct
  type t =
    { translation : float * float
    ; matrix      : float * float * float * float
    }

  let identity = 
    { translation = (0., 0.)
    ; matrix = (1., 0., 0., 1.)
    }

  let mul_matrix (a, b, c, d) (w, x, y, z) =
    (a *. w +. b *. y, a *. x +. b *. z
    ,c *. w +. d *. y, c *. x +. d *. z)

  let apply_matrix (a, b, c, d) (x, y) = (a *. x +. b *. y, c *. x +. d *. y)

  let add_vec (a, b) (c, d) = (a +. c, b +. d)

  let compose t1 t2 =
    { matrix = mul_matrix t1.matrix t2.matrix
    ; translation = add_vec t1.translation (apply_matrix t1.matrix t2.translation)
    }
end

module Image_style = struct
  type t = Plain | Fitted | Cropped of int * int | Tiled
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

module Position = struct
  type pos = Absolute of int | Relative of float

  let pos_to_js_string p = Js.string (match p with
    | Absolute n -> Printf.sprintf "%dpx" n
    | Relative x -> Printf.sprintf "%f%%" x)

  type t =
    { horizontal : [`P | `Z | `N]
    ; vertical   : [`P | `Z | `N]
    ; x : pos
    ; y : pos
    }

  let middle       = {horizontal = `Z; vertical = `Z; x = Relative 0.5; y = Relative 0.5}
  let top_left     = {horizontal = `N; vertical=`P; x = Absolute 0; y = Absolute 0}
  let top_right    = {top_left with horizontal = `P}
  let bottom_left  = {top_left with vertical = `N}
  let bottom_right = {bottom_left with horizontal = `P}
  let mid_left     = {middle with horizontal = `N; x = Absolute 0}
  let mid_right    = {mid_left with horizontal = `P}
  let mid_top      = {middle with vertical = `P; y = Absolute 0}
  let mid_bottom   = {mid_top with vertical = `N}
end

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

  let style style t = {t with style}
end

