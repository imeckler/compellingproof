open Core

module Point = struct
  type t = { x : float ; y : float }
end

module Color = struct
  type t = { r : int; g : int; b : int; alpha : float }

  let of_rgb ?(alpha=0.0) ~r ~g ~b = {r; g; b; alpha}

  let render {r; g; b; alpha} =
    Printf.sprintf "rgba(%d,%d,%d,%f)" r g b alpha
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
  | Circle    of Property.t array * float * Point.t 
  | Transform of t * Transform.t
  | Path      of Property.t array * Point.t list
(*   | Bezier *)
  | Beside    of t list
  | Stack     of t list
  | Rect      of Property.t array * (float * float) * (float * float)


let rec render =
  let mk_svg tag style = Jq.jq (Printf.sprintf "<%s style=%s>" tag style)  in
  function
  | Circle (ps, r, {Point.x; y}) ->
      mk_svg "circle" (String.concat_array ~sep:"," (Array.map ps ~f:Property.render))
  | Transform (t, trans) ->
      let e = render t in
      Jq.set_attr e ~name:"transform" ~value:(Transform.render trans);
      e
  | Path (ps, pts) -> 

