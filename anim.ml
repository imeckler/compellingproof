type shape

open Core

module Layout = struct
  type t =
    | Columns of t list
    | Rows    of t list
    | Sized   of int * int * t
    | Elt     of Jq.t
    | Tex     of string

  let rec render = function
    | Columns ts ->
      let x = Jq.create "div" in
      List.iter ts ~f:(fun t ->
        let c = render t in
        Jq.css c [| "display", "inline-block" |];
        Jq.append x (render t)
      );
      x
    | Rows ts ->
      let x = Jq.create "div" in
      List.iter ts ~f:(fun t -> Jq.append x (render t));
      x
    | Sized (w, h, t) ->
      let x = render t in
      Jq.css x [| ("width", string_of_int w); ("height", string_of_int h) |];
      x
    | Elt e -> e
    | Tex s -> Jq.create ("div>" ^ s ^ "</div")
  ;;

  let beside l r = match l, r with
    | Columns cs, Columns cs' -> Columns (cs @ cs')
    | Columns cs, _           -> Columns (cs @ [r])
    | _         , Columns cs' -> Columns (l :: cs')
    | _         , _           -> Columns [l; r]
  ;;

  let stack top bot = match top, bot with
    | Rows rs, Rows rs' -> Rows (rs @ rs')
    | Rows rs, _        -> Rows (rs @ [bot])
    | _      , Rows rs' -> Rows (top :: rs')
    | _      , _        -> Rows [top; bot]
  ;;

  let combine = function
    | `Right -> beside
    | `Below -> stack
  ;;
end

module Slide = struct
  module Enter = struct
    type t = [`Right | `Below] * [`Layout of Layout.t | `Enters of t list ]
  end

  module Action = struct
    type t =
      | Enter of Enter.t
      | Wait of int
  end

  let rec layout (xs : Enter.t list) : Layout.t = 
    let (rights, belows) =
      List.partition_map xs ~f:(function
        | (`Right, `Layout a)  -> InL a
        | (`Right, `Enters ys) -> InL (layout ys)
        | (`Below, `Layout a)  -> InR a
        | (`Below, `Enters ys) -> InR (layout ys)
      )
    in
    Layout.(Columns (Rows belows :: rights))
  ;;

  let rec render (xs : Enter.t list) : (Jq.t * Jq.t list) =
    let outer = Jq.create "div" in
    let div   = Jq.create "div" in
    Jq.append outer div;
    let entrances =
      List.concat_map xs ~f:(function
        | `Right, `Layout a ->
          let c = Layout.render a in
          Jq.css c [| "display", "inline-block" |];
          println "Right, Layout";
          Jq.append outer c;
          [c]
        | `Right, `Enters ys ->
          let (c, cs) = render ys in
          println "Right, Enters";
          Jq.css c [| "display", "inline-block" |];
          Jq.append outer c;
          cs
        | `Below, `Layout a ->
          let c = Layout.render a in
          println "Below, Layout";
          print c;
          Jq.append div c;
          [c]
        | `Below, `Enters ys ->
          let (c, cs) = render ys in
          println "Below, Enters";
          Jq.append div c;
          cs
      )
    in
    (outer, entrances)
  ;;
end

