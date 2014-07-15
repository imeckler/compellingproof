open Core

module Heading = struct
  type t =
    { color : Color.t
    ; text  : Text.t
    }
end

module Border_style = struct
  type t =
    | Standard of Draw.Stroke.t
    | Handrawn of int (* width *)

  let default = Standard (Draw.Stroke.create Color.black 2)

  let rect w h t = let open Frp.Behavior in match t with
    | Standard stroke ->
        Draw.rect ~width:(return w) ~height:(return h) (return (0., 0.))
          ~stroke:(return stroke) ~fill:(return Color.none)
    | _ -> failwith "Border_style.rect: Handrawn not implemented"
end

module Frame = struct
  type t =
    { width_proportion : float
    ; art              : Draw.t
    ; heading          : Heading.t option
    }

  let render row_height row_space border_style {width_proportion; art; heading} =
    let open Frp.Behavior in let open Draw in
    let frame_width = width_proportion *. row_space in
    pictures [|
      Border_style.rect frame_width row_height border_style;
      crop (return (0., 0.)) art
        ~width:(return frame_width)
        ~height:(return row_height)
    |]
end

module Row = struct
  type t =
    { frames : Frame.t array
    ; height : float
    }

  let render comic_width bar_width border_style {frames; height} =
    let bars            = Array.length frames + 1 in
    let available_space = comic_width -. float_of_int bars *. bar_width in
    Draw.pictures (
      Array.fold_map frames ~init:0. ~f:(fun left frame ->
        let w = frame.Frame.width_proportion *. available_space in
        ( left +. w +. bar_width
        , Draw.translate (Frame.render height available_space border_style frame)
            (Frp.Behavior.return (left, 0.)))))
end

module Spacing = struct
  type t =
    { width : float
    ; color : Color.t
    }

  let default = { width = 10.; color = Color.white }
end

type t = 
  { rows         : Row.t array
  ; spacing      : Spacing.t
  ; border_style : Border_style.t
  ; width        : float
  }

let render {rows; spacing; border_style} = spacing.color

let create
  ?(border_style=Border_style.default)
  ?(spacing=Spacing.default)
  rows
  =
  ()

(*
rows
  (100, [

*)
