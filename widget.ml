open Core

module Control = struct
  type 'a t = Jq.t -> Jq.t -> 'a

  let slider label container _ =
    let c = Jq.create "div" in
    let b = Frp.Behavior.return 0.0 in
    let update e ui =
      Frp.Behavior.trigger b (Js.to_float (Js.Unsafe.get ui (Js.string "value")))
    in
    Jq.append container c;
    set_global "c" c;
    Js.Unsafe.(meth_call c "slider" [|
      inject [| obj [|"slide", inject (Js.wrap_callback update) |] |]
    |]) |> ignore;
    b

  let clicks _ canvas = Jq.clicks canvas
  let drags _ canvas  = Jq.drags canvas

  let drag_point init : (int * int) Frp.Behavior.t t = fun _ canvas ->
    let open Jq.Event.Mouse.Drag in
    Frp.scan (Jq.drags canvas) ~init
      ~f:(fun (x, y) (dx, dy) -> (x + dx,  y + dy))

  let drag_angle (cx, cy) =
    let pi = acos (-1.) in
    let angle_of_pos =
      let cxf, cyf = float_of_int cx, float_of_int cy in
      fun (x, y) ->
        let a = atan ((cyf -. float_of_int y) /. (cxf -. float_of_int x)) in
        if x < cx then pi -. a else a
    in
    fun container canvas ->
      Frp.Behavior.map (drag_point (cx + 100, cy) container canvas) 
        ~f:angle_of_pos
end

type 'a t = Jq.t * Jq.t * 'a

let (+>) ((container, canvas, f) : ('a -> 'b) t) w =
  (container, canvas, f (w container canvas))

let create ~width ~height container f = 
  let canvas = 
    Jq.Dom.svg_node "svg" [|"width", string_of_int width; "height", string_of_int height|]
    |> Jq.wrap
  in
  Jq.append container canvas;
  (container, canvas, f)

let run (_, canvas, drawing) =
  let (drawing_elt, sub) = Draw.render drawing in
  Jq.append canvas (Jq.wrap drawing_elt);
  sub

