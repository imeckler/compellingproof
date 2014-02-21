open Core

let drawing, sub =
  let ctr_x =
    let open Animate.Sequence in
       quadratic 1000. ~final:100.
    >> stay_for 500.
    >> quadratic 1000. ~final:200.
    >> stay_forever
  in
  let open Draw in let open Point in let open Frp.Behavior in
  circle ~props:[|return (Property.fill Color.black)|] (return 10.)
    (Frp.Behavior.map (Animate.Sequence.run ~init:0. ctr_x) ~f:(fun x -> {y = 50.; x}))
  |> render


let (drawing, sub) =
  let open Draw in let open Frp.Behavior in let open Point in
  let side_len  = 80. in
  let hside_len = side_len /. 2. in
  let height    = sqrt 3. *. hside_len in
  let ctr = {x = 100.; y = height -. 0.5 *. (side_len /. sqrt 3.)} in

  let triangle =
    polygon 
      (return [|
        {x = 100.; y = 0.};
        {x = 100. +. hside_len; y = height};
        {x = 100. -. hside_len; y = height}
      |])
      ~props:[|
        return (Property.fill Color.white);
        return (Property.stroke Color.red 2)
      |]
  in
  let angle =
    let open Animate.Sequence in
    cycle (
         quadratic 1000. ~final:120.
      >> stay_for 500.
      >> quadratic 1000. ~final:240.
      >> stay_for 500.
      >> quadratic 1000. ~final:360.
      >> stay_for 500.
    )
  in

  transform triangle
    (map ~f:(fun a -> Transform.Rotate (a, ctr))
      (Animate.Sequence.run ~init:0. angle))
  |> render


let () =
  let svg = Jq.Dom.svg_node "svg" [|"width", "400"; "height", "400"|] in
  Jq.Dom.append svg drawing;
  match Jq.to_dom_node (Jq.jq "#content") with
    | None -> ()
    | Some t -> Jq.Dom.append t svg

