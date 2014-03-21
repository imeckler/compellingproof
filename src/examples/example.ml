let square w h =
  let open Draw in
  let side_len = min w h /. 2. in
  let ctr      = (w /. 2., h /. 2. ) in
  let square   = let open Frp.Behavior in let open Property in
    rect 
      ~props:[|return (fill Color.black)|]
      ~width:(return side_len) ~height:(return side_len)
      (return ((w -. side_len) /. 2., (h -. side_len) /. 2.))
  in
  let angle = let open Animate.Sequence in
    cycle (quadratic 2000. ~final:360.)
    |> run ~init:0. |> Frp.Behavior.map ~f:Angle.of_degrees
  in
  transform square (Frp.Behavior.map ~f:(fun a -> Transform.Rotate (a, ctr)) angle)

let () =
  let svg = Jq.Dom.svg_node "svg" [| "width", "400"; "height", "600" |] in
  Jq.Dom.append svg (fst (Draw.render (square 400. 600.)));
  match Option.bind (Jq.jq "#content") Jq.to_dom_node with
    | Some t -> Jq.Dom.append t svg
    | None   -> ()

