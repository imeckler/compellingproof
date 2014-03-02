This is an FRP-based animation library for js_of_ocaml built using my
[FRP library](https://github.com/imeckle/ocamlfrp). The basic idea is
to make animations by describing drawings with time varying properties.

For example, to draw a square which continuously rotates you can write

```
let square w h =
  let open Draw in let open Point in
  let side_len = min w h /. 2. in
  let ctr      = {x = w /. 2.; y = h /. 2. } in
  let square   = let open Frp.Behavior in let open Property in
    rect 
      ~props:[|return (fill Color.black)|]
      ~width:(return side_len) ~height:(return side_len)
      (return {x = (w -. side_len) /. 2.; y = (h -. side_len) /. 2.})
  in
  let angle = let open Animate.Sequence in
    cycle (quadratic 2000. ~final:360.)
    |> run ~init:0. |> Frp.Behavior.map ~f:Angle.of_degrees
  in
  transform square (Frp.Behavior.map ~f:(fun a -> Transform.Rotate (a, ctr)) angle)
```
You can see the result of this example [here](http://jsfiddle.net/LJw6m/).
