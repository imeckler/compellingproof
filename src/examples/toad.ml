open Core
open Oak

type dir = Left  | Right

type toad =
  { x   : float
  ; y   : float
  ; vx  : float
  ; vy  : float
  ; dir : dir
  }

type action = Stand | Walk1 | Walk2 | Jump
type pose   = action * dir

let jump (_, y) m = if y > 0 && m.y = 0. then {m with vy = 9.} else m

let gravity t m = if m.y > 0. then {m with vy = m.vy -. t /. 3.} else m

let physics t ({x;y;vx;vy;_} as m) =
  {m with x = x +. t *. vx ; y = max 0. (y +. t *. vy)}

let walk (x, _) m =
  { m with vx = if m.y > 0. then m.vx else 4. *. float_of_int x
  ; dir = if x < 0 then Left else if x > 0 then Right else m.dir
  }

let step m (t, dir) = physics t (walk dir (gravity t (jump dir m)))

(* Sprite is 16 x 32 *)

let w, h = 600, 400
let w', h' = float_of_int w, float_of_int h

let sprite_offset (action, dir) =
  let x = match action with
    | Stand -> 0 | Walk1 -> 16 | Walk2 -> 32 | Jump -> 48 in
  let y = match dir with Left -> 0 | Right -> 32 in
  (x, y)

let pose m = ((if m.y > 0. then Jump else if m.vx = 0. then Stand else Walk1), m.dir)

let render toad_image m = let open Form in
  let r = 20. in
  group [|
    Shape.rect w' h'
      |> filled (Color.of_rgb 174 238 238)
      |> move (w' /. 2., h' /. 2.);
    Shape.rect w' 25.
      |> filled (Color.of_rgb 0 255 0)
      |> move (w' /. 2., h' -. 12.5);
    sprite 16 32 (sprite_offset (pose m)) toad_image
      |> move (m.x, m.y -. 25. -. r)
  |]

let div = 
  Dom_html.document##getElementById(Js.string "content")
  |> Js.Opt.to_option
  |> Option.value_exn

let input =
  let delta = Frp.Stream.(map ~f:(fun t -> Time.Span.to_ms t /. 20.) (deltas (1000. /. 30.))) in
  Frp.project_with Jq.arrows delta ~f:(fun x y -> (y, x))

let init_toad = 
  {x=0.;y=0.;vx=0.;vy=0.;dir=Right}

let toad =
  Frp.scan input ~init:init_toad ~f:step
  |> Frp.Behavior.map ~f:(fun m -> {m with y = h' -. m.y})

let drawing =
  Image.load "toad_sprite.png"
  |> Frp.Stream.map ~f:(fun toad_image ->
      println "it loaded";
      Frp.Stream.map (Frp.Behavior.changes toad) ~f:(render toad_image))
  |> Frp.Stream.switch
  |> Frp.latest ~init:(Form.group [||])

let () = Frp.Stream.iter ~f:print (Frp.Behavior.changes drawing) |> ignore

let () =
  set_global "f" (Js.wrap_callback (fun () -> Frp.Behavior.peek drawing))

let () =
  set_global "g" (Js.wrap_callback (fun () -> Frp.Behavior.peek toad))

let () =
  let toad_image = Dom_html.(createImg document) in
  toad_image##src <- Js.string "toad_sprite.png";
  Frp.Behavior.map ~f:(render (Obj.magic toad_image)) toad
  |> Form.draw ~width:w ~height:h div
  |> ignore


(*
 This prints it loaded and does nothing else
let drawing =
  Image.load "toad_sprite.png"
  |> Frp.Stream.map ~f:(fun toad_image ->
      println "it loaded";
      Frp.Stream.map (Frp.Behavior.changes toad) ~f:(render toad_image))
  |> Frp.Stream.switch
  |> Frp.latest ~init:(Form.group [||])
  |> Frp.Behavior.changes
  |> Frp.Stream.iter ~f:print
  |> ignore

*)
(*
let () =
  Image.load "toad_sprite.png"
  |> Frp.Stream.map ~f:(fun toad_image ->
      println "it loaded";
      Frp.Stream.map (Frp.Behavior.changes toad) ~f:(render toad_image))
  |> Frp.Stream.switch
  |> Frp.latest ~init:(Form.group [||])
  |> (fun x -> Frp.Stream.iter (Frp.Behavior.changes x) ~f:print |> ignore; x)
  |> Form.draw ~width:w ~height:h div
  |> ignore

*)
