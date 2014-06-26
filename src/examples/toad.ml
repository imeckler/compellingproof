open Core
open Oak

type dir = Left | Right

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

let walk_anim = let open Animate.Sequence in
  cycle (for_ 0.5 (fun _ _ -> Walk1) >> for_ 0.5 (fun _ _ -> Walk2))
  |> run ~init:Walk1

let sprite_offset (action, dir) =
  let x = match action with
    | Stand -> 0 | Walk1 -> 16 | Walk2 -> 32 | Jump -> 48 in
  let y = match dir with Left -> 0 | Right -> 32 in
  (x, y)

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
(*   |> Frp.Behavior.map ~f:(fun m -> {m with }) *)

let action_controller = 
  Frp.Behavior.map toad ~f:(fun m ->
    if m.y <> 0. then `Jump else if m.vx = 0. then `Stand else `Walk)
  |> Frp.Behavior.skip_duplicates

let () = Frp.Stream.iter (Frp.Behavior.changes action_controller) ~f:(fun _ ->
  println "eyo") |> ignore

let actions = 
  action_controller
  |> Frp.Behavior.map ~f:(function
    | `Jump  -> Frp.Behavior.return Jump
    | `Stand -> Frp.Behavior.return Stand
    | `Walk  -> walk_anim)
  |> Frp.Behavior.join

let trace_b b to_str = Frp.Stream.iter (Frp.Behavior.changes b) ~f:(fun x -> println (to_str x))
  |> ignore

let poses = Frp.Behavior.zip_with toad actions ~f:(fun t a -> (a, t.dir))

let string_of_action = function Stand -> "Stand" | Walk1 -> "Walk1" | Walk2 -> "Walk2" | Jump -> "Jump"

let drawing toad_image = Frp.Behavior.zip_with toad actions ~f:(fun t a -> let open Form in
  group (`Array [|
    Shape.rect w' h'
      |> filled (Color.of_rgb 174 238 238)
      |> move (w' /. 2., h' /. 2.);
    Shape.rect w' 25.
      |> filled (Color.of_rgb 0 255 0)
      |> move (w' /. 2., h' -. 12.5);
    sprite 16 32 (sprite_offset (a, t.dir)) toad_image
      |> move (t.x, h' -. t.y -. 32. -. 25.)
  |]))

(* This prints nothing
let () =
  let drawing =
    Image.load "toad_sprite.png"
    |> Frp.Stream.map ~f:(fun toad_image ->
        println "it loaded";
        Frp.Stream.map (Frp.Behavior.changes toad) ~f:(render toad_image))
    |> Frp.Stream.switch
    |> Frp.latest ~init:(Form.group [||])
  in
  Frp.Stream.iter ~f:print (Frp.Behavior.changes drawing) |> ignore
 *)
let () =
  let toad_image = Dom_html.(createImg document) in
  toad_image##src <- Js.string "toad_sprite.png";
  Form.draw ~width:w ~height:h div (drawing toad_image)
  |> ignore

(*
 This prints "it loaded" and does nothing else
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

