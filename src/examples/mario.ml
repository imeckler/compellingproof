open Core
open Oak

type mario =
  { x   : float
  ; y   : float
  ; vx  : float
  ; vy  : float
  ; dir : string
  }

let jump (_, y) m = if y > 0 && m.y = 0. then {m with vy = 9.} else m

let gravity t m = if m.y > 0. then {m with vy = m.vy -. t /. 3.} else m

let physics t ({x;y;vx;vy;_} as m) =
  {m with x = x +. t *. vx ; y = max 0. (y +. t *. vy)}

let walk (x, _) m =
  { m with vx = if m.y > 0. then m.vx else 4. *. float_of_int x
  ; dir = if x < 0 then "left" else if x > 0 then "right" else m.dir
  }

let step m (t, dir) = physics t (walk dir (gravity t (jump dir m)))

let fetch_img verb = Image.load (
  "http://elm-lang.org/imgs/mario/stand/" ^ verb ^ ".gif")

(* let right_img = fetch_img "right" *)

(* Sprite is 16 x 32 *)

let w, h = 600, 400
let w', h' = float_of_int w, float_of_int h

let render m = let open Form in
  let r = 20. in
  group [|
    Shape.rect w' h' |> filled (Color.of_rgb 174 238 238) |> move (w' /. 2., h' /. 2.);
    Shape.rect w' 25. |> filled (Color.of_rgb 0 255 0) |> move (w' /. 2., h' -. 12.5);
    Shape.circle r |> filled (Color.of_rgb 255 0 0) 
      |> move (m.x, m.y -. 25. -. r)
  |]

let div = 
  Dom_html.document##getElementById(Js.string "content")
  |> Js.Opt.to_option
  |> Option.value_exn

let input =
  let delta = Frp.Stream.(map ~f:(fun t -> Time.Span.to_ms t /. 20.) (deltas (1000. /. 30.))) in
  Frp.project_with Jq.arrows delta ~f:(fun x y -> (y, x))

let mario =
  Frp.scan input ~init:{x=0.;y=0.;vx=0.;vy=0.;dir="right"} ~f:step
  |> Frp.Behavior.map ~f:(fun m -> {m with y = h' -. m.y})

let () = begin
  set_global "mario_changes" (Frp.Behavior.changes mario);
  Form.draw ~width:w ~height:h div (Frp.Behavior.map ~f:render mario)
  |> ignore
end

