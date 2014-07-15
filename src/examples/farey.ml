open Core
open Oak

let scale_factor = 200.

let width, height = 1000, 300
let width', height' = float_of_int width, float_of_int height

let farey_circle x = let open Form in
  let q  = float_of_int (Ratio.denominator x) in
  let r  = scale_factor /. (2. *. q *. q) in
  move (scale_factor *. Ratio.to_float x, height' -. r) (outlined Line_style.default (Shape.circle r))

module Array_version = struct
  let circles_in_range denom (lo, hi) =
    let denom'             = float_of_int denom in
    let least_numerator    = int_of_float (ceil (lo *. denom')) in
    let greatest_numerator = int_of_float (floor (hi *. denom')) in
    let n                  = greatest_numerator - least_numerator + 1 in
    if n <= 0 then [||]
    else Array.init (greatest_numerator - least_numerator + 1) ~f:(fun i ->
      farey_circle ((i + least_numerator) % denom))

  let farey range max_denom =
    Form.group (`Array (
      (Array.concat (List.init (max_denom - 1) ~f:(fun i ->
        circles_in_range (i + 2) range)))))
end

module Iterator_version = struct
  let circles_in_range denom (lo, hi) =
    let denom'             = float_of_int denom in
    let least_numerator    = int_of_float (ceil (lo *. denom')) in
    let greatest_numerator = int_of_float (floor (hi *. denom')) in
    let n                  = greatest_numerator - least_numerator + 1 in
    if n <= 0 then Iterator.create 0 ~f:(fun _ -> failwith "undefined")
    else Iterator.create (greatest_numerator - least_numerator + 1) ~f:(fun i ->
      farey_circle ((i + least_numerator) % denom))

  let farey range max_denom =
    Form.group (`Iterator (
      Iterator.for_ (max_denom - 1) ~f:(fun i ->
        circles_in_range (i + 2) range)))
end

open Iterator_version

let () = begin
  println "hola";
  let div = Dom_html.document##getElementById(Js.string "content")
    |> Js.Opt.to_option |> Option.value_exn
  in
  let t0    = Time.now () in
  let scale_pan = 
    Jq.relative_mouse_pos (Jq.wrap div)
    |> Frp.Stream.map ~f:(fun (x, y) ->
        let scale =
          let d = height' -. if y = 0 then 0.000001 else float_of_int y in 
          if d < 0. then 5. else min 5. (height' /. d)
        in
        (scale, -. (float_of_int x) *. scale))
    |> Frp.latest ~init:(0., 1.)
  in
  Form.draw ~width ~height div (Frp.Behavior.map scale_pan ~f:(fun (s, x) ->
    let depth = 8 * int_of_float s + 1 in
    Form.move (x, height' -. s *. height') (
      Form.scale s (farey (0., 8. /. s) depth) )))
  |> ignore
end

