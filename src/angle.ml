type t = float (* Internally we use degrees *)

let pi = 4. *. atan 1.

let to_degrees x = x

let to_radians = let c = (2. *. pi) /. 360. in fun x -> c *. x

let of_degrees x = x

let of_radians = let c = 360. /. (2. *. pi) in fun x -> c *. x

let rotate ~about:(a, b) (x, y) angle =
  let angle = to_radians angle in
  let x', y' = x -. a, y -. b in
  let x'' = (x' *. cos angle) -. (y' *. sin angle) in
  let y'' = (x' *. sin angle) +. (y' *. cos angle) in
  (x'' +. a, y'' +. b)

let about ~center:(cx, cy) (x, y) =
  let a = of_radians (atan2 (cy -. y)  (x -. cx)) in
  if a < 0. then 360. +. a else a

let cos x = cos (to_radians x)

let sin x = sin (to_radians x)

let acos x = of_radians (acos x)

let asin x = of_radians (asin x)

let atan x = of_radians (atan x)

let (+) = (+.)
let (-) = (-.)
let ( * ) = ( *. )

