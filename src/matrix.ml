type t = float * float * float * float

let mul (a, b, c, d) (w, x, y, z) =
  (a *. w +. b *. y, a *. x +. b *. z
  ,c *. w +. d *. y, c *. x +. d *. z)

let apply (a, b, c, d) (x, y) = (a *. x +. b *. y, c *. x +. d *. y)

let identity = (1., 0., 0., 1.)

module Infix = struct
  let ( * ) = mul
end

