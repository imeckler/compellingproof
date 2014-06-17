type t = float * float

let scale c (x, y) = (c *. x, c *. y)

let add (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)

let sub (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)

let norm (x, y) = sqrt (x ** 2. +. y ** 2.)

let normed v = scale (1. /. norm v) v
