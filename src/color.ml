type t = { r : int; g : int; b : int; alpha : float }

let of_rgb ?(alpha=1.0) r g b = {r; g; b; alpha}

let random () = 
  { r = Random.int 256; g = Random.int 256; b = Random.int 256; alpha = 1.0 }

let render {r; g; b; alpha} =
  Printf.sprintf "rgba(%d,%d,%d,%f)" r g b alpha

let white = { r = 255; g = 255; b = 255; alpha = 1.0 }
let black = { r = 0; g = 0; b = 0; alpha = 1.0 }
let red   = { r = 255; g = 0; b = 0; alpha = 1.0 }
let green = { r = 0; g = 255; b = 0; alpha = 1.0 }
let blue  = { r = 0; g = 0; b = 255; alpha = 1.0 }
let none  = { blue with alpha = 0. }
