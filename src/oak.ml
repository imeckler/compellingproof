open Core
include Oak_common

module Resource : sig
  type 'a t

  val with_many
    : string array
    -> (Image.t array -> 'a Frp.Stream.t)
    -> 'a Frp.Stream.t

  val (+>) : (Image.t -> 'a) t -> string -> 'a t

  val run : 'a Frp.Stream.t t -> 'a Frp.Stream.t
end = struct
  type 'a t = 'a Frp.Stream.t

  let with_many urls f =
    Array.map ~f:Image.load urls
    |> Frp.Stream.zip_many ~f
    |> Frp.Stream.join

  let (+>) t url = Frp.Stream.Infix.(t <*> Image.load url)

  let run = Frp.Stream.join
end

module Form = struct
  include Oak_graphics.Form

  let draw = Oak_graphics.Render_form.draw
end

