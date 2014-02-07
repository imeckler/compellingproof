type t

module Event : sig
  module Mouse : sig
    module Button : sig
      type t = [ `Left | `Middle | `Right ]
    end

    module Click : sig
      type t =
        { pos    : int * int
        ; button : Button.t
        }
    end

    module Drag : sig
      type t =
        { change : int * int
        ; button : Button.t
        }
    end
  end

  module Key : sig
    type t
  end
end

val create : string -> t

val jq : string -> t

val append : t -> t -> unit

val empty : t -> unit

val width : t -> int

val height : t -> int

val set_attr : t -> name:string -> value:string -> unit

val sink_attr : t -> name:string -> value:string Frp.Behavior.t -> Frp.Subscription.t

val css : t -> (string * string) array -> unit

val on : t -> string -> (Dom_html.event Js.t -> unit) -> unit

val clicks : t -> Event.Mouse.Click.t Frp.Stream.t

val drags : t -> Event.Mouse.Drag.t Frp.Stream.t
