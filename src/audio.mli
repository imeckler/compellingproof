class type audio = object
  method play     : unit Js.meth
  method pause    : unit Js.meth
  method fastSeek : Js.number Js.t -> unit Js.meth

  method volume       : Js.number Js.t Js.prop
  method loop         : bool Js.t Js.prop
  method playbackRate : Js.number Js.t Js.prop
end

type t = audio Js.t

val create : string -> t

val on_ended : t -> f:(unit -> unit) -> Dom.event_listener_id

