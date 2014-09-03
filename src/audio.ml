class type audio = object
  method play     : unit Js.meth
  method pause    : unit Js.meth
  method fastSeek : Js.number Js.t -> unit Js.meth

  method volume       : Js.number Js.t Js.prop
  method loop         : bool Js.t Js.prop
  method playbackRate : Js.number Js.t Js.prop
end

type t = audio Js.t

let audio : (Js.js_string Js.t -> audio Js.t) Js.constr = Js.Unsafe.global##_Audio

let create url = jsnew audio (Js.string url)

let add_listener ty t f = Dom.addEventListener t (Dom.Event.make ty)
  (Dom.handler (fun _ -> f (); Js._false)) Js._false

let on_ended (t : t) ~f = add_listener "ended" t f
