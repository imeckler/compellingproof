open Core

type t

let jq s =
  Js.Unsafe.fun_call (Js.Unsafe.variable "jQuery") [| Js.Unsafe.inject (Js.string s) |]

let create tag = jq ("<" ^ tag ^ ">")

let append parent child =
  Js.Unsafe.meth_call parent "append" [| Js.Unsafe.inject child |]

let empty t =
  Js.Unsafe.meth_call t "empty" [||]

let width t = Js.Unsafe.meth_call t "width" [||]

let height t = Js.Unsafe.meth_call t "height" [||]

let css t ps = 
  let open Js.Unsafe in
  let ps' = obj (Array.map ps ~f:(fun (p, x) -> p, inject x)) in
  meth_call t "css" [| inject ps' |]

let on t event_name (f : Dom_html.event Js.t -> unit) : unit =
  Js.Unsafe.(meth_call t "on" [| inject (Js.string event_name); inject (Js.wrap_callback f) |])

let set_attr t ~name ~value =
  Js.Unsafe.(meth_call t "attr" [| inject (Js.string name); inject (Js.string value) |])

let sink_attr t ~name ~value =
  Frp.Stream.iter (Frp.Behavior.changes value) ~f:(fun value ->
    set_attr t ~name ~value
  )

module Event = struct
  module Mouse = struct
    module Button = struct
      type t = [ `Left | `Middle | `Right ]

      let from_code = function
        | 1 -> `Left
        | 2 -> `Middle
        | 3 -> `Right
        | x -> failwith ("Not a valid mouse code: " ^ string_of_int x)
    end

    module Click = struct
      type t =
        { pos    : int * int
        ; button : Button.t
        }
    end

    module Drag = struct
      type t =
        { change : int * int
        ; button : Button.t
        }
    end

  end

  module Key = struct
    type t = int
  end
end

let clicks t =
  (* TODO: Consider adding a variant for uninitialized streams to prevent stuff like this *)
  let s = Frp.Stream.create () in
  on t "click" (fun e ->
    let pos = Js.Unsafe.(get e (Js.string "offsetX"), get e (Js.string "offsetY")) in
    let button = Event.Mouse.Button.from_code Js.Unsafe.(get e (Js.string "which")) in
    Frp.Stream.trigger s { Event.Mouse.Click.pos ; button }
  );
  s
;;

let clicks_with button t = Frp.Stream.filter (clicks t) ~f:(fun b -> b = button)

let drags t =
  let s = Frp.Stream.create () in
  let on_drag e d =
    let open Js.Unsafe in
    Frp.Stream.trigger s
      { Event.Mouse.Drag.change = (get d "deltaX", get d "deltaY")
      ; button                  = Event.Mouse.Button.from_code (get e "which")
      }
  in
  Js.Unsafe.(
    meth_call t "on" [| inject "drag"; inject (Js.wrap_callback on_drag) |]
  );
  s
;;

let drags_with button t = Frp.Stream.filter (drags t) ~f:(fun b -> b = button)

(* (Array.map ~f:(fun (p, x) -> (p, inject x)) ps) *)
