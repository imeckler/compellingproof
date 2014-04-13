open Core

type t

let unsafe_jq s = Js.Unsafe.(fun_call (variable "jQuery") [| inject (Js.string s) |])

let jq s =
  let t = unsafe_jq s in
  match (Obj.magic t)##length with
  | 0 -> None
  | _ -> Some t

let wrap elt =
  Js.Unsafe.(fun_call (variable "jQuery") [| inject elt |])

let create tag = unsafe_jq ("<" ^ tag ^ ">")

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

let set_attr t ~name ~value : unit =
  Js.Unsafe.(meth_call t "attr" [| inject (Js.string name); inject (Js.string value) |])

let sink_attr t ~name ~value =
  set_attr t ~name ~value:(Frp.Behavior.peek value);
  Frp.Stream.iter (Frp.Behavior.changes value) ~f:(fun value ->
    set_attr t ~name ~value
  )

let to_dom_node t =
  Js.Optdef.to_option (Js.Unsafe.(meth_call t "get" [| inject 0 |]))

let to_dom_node_exn t =
  match to_dom_node t with
  | None   -> failwith "Jq.to_dom_node_exn: Empty object"
  | Some x -> x

module Dom = struct
  type t = Dom_html.element Js.t

  let set_attr t ~name ~value =
    t##setAttribute(Js.string name, Js.string value)

  let sink_attr t ~name ~value =
    set_attr t ~name ~value:(Frp.Behavior.peek value);
    let name = Js.string name in
    Frp.Stream.iter (Frp.Behavior.changes value) ~f:(fun value ->
      t##setAttribute(name, Js.string value)
    )

  let set_html (t : t) s = t##innerHTML <- Js.string s

  let sink_html t sb =
    set_html t (Frp.Behavior.peek sb);
    Frp.Stream.iter (Frp.Behavior.changes sb) ~f:(set_html t)

  let append t c = Dom.appendChild t c

  let empty (t : t) =
    while Js.Opt.test (t##firstChild) do
      Js.Opt.iter (t##firstChild) (fun x -> ignore (t##removeChild(x)))
    done

  let svg_node tag attrs : t =
    let str s = Js.Unsafe.inject (Js.string s) in
    let elt = let open Js.Unsafe in
      fun_call (variable "document.createElementNS") [| 
        str "http://www.w3.org/2000/svg"; str tag
      |]
    in
    Array.iter attrs ~f:(fun (k, v) ->
      elt##setAttribute(Js.string k, Js.string v)
    );
    elt
  ;;
end

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

    let of_code t = t

    let to_code t = t
  end
end

let body = unsafe_jq "body"

let keys =
  let elt          = to_dom_node_exn body in
  let pressed      = Inttbl.create () in
  let b            = Frp.Behavior.return [||] in
  let key_down evt =
    Inttbl.add pressed ~key:(evt##keyCode) ~data:();
    Frp.Behavior.trigger b (Inttbl.keys pressed);
    Js._true
  in
  let key_up evt =
    Inttbl.remove pressed (evt##keyCode);
    Frp.Behavior.trigger b (Inttbl.keys pressed);
    Js._true
  in
  elt##onkeydown <- Dom_html.handler key_down;
  elt##onkeyup   <- Dom_html.handler key_up;
  b

let mouse_pos =
  let s = Frp.Stream.create () in
  on body "mousemove" (fun e ->
    let pos = Js.Unsafe.(get e (Js.string "pageX"), get e (Js.string "pageY")) in
    Frp.Stream.trigger s pos 
  );
  s

let mouse_movements = 
  Frp.Stream.delta mouse_pos ~f:(fun (x0, y0) (x1, y1) -> (x1 - x0, y1 - y0))

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

let dragged t =
  let b = Frp.Behavior.return false in
  on t    "mousedown" (fun _ -> Frp.Behavior.trigger b true);
  on body "mouseup"   (fun _ -> Frp.Behavior.trigger b false);
  b

let drags t = Frp.when_ (dragged t) mouse_movements

(* let drags_with button t = Frp.Stream.filter (drags t) ~f:(fun b -> b = button) *)

(* (Array.map ~f:(fun (p, x) -> (p, inject x)) ps) *)
