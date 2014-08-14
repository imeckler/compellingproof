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

let to_ocaml_array t =
  Js.(to_array Unsafe.(meth_call t "toArray" [||]))

let children t =
  to_ocaml_array (Js.Unsafe.meth_call t "children" [||])

let find = jq

let find_descendants t selector =
  Array.map ~f:wrap
    (to_ocaml_array (Js.Unsafe.(meth_call t "find" [|inject (Js.string selector)|])))

let append parent child =
  Js.Unsafe.(meth_call parent "append" [|inject child|])

let insert_after t1 t2 =
  Js.Unsafe.(meth_call t1 "after" [|inject t2|])

let insert_before t1 t2 =
  Js.Unsafe.(meth_call t1 "before" [|inject t2|])

let empty t =
  Js.Unsafe.meth_call t "empty" [||]

let width t = Js.Unsafe.meth_call t "width" [||]

let height t = Js.Unsafe.meth_call t "height" [||]

let offset t =
  let o = Js.Unsafe.meth_call t "offset" [||] in
  Js.Unsafe.(
    get o (Js.string "left"), get o (Js.string "top"))

let css t ps = 
  let open Js.Unsafe in
  let ps' = obj (Array.map ps ~f:(fun (p, x) -> p, inject x)) in
  meth_call t "css" [| inject ps' |]

module Event = struct
  type removal_token = Js.js_string Js.t * (Dom_html.event Js.t -> unit) Js.callback

  let on t event_name f : removal_token =
    let callback = Js.wrap_callback f in
    let js_event_name = Js.string event_name in
    Js.Unsafe.(meth_call t "on" [|inject js_event_name; inject callback|]);
    (js_event_name, callback)

  let off t (js_event_name, callback) = let open Js.Unsafe in
  meth_call t "off" [|inject js_event_name; inject callback|]
end

let set_attr t ~name ~value : unit =
  Js.Unsafe.(meth_call t "attr" [| inject (Js.string name); inject (Js.string value) |])

let stop_on_removal t sub =
  ignore (Event.on t "removal" (fun _ -> Frp.Subscription.cancel sub));
  sub

let sink_attr t ~name ~value =
  set_attr t ~name ~value:(Frp.Behavior.peek value);
  Frp.Stream.iter (Frp.Behavior.changes value) ~f:(fun value ->
    set_attr t ~name ~value
  ) |> stop_on_removal t

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
    ) |> stop_on_removal (wrap t)

  let set_html (t : t) s = t##innerHTML <- Js.string s

  let sink_html t sb =
    set_html t (Frp.Behavior.peek sb);
    Frp.Stream.iter (Frp.Behavior.changes sb) ~f:(set_html t)
    |> stop_on_removal (wrap t)

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

let body : t = unsafe_jq "body"

(* TODO: Oh god what do I name this *)

let setup_event_handlers t hs =
  let hs' = Array.map hs ~f:(fun (event_name, handler) ->
    (event_name, Js.wrap_callback handler))
  in
  Array.iter hs' ~f:(fun (event_name, wrapped_handler) ->
    on_wrapped t event_name wrapped_handler);
  fun () -> Array.iter hs' ~f:(fun (event_name, wrapped_handler) ->
    off_wrapped t event_name wrapped_handler)

let key_stream =
  Frp.Stream.create ~start:(fun trigger ->
    let which e      = Js.Unsafe.(get e (Js.string "which")) in
    setup_event_handlers body [|
      "keydown", (fun e -> e##preventDefault(); trigger (`Down (which e)));
      "keyup"  , (fun e -> e##preventDefault(); trigger (`Up (which e)));
    |])
  ()

let () = set_global "key_stream" key_stream

let keys =
  let pressed = Inttbl.create () in
  Frp.scan ~init:[||] key_stream ~f:(fun _ k -> 
    begin match k with
      | `Down n -> Inttbl.add pressed ~key:n ~data:()
      | `Up n -> Inttbl.remove pressed n
    end;
    Inttbl.keys pressed)

(*
let arrows = 
  let bool_to_int = function
    | true  -> 1
    | false -> 0
  in
  Frp.Behavior.map keys ~f:(fun ks -> let open Event.Key in
  ( bool_to_int (Array.mem ks (of_code 39)) - bool_to_int (Array.mem ks (of_code 37))
  , bool_to_int (Array.mem ks (of_code 38)) - bool_to_int (Array.mem ks (of_code 40))))
*)

let dragged t =
  Frp.Stream.create ~start:(fun trigger ->
    let stop_down_handler =
      setup_event_handlers t [|"mousedown", fun _ -> trigger true|]
    in
    let stop_up_handler =
      setup_event_handlers body [|"mouseup", fun _ -> trigger false|]
    in
    fun () -> stop_down_handler (); stop_up_handler ()) ()
  |> Frp.latest ~init:false

let drags t =
  Frp.when_ (dragged t) mouse_movements

(* let drags_with button t = Frp.Stream.filter (drags t) ~f:(fun b -> b = button) *)

(* (Array.map ~f:(fun (p, x) -> (p, inject x)) ps) *)
