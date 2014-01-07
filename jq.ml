open Core

type t

let jq s =
  Js.Unsafe.fun_call (Js.Unsafe.variable "$") [| Js.Unsafe.inject (Js.string s) |]

let create tag = jq ("<" ^ tag ^ ">")

let append parent child =
  Js.Unsafe.meth_call parent "append" [| Js.Unsafe.inject child |]

let css t ps = 
  let open Js.Unsafe in
  let ps' = obj (Array.map ps ~f:(fun (p, x) -> p, inject x)) in
  meth_call t "css" [| inject ps' |]

let on t event_name (f : Js.Dom_html.event -> unit) =
  Js.Unsafe.(meth_call t "on" [| inject event_name; inject (Js.wrap_callback f) |]);

let clicks t =
  (* TODO: Consider adding a variant for uninitialized streams to prevent shit like this *)
  let s = Frp.Stream.create (0, 0) in
  on t "click" (fun e ->
    Frp.Stream.trigger s Js.Unsafe.(get e (Js.string "offsetX"), get e (Js.string "offsetY"))
  );
  s
;;

let drags t = failwith "unimplemented" (* TODO *)

(* (Array.map ~f:(fun (p, x) -> (p, inject x)) ps) *)
