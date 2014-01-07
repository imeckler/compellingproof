module Control = struct

  (*
  type _ t =
    | Slider : string -> float t
    | Clicks : (int * int) option t
    | Drags  : (int * int) option t
  *)
  type 'a t = Jq.t -> Jq.t -> 'a Frp.Stream.t

  let map t ~f = fun canvas container ->
    Frp.Stream.map (t canvas container) ~f

  let ap t1 t2 = fun canvas container ->
    Frp.Stream.ap (t1 canvas container) (t2 canvas container)

  let briefly s =
    let s' = Frp.Stream.create None in
    Frp.Stream.iter s ~f:(fun x ->
      Frp.Stream.trigger s' (Some x);
      Frp.Stream.set s' None
    );
    s'
  ;;

  let clicks _ canvas = briefly (Jq.clicks canvas)

  let drags _ canvas = briefly (Jq.drags canvas)

  let slider label container _ =
    let c = Jq.create "div" in
    let s = Frp.Stream.create 0.0 in
    let update_stream e ui =
      Frp.Stream.trigger s (Js.to_float (Js.Unsafe.get ui (Js.string "value")))
    in

    Jq.append container c;
    Js.Unsafe.(meth_call c "slider" [|
      inject [| obj [|"slide", inject (Js.wrap_callback update_stream) |] |]
    |]);
    s
  ;;
end

type 'a t = Jq.t * Jq.t * 'a Frp.Stream.t

let (+>) (container, canvas, tf) w =
  let open Frp.Stream.Infix in
  (container, canvas, tf <*> w container canvas)

(*
let (+>) : type a. (a -> 'b) t -> a Control.t -> 'b t =
  let open Control in let open Frp.Stream.Infix in
  fun (container, canvas, tf) w -> match w with
  | Slider label -> (container, canvas, tf <*> slider container)
  | Clicks       -> (container, canvas, tf <*> clicks canvas)
  | Drags        -> (container, canvas, tf <*> drags canvas)
;;
*)

let create container canvas f = (container, canvas, Frp.Stream.return f)

module Slidy = struct

  type 'a t = Jq.t * Jq.t * 'a Frp.Stream.t

  let (+>) ((container, canvas, tf) : (float -> 'b) t) label : 'b t =
    let c = Jq.create "div" in
    let s = Frp.Stream.create 0.0 in
    let update_stream e ui =
      Frp.Stream.trigger s (Js.to_float (Js.Unsafe.get ui (Js.string "value")))
    in

    Jq.append container c;
    Js.Unsafe.(meth_call c "slider" [|
      inject [| obj [|"slide", inject (Js.wrap_callback update_stream) |] |]
    |]);

    Frp.Stream.Infix.(container, canvas, tf <*> s)
  ;;

  let sliders container canvas f = (container, canvas, Frp.Stream.return f)

end
