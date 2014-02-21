module Control = struct

  module When = struct
    type 'a t = [ `Fresh of 'a | `Stale of 'a ]

    let map t ~f = match t with
      | `Stale x -> `Stale (f x)
      | `Fresh x -> `Fresh (f x)
  end

  type 'a t = Jq.t -> Jq.t -> 'a Frp.Behavior.t

  let map t ~f = fun canvas container ->
    Frp.Behavior.map (t canvas container) ~f

  (*
  let fold t ~init ~f = fun canvas container ->
    Frp.Stream.fold (t canvas container) ~init ~f
    *)

  let ap t1 t2 = fun canvas container ->
    Frp.Behavior.ap (t1 canvas container) (t2 canvas container)

  module Infix = struct
    let (>>|) t f = map t ~f

    let (<$>) f t = map t ~f

    let (<*>) = ap
  end

  let briefly s =
    let b = Frp.Behavior.return None in
    Frp.Stream.iter s ~f:(fun x ->
      let open Frp.Behavior in
      set b (Some x);
      notify_listeners b;
      set b None
    ) |> ignore;
    b
  ;;

  let dated_b b =
    let b' = Frp.Behavior.return (`Stale (Frp.Behavior.peek b)) in
    Frp.Stream.iter (Frp.Behavior.changes b) ~f:(fun x ->
      let open Frp.Behavior in
      trigger b' (`Fresh x);
      set b' (`Stale x)
    ) |> ignore;
    b'
  ;;

  let dated s =
    let b = Frp.Behavior.return (`Stale (Obj.magic ())) in
    Frp.Stream.iter s ~f:(fun x ->
      let open Frp.Behavior in
      set b (`Fresh x);
      notify_listeners b;
      set b (`Stale x);
    ) |> ignore;
    b
  ;;

  let clicks _ canvas = dated (Jq.clicks canvas)

  let drags _ canvas = dated (Jq.drags canvas)

  let drag_point init : 'a t = fun _ canvas ->
    let open Jq.Event.Mouse.Drag in
    Frp.scan (Jq.drags canvas) ~init
      ~f:(fun (x, y) { change = (dx, dy); _ } -> (x + dx, y + dy))
    |> dated_b
  ;;

  let drag_angle (cx, cy) =
    let pi = acos (-1.) in
    let angle_of_pos =
      let cxf, cyf = float_of_int cx, float_of_int cy in
      fun (x, y) ->
        let a = atan ((cyf -. float_of_int y) /. (cxf -. float_of_int x)) in
        if x < cx then pi -. a else a
    in
    map (drag_point (cx + 100, cy)) ~f:(When.map ~f:angle_of_pos)
  ;;

  let slider label container _ =
    let c = Jq.create "div" in
    let b = Frp.Behavior.return 0.0 in
    let update e ui =
      Frp.Behavior.trigger b (Js.to_float (Js.Unsafe.get ui (Js.string "value")))
    in

    Jq.append container c;
    Js.Unsafe.(meth_call c "slider" [|
      inject [| obj [|"slide", inject (Js.wrap_callback update) |] |]
    |]) |> ignore;
    dated_b b
  ;;

  let ticks ms _ _ = 
    let b = Frp.Behavior.return (`Stale (Time.now ())) in
    Frp.Stream.iter (Frp.Stream.ticks ms) ~f:(fun time ->
      Frp.Behavior.trigger b (`Fresh time);
      Frp.Behavior.set b (`Stale time)
    ) |> ignore;
    b
end

type 'a t = Jq.t * Jq.t * 'a Frp.Behavior.t

let (+>) (container, canvas, tf) w =
  let open Frp.Behavior.Infix in
  (container, canvas, tf <*> w container canvas)

let create container canvas f = (container, canvas, Frp.Behavior.return f)

(* let run ((container, canvas, f) : 'a t) = ignore (f container canvas) *)

(*
let (+>) : type a. (a -> 'b) t -> a Control.t -> 'b t =
  let open Control in let open Frp.Stream.Infix in
  fun (container, canvas, tf) w -> match w with
  | Slider label -> (container, canvas, tf <*> slider container)
  | Clicks       -> (container, canvas, tf <*> clicks canvas)
  | Drags        -> (container, canvas, tf <*> drags canvas)
;;
*)

(*
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
*)

  (*
  let briefly s =
    let s' = Frp.Stream.create None in
    Frp.Stream.iter s ~f:(fun x ->
      Frp.Stream.trigger s' (Some x);
      Frp.Stream.set s' None
    );
    s'
  ;;
  *)
  (*
  type _ t =
    | Slider : string -> float t
    | Clicks : (int * int) option t
    | Drags  : (int * int) option t
  *)


