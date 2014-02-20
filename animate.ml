open Core

let transfer b b' =
  let sub = Frp.Stream.iter (Frp.Behavior.changes b) ~f:(fun x ->
    Frp.Behavior.trigger b' x
  ) in
  sub

let copy_behavior b =
  let b'  = Frp.Behavior.(return (peek b)) in
  (b', transfer b b')

module Sequence = struct
  type finished
  type unfinished

  type ('b, 'a) t =
    (* The float is time elapsed in ms *)
    | For      : float * ('a -> float -> 'a) -> (unfinished, 'a) t
    | Forever  : ('a -> float -> 'a) -> (finished, 'a) t
(*     | Seq      : (unfinished, 'a) t * (finished, 'a) t -> (finished, 'a) t *)
(*     | Wait_for : bool Frp.Stream.t * (finished, 'a) t -> (finished, 'a) t *)

  let forever x  = Forever x

  let for_ dur f = For (dur, f)

  let stay_for dur = For (dur, fun x0 _ -> x0)

  let stay_forever = Forever (fun x0 _ -> x0)

  let rec (>>) : type b. (unfinished, 'a) t -> (b, 'a) t -> (b, 'a) t =
    let mk_f dur1 f1 f2 =
      fun x0 ->
        let f1' = f1 x0 in
        let xf  = f1' dur1 in
        let f2' = f2 xf in
        fun t -> if t <= dur1 then f1' t else f2' (t -. dur1)
    in
    fun (For (dur1, f1) as t1) t2 ->
    match t2 with
    | Forever f2      -> Forever (mk_f dur1 f1 f2)
    | For (dur2, f2)  -> For (dur1 +. dur2, mk_f dur1 f1 f2)
(*     | Seq (tu, tf)    -> Seq (t1 >> tu, tf) *)
(*     | Wait_for (_, _) -> Seq (t1, t2) *)
  ;;

  let rec (run' : (finished, 'a) t -> 'a -> 'a Frp.Behavior.t) = function
    | Forever f ->
      fun init ->
        let elapsed =
          Frp.scan ~init:0. (Frp.Stream.elapsed 30.)
            ~f:(fun _ t ->
              Time.Span.to_ms t
            )
        in
        let f' = f init in
        Frp.Behavior.map ~f:f' elapsed

(*
    | Seq (For (dur, fu) as tu, Wait_for (b, t)) ->
      fun init ->
      let final_val = fu init dur  in
      let ff        = run' tf in
      Frp.Behavior.bind 
      ff final_val

    | Wait_for (b, t) ->
      fun init -> failwith ""
  ;;
  *)


  let run ~init t = run' t init

  let quadratic_in dur final =
    let f x0 =
      let c = (final -. x0) /. (dur ** 2.) in
      fun t -> x0 +. c *. (t ** 2.)
    in
    For (dur, f)

  let quadratic_in dur final =
    fun x0 ->
      let c = (final -. x0) /. (dur ** 2.) in
      fun t -> x0 +. c *. (t ** 2.)

  let quadratic_out dur final =
    fun x0 ->
      let b = (x0 -. final) /. dur in
      let a = b /. dur in
      fun t -> x0 +. a *. (t ** 2.) -. 2. *. b *. t

  let quadratic dur ~final =
    let f x0 =
      let h = final -. x0 /. 2. in
      let q_in  = quadratic_in (dur /. 2.) h x0 in
      let q_out = quadratic_out (dur /. 2.) final h in
      fun t -> if t <= dur /. 2. then q_in t else q_out (t -. (dur /. 2.))
    in
    For (dur, f)

  let jump_to x = For (0., fun _ _ -> x)

    (*
  let quadratic_out dur final =
    let f x0 =
      let b = (x0 -. final) /. dur in
      let a = b /. dur in
      fun t -> x0 +. a *. (t ** 2.) -. 2. *. b *. t
    in
    For (dur, f)

  let quadratic dur ~init ~final =
    For (0., fun _ _ -> init)
    >> quadratic_in (dur /. 2.) ((final -. init) /. 2.)
    >> quadratic_out (dur /. 2.) final
    >> For (0., fun _ _ -> final)


  let (&>) (f1, dur) f2 =
    fun x0 t0 ->
      let f1' = f1 x0 t0 in
      let xf  = f1' Time.(t0 + dur) in
      let f2' = f2 xf Time.(t0 + dur) in
      fun t -> if Time.(t - t0) <= dur then f1' t else f2' t
      *)
end

  (*
let sequence bs =
  let n = Array.length bs in
  if n = 0 then failwith "Animate.sequence: Empty array";

  let (b', sub) = copy_behavior (bs.(0)) in
(*   let i        = ref 0 in *)
  let last_sub = ref sub in

  let rec go i =
    if !i = n
    then ()
    else begin
      let (t, b) = bs.(i) in
      Frp.Subscription.cancel 
      transfer b b'
      set_timeout 
    end
  in ()
*)
