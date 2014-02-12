open Core

let transfer b b' =
  let sub = Frp.Stream.iter (Frp.Behavior.changes b) ~f:(fun x ->
    Frp.Behavior.trigger b' x
  ) in
  sub

let copy_behavior b =
  let b'  = Frp.Behavior.(return (peek b)) in
  (b', transfer b b')

  (*
module Easing_ = struct

  val mk : (Time.Span.t * float * t) list

  mk ~start:0. 
     [ Cubic_in  (of_ms 100., 0.1)
     ; Cubic_out (of_ms 200., 0.9)
     ]
     ~end:5.
end
*)

module Easing = struct
  type free  = float
  type fixed = unit

  type (_, _) t =
    | Cubic        : Time.Span.t -> (free, free) t
    | Quintic      : Time.Span.t -> (free, free) t
    | Linear       : Time.Span.t -> (free, free) t
    | Free         : Time.Span.t * (x0:float -> x1:float -> Time.Span.t -> float) -> (free, free) t
    | Glue_r       : ('a, fixed) t * (free, 'b) t -> ('a, 'b) t
    | Glue_l       : ('a, free) t * (fixed, 'b) t -> ('a, 'b) t
    | Anchor_left  : float * (free, 'b) t -> (fixed, 'b) t
    | Anchor_right : float * ('a, free) t -> ('a, fixed) t

  let rec interp : type a b. (a, b) t -> (Time.Span.t * (a -> b -> Time.Span.t -> float)) =
    let open Draw.Point in function
    | Free (dur, f) ->
      dur, fun x0 x1 -> let f' = f ~x0 ~x1 in fun t -> f' (min t dur)

    | Cubic dur ->
      dur, fun x0 x1 ->
        let c = (x1 -. x0) /. (Time.Span.to_ms dur ** 3.) in
        fun t -> x0 +. c *. (Time.Span.to_ms (min dur t) ** 3.)

    | Quintic dur ->
      dur, fun x0 x1 ->
        let c = (x1 -. x0) /. (Time.Span.to_ms dur ** 5.) in
        fun t -> x0 +. c *. (Time.Span.to_ms (min dur t) ** 5.)

    | Linear dur ->
      dur, fun x0 x1 ->
        let c = (x1 -. x0) /. Time.Span.to_ms dur in
        fun t -> x0 +. c *. Time.Span.to_ms (min dur t)

    | Anchor_left (x0, t) ->
      let dur, f = interp t in (dur, fun () x1 t -> f x0 x1 t)

    | Anchor_right (x1, t) ->
      let dur, f = interp t in (dur, fun x0 () t -> f x0 x1 t)

    | Glue_r (t_l, t_r) ->
      let (dur_l, f_l), (dur_r, f_r) = interp t_l, interp t_r in
      Time.Span.(dur_l + dur_r)
      , fun x0 x1 -> 
        let f_l'    = f_l x0 () in
        let start_r = f_l' dur_l in
        let f_r'    = f_r start_r x1 in
        fun t -> if t <= dur_l then f_l' t else f_r' Time.Span.(t - dur_l)

    | Glue_l (t_l, t_r) ->
      let (dur_l, f_l), (dur_r, f_r) = interp t_l, interp t_r in
      Time.Span.(dur_l + dur_r)
      , fun x0 x1 ->
        let f_r'    = f_r () x1 in
        let final_l = f_r' Time.Span.(of_ms 0.) in
        let f_l'    = f_l x0 final_l in
        fun t -> if t <= dur_l then f_l' t else f_r' Time.Span.(t - dur_l)

  let interpret t = snd (interp t) () ()

  let behavior t rate =
    let g = interpret t in
    Frp.scan (Frp.Stream.elapsed rate)
      ~init:(g (Time.Span.of_ms 0.))
      ~f:   (fun _ t -> g t)

  let quadratic dur =
    let f ~x0 ~x1 =
      let c = (x1 -. x0) /. Time.Span.to_ms dur in
      fun t -> c *. Time.Span.to_ms t
    in
    Free (dur, f)

  let cubic dur = Cubic dur

  let quintic dur = Quintic dur

  let linear dur = Linear dur

  let anchor_left x0 t = Anchor_left (x0, t)

  let anchor_right x1 t = Anchor_right (x1, t)

  let glue_r l r = Glue_r (l, r)

  let glue_l l r = Glue_l (l, r)

  let rec right_endpoint : type a. (a, fixed) t -> float = function
    | Anchor_right (pt, _) -> pt
    | Anchor_left (_, t)   -> right_endpoint t
    | Glue_r (t1, t2)      -> right_endpoint t2
    | Glue_l (t1, t2)      -> right_endpoint t2

  let rec left_endpoint : type a. (fixed, a) t -> float = function
    | Anchor_right (_, t)  -> left_endpoint t
    | Anchor_left (pt, _)  -> pt
    | Glue_r (t1, t2)      -> left_endpoint t1
    | Glue_l (t1, t2)      -> left_endpoint t1
end

module Sequence = struct
  type finished
  type unfinished

  type ('b, 'a) t =
    (* The float is time elapsed in ms *)
    | For     : float * ('a -> float -> 'a) -> (unfinished, 'a) t
    | Forever : ('a -> float -> 'a) -> (finished, 'a) t

  let forever x  = Forever x

  let for_ dur f = For (dur, f)

  let stay_for dur = For (dur, fun x0 _ -> x0)

  let stay_forever = Forever (fun x0 _ -> x0)

  let (&>) : type b. (unfinished, 'a) t -> (b, 'a) t -> (b, 'a) t =
    fun (For (dur, f1)) t2 ->
      let mk_f f2 =
        fun x0 ->
          let f1' = f1 x0 in
          let xf  = f1' dur in
          let f2' = f2 xf in
          fun t -> if t <= dur then f1' t else f2' (t -. dur)
      in
      match t2 with
      | Forever f2     -> Forever (mk_f f2)
      | For (dur2, f2) -> For (dur +. dur2, mk_f f2)
  ;;

  let run ~init (Forever f) =
    let elapsed =
      Frp.scan ~init:0. (Frp.Stream.elapsed 30.)
        ~f:(fun _ t ->
          Time.Span.to_ms t
        )
    in
    let f' = f init in
    Frp.Behavior.map ~f:f' elapsed

  let quadratic_in dur final =
    let f x0 =
      let c = (final -. x0) /. (dur ** 2.) in
      fun t -> x0 +. c *. (t ** 2.)
    in
    For (dur, f)

  let quadratic_out dur final =
    fun x0 ->
      let b = (x0 -. final) /. dur in
      let a = b /. dur in
      fun t -> x0 +. a *. (t ** 2.) -. 2. *. b *. t

  let quadratic_out dur final =
    let f x0 =
      let b = (x0 -. final) /. dur in
      let a = b /. dur in
      fun t -> x0 +. a *. (t ** 2.) -. 2. *. b *. t
    in
    For (dur, f)

  let quadratic dur ~init ~final =
    For (0., fun _ _ -> init)
    &> quadratic_in (dur /. 2.) ((final -. init) /. 2.)
    &> quadratic_out (dur /. 2.) final
    &> For (0., fun _ _ -> final)

    (*
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
