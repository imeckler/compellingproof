module Control = struct
  type _ t =
    | Slider : string -> float t
    | Clicks : (int * int) option t
    | Drags  : (int * int) option t

  type 'a aux = Jq.t * Jq.t * 'a Frp.Stream.t

  let briefly s =
    let s' = Frp.Stream.create in
    Frp.Stream.iter s ~f:(fun x ->
      Frp.Stream.trigger s' (Some x);
      Frp.Stream.set s' None
    );
    s'
  ;;

  let clicks elt = briefly (Jq.clicks elt)

  let drags elt  = briefly (Jq.drags elt)

  let slider container =
    let c = Jq.create "div" in
    let s = Frp.Stream.create 0.0 in
    let update_stream e ui =
      Frp.Stream.trigger s (Js.to_float (Js.Unsafe.get ui (Js.string "value")))
    in

    Jq.append container c;
    Js.Unsafe.(meth_call c "slider" [|
      inject [| obj [|"slide", inject (Js.wrap_callback update_stream) |] |]
    |];
    s
  ;;

  let (+>) (container, canvas, tf : ('a -> 'b) t) = let open Frp.Stream.Infix in function
    | Slider label -> (container, canvas, tf <*> slider container)
    | Clicks       -> (container, canvas, tf <*> clicks canvas)
    | Drags        -> (container, canvas, tf <*> drags canvas)

end

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
    |];

    Frp.Stream.Infix.(container, canvas, tf <*> s)
  ;;

  let sliders container canvas f = (container, canvas, Frp.Stream.return f)

  (*
    sliders x y (fun thickness angle ->
      set_thickness_and_angle y thickness angle
    )
    +> "Thickness"
    +> "angle"
  *)

  type 'a t =
    { canvas    : Jq.t
    ; container : Jq.t
    ; arg       : 'a ref
    ; listen    : (float -> unit) -> unit
(*     ; refs      : 'a ref_list *)
    }

  type 'a t =
    { canvas : Jq.t
    ; container : Jq.t
    ; vals      : 'a stream
    }

  type 'a t = 'a -> unit

  let add_slider (t : 'a t) : (float -> 'a) t =
    let c = Jq.create "div" in
    fun f ->

    let on_slide _e ui = 
      let percentage = Js.to_float (get ui (Js.string "value")) in
      last := percentage /. 100
    in
    let listen f =
      t.listen 

  type 'a stream = ('a -> unit) -> unit

  let run_slider (t : 'a t) (f : 'a) =
    t.sink_callback f

  let add_slider (t : 'a t) : (float -> 'a) t =
    let c   = Jq.create "div" in
    let arg = ref 0.0 in
    let on_slide _e ui =
      let percentage = Js.to_float (get ui (Js.string "value")) in
      arg := (percentage, t.last);
    in
    Jq.append container c;
    Js.Unsafe.( meth_call c "slider" [| inject "slide", inject (Js.wrap_callback on_slide) |] );
    { canvas    = t.canvas
    ; container = t.container
    ; sink_callback = fun f -> ()
    }

  type 'a t =
    { sink      : ('a -> unit) ref
    ; last      : 'a ref
    ; tail      : 'b ref
    ; canvas    : Jq.t
    ; container : Jq.t
    }

  let create () =
    let container = Jq.create "div" in
    let canvas    = Jq.create "div" in
    Jq.append container canvas
    { sink = fun () -> () ; container; canvas }
  ;;

  let add_slider (t : 'a t) : (float -> 'a) t =
    let c        = Jq.create "div" in
    let last     = ref 0.0 in
    let on_slide = fun _e ui ->
      let percentage = Js.to_float (get ui (Js.string "value")) in
      last := (percentage, t.last);
    in

    Jq.append t.container c;
    Js.Unsafe.( meth_call c "slider" [| inject "slide", inject (Js.wrap_callback on_slide) |] );
    { t with sink = fun f -> 
      let open Js.Unsafe in
      meth_call c "on" [|
        inject "slide",
        inject (Js.wrap_callback (fun _e ui -> 
          let percentage = Js.to_float (get ui (Js.string "value")) in
          t.sink (f (percentage /. 100))
        ))
      |]
    }


  let add_slider (f : 'a t) : (int -> 'a) t = 
    let new_slider = Slider.create () in
    fun g -> 
      new_slider##slide(fun e ->
        f (g (e##ui##value))
      )

end
