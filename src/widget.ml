open Core

module Control = struct
  type 'a t = Jq.t -> Jq.t -> 'a

  let add_slider_listener' slider_elt event listener : unit = let open Js.Unsafe in
    meth_call slider_elt "slider"
      [| inject (Js.string "option")
      ;  inject (Js.string event)
      ;  inject listener
      |]

  let add_slider_listener slider_elt event listener =
    add_slider_listener' slider_elt event (Js.wrap_callback listener)

  let remove_slider_listener slider_elt event =
    add_slider_listener' slider_elt event Js.Opt.empty

  let init_slider slider_elt args : unit =
    Js.Unsafe.meth_call slider_elt "slider" args

  module Continuous_slider = struct
    (* rate in percentage per seconds *)
    let play_incrs sliding button rate = let open Frp in
      (* rate' is in percentage per milliseconds *)
      let rate' = rate /. 1000. in
      let playing = scan (Input.Mouse.clicks_on button) ~init:false ~f:(fun p _ -> not p) in
      Jq.sink_attr button ~name:"class" ~value:(Behavior.map playing ~f:(fun p -> 
        if p then "btn cp-slider-button-playing" else "btn cp-slider-button-paused"
      )) |> ignore;

      let incrs = Stream.map (Stream.deltas 30.) ~f:(fun chg -> Time.Span.to_ms chg *. rate') in
      when_ (Behavior.zip_with ~f:(fun p s -> p && not s) playing sliding) incrs

    let sink_incrs slider_div slider_val =
      Frp.Stream.iter slider_val ~f:(fun x ->
        slider_div##slider_set(Js.string "option", Js.string "value", Js.float (x *. 100.)))

    let mk_play_button () = Jq.create "div"

    let mk_slides_stream slider_elt =
      Frp.Stream.create' () ~start:(fun trigger ->
        let update_slider_val _e ui =
          trigger (Js.to_float (Js.Unsafe.get ui (Js.string "value")) /. 100.)
        in 
        add_slider_listener slider_elt "slide" update_slider_val;
        fun () -> remove_slider_listener slider_elt "slide")

    let mk_sliding_beh slider_elt =
      Frp.Stream.create () ~start:(fun trigger -> let open Js.Unsafe in
        add_slider_listener slider_elt "start" (fun _ _ -> trigger true);
        add_slider_listener slider_elt "stop" (fun _ _ -> trigger false);
        fun () ->
          remove_slider_listener slider_elt "start";
          remove_slider_listener slider_elt "stop"
      )
      |> Frp.latest ~init:false

    let slider label container _ =
      let inner_container        = Jq.create "div" in
      let slider_div             = Jq.create "div" in
      let button                 = mk_play_button () in
      init_slider slider_div [||];

      let sliding             = mk_sliding_beh slider_div in
      let play_incr_stream    = play_incrs sliding button 25. in
      let slide_vals, trigger = mk_slides_stream slider_div in
      let slider_val = let open Frp in
        Stream.merge
          (Stream.map ~f:(fun d -> `Incr d) play_incr_stream)
          (Stream.map ~f:(fun p -> `Slide_to p) slide_vals)
        |>
        scan ~init:0. ~f:(fun acc x -> match x with
          | `Incr d     -> min (acc +. (d /. 100.)) 1.
          | `Slide_to p -> p)
      in

      begin
        sink_incrs (Obj.magic slider_div) (Frp.Behavior.changes slider_val) |> ignore;
        Jq.append inner_container button;
        Jq.append inner_container slider_div;
        Jq.append container inner_container;
      end;
      slider_val
  end

  let continuous_slider = Continuous_slider.slider

  let step_slider steps container _ =
    let slider_div = Jq.create "div" in
    Jq.append container slider_div;
    init_slider slider_div Js.Unsafe.(
      obj [|"min", inject 0; "max", inject steps|]);
    Frp.Stream.create () ~start:(fun trigger ->
      add_slider_listener slider_div "change" (fun _e ui ->
        trigger (Js.Unsafe.get ui (Js.string "value")));
      fun () -> remove_slider_listener slider_div "change")
    |> Frp.latest ~init:0

  let incr_decr ?(bot=min_int) ?(top=max_int) () container _ =
    let buttons_p   = Jq.create "p" in
    let incr_button = Jq.create "a class='btn btn-primary'" in
    let decr_button = Jq.create "a class='btn btn-default'" in
    let incr_icon   = Jq.create "span class='glyphicon glyphicon-plus'" in
    let decr_icon   = Jq.create "span class='glyphicon glyphicon-minus'" in
    begin
      Jq.append incr_button incr_icon;
      Jq.append decr_button decr_icon;
      Jq.append buttons_p decr_button; Jq.append buttons_p incr_button;
      Jq.append container buttons_p
    end;
    let open Frp.Stream in
    merge
      (map ~f:(fun _ -> 1)  (Input.Mouse.clicks_on incr_button)) 
      (map ~f:(fun _ -> -1) (Input.Mouse.clicks_on decr_button))
    |> Frp.scan ~f:(fun n i -> max bot (min top (n + i))) ~init:bot
    |> Frp.Behavior.skip_duplicates
  ;;


  let clicks _ canvas = Input.Mouse.clicks_on canvas
  let drags_with ~button _ canvas = Input.Mouse.drags_with ~button canvas

  let drag_point_with ~init ~button : (int * int) Frp.Behavior.t t = fun _ canvas ->
    Frp.scan (Input.Mouse.drags_with ~button:`Left canvas) ~init
      ~f:(fun (x, y) (dx, dy) -> (x + dx,  y + dy))

  let pi = acos (-1.)

  let drag_angle_with (cx, cy) ~button  =
    let cxf, cyf = float_of_int cx, float_of_int cy in
    let angle_of_pos =
      fun (x, y) ->
        let a = atan ((cyf -. float_of_int y) /. (cxf -. float_of_int x)) in
        if x < cx then pi -. a else a
    in
    fun container canvas ->
      Frp.Behavior.map (drag_point_with ~button ~init:(cx + 100, cy) container canvas)
        ~f:angle_of_pos

end

type 'a t = Jq.t * Jq.t * 'a

let (+>) (container, canvas, f) w =
  (container, canvas, f (w container canvas))

let create ~width ~height container f = 
  let canvas = 
    Jq.Dom.svg_node "svg" [|
      "width", string_of_int width; 
      "height", string_of_int height;
      "class", "proof-canvas"
    |]
    |> Jq.wrap
  in
  Jq.append container canvas;
  (container, canvas, f)

let run (_, canvas, drawing) =
  let (drawing_elt, sub) = Draw.render drawing in
  Jq.append canvas (Jq.wrap drawing_elt);
  sub

