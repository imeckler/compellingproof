open Core

module Control = struct
  type 'a t = Jq.t -> Jq.t -> 'a

  module Continuous_slider = struct
    (* rate in percentage per seconds *)
    let play_incrs sliding button rate = let open Frp in
      (* rate' is in percentage per milliseconds *)
      let rate' = rate /. 1000. in
      let playing = scan (Jq.clicks button) ~init:false ~f:(fun p _ -> not p) in
      Jq.sink_attr button ~name:"class" ~value:(Behavior.map playing ~f:(fun p -> 
        if p then "btn cp-slider-button-playing" else "btn cp-slider-button-paused"
      )) |> ignore;

      let incrs = Stream.map (Stream.deltas 30.) ~f:(fun chg -> Time.Span.to_ms chg *. rate') in
      when_ (Behavior.zip_with ~f:(fun p s -> p && not s) playing sliding) incrs

    (* TODO: This is dirty *)
    let sink_auto_incrs slider_div slider_val auto_incrs =
      Frp.Stream.iter auto_incrs ~f:(fun x ->
        let curr_val = Js.to_float (slider_div##slider(Js.string "option", Js.string "value")) in
        let new_val  = min (curr_val +. x) 100. in
        Frp.Behavior.trigger slider_val (new_val /. 100.);
        slider_div##slider_set(Js.string "option", Js.string "value", new_val)
      )

    let mk_play_button () = Jq.create "div"

    let slider label container _ =
      let inner_container        = Jq.create "div" in
      let slider_div             = Jq.create "div" in
      let button                 = mk_play_button () in
      let slider_val             = Frp.Behavior.return 0.0 in
      let sliding                = Frp.Behavior.return false in
      let play_incr_stream       = play_incrs sliding button 25. in
      let update_slider_val e ui =
        Frp.Behavior.trigger slider_val 
          (Js.to_float (Js.Unsafe.get ui (Js.string "value")) /. 100.)
      in
      begin
        sink_auto_incrs (Obj.magic slider_div) slider_val play_incr_stream |> ignore;
        Jq.append inner_container button;
        Jq.append inner_container slider_div;
        Jq.append container inner_container;
      end;
      let arg_obj =
        Js.Unsafe.(obj [|
          "slide", inject (Js.wrap_callback update_slider_val);
          "start", inject (Js.wrap_callback (fun _ _ -> Frp.Behavior.trigger sliding true));
          "stop", inject (Js.wrap_callback (fun _ _ -> Frp.Behavior.trigger sliding false));
          "step", inject (Js.float 0.01)
        |])
      in
      Js.Unsafe.(meth_call slider_div "slider" [|inject arg_obj|]) |> ignore;
      slider_val
  end

  let continuous_slider = Continuous_slider.slider

  let step_slider steps container _ =
    let slider_div = Jq.create "div" in
    let slider_val = Frp.Behavior.return 0 in
    let update_slider_val e ui =
      Frp.Behavior.trigger slider_val (Js.Unsafe.get ui (Js.string "value"))
    in
    let arg_obj =
      Js.Unsafe.(obj [|
        "min", inject 0;
        "max", inject steps;
        "change", inject (Js.wrap_callback update_slider_val)
      |])
    in
    Jq.append container slider_div;
    Js.Unsafe.(meth_call slider_div "slider" [|inject arg_obj|]) |> ignore;
    slider_val

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
      (map ~f:(fun _ -> 1)  (Jq.clicks incr_button)) 
      (map ~f:(fun _ -> -1) (Jq.clicks decr_button))
    |> Frp.scan ~f:(fun n i -> max bot (min top (n + i))) ~init:bot
    |> Frp.Behavior.skip_duplicates
  ;;


  let clicks _ canvas = Jq.clicks canvas
  let drags _ canvas  = Jq.drags canvas

  let drag_point init : (int * int) Frp.Behavior.t t = fun _ canvas ->
    let open Jq.Event.Mouse.Drag in
    Frp.scan (Jq.drags canvas) ~init
      ~f:(fun (x, y) (dx, dy) -> (x + dx,  y + dy))

  let drag_angle (cx, cy) =
    let pi = acos (-1.) in
    let angle_of_pos =
      let cxf, cyf = float_of_int cx, float_of_int cy in
      fun (x, y) ->
        let a = atan ((cyf -. float_of_int y) /. (cxf -. float_of_int x)) in
        if x < cx then pi -. a else a
    in
    fun container canvas ->
      Frp.Behavior.map (drag_point (cx + 100, cy) container canvas) 
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

