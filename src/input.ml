open Core

let setup_handlers t hs =
  let toks = Array.map hs ~f:(fun (evt, handler) ->
    Jq.Event.on t evt handler)
  in
  fun () -> Array.iter toks ~f:(Jq.Event.off t)

let unsafe_get o s = Js.Unsafe.get o (Js.string s)

module Mouse = struct
  module Pos = struct
    (* The absolute position (relative to the upper left corner of the page) *)
    type t = (int * int)

    let relative (x, y) jq =
      let (left, top) = Jq.offset jq in (x - left, y - top)

    let absolute t = t
  end

  module Button = struct
    type t = [ `Left | `Middle | `Right ]

    let from_code = function
      | 1 -> `Left
      | 2 -> `Middle
      | 3 -> `Right
      | x -> failwith ("Not a valid mouse code: " ^ string_of_int x)
  end

  module Button_state = struct
    type t = bool * bool * bool
  end

  module Movement = struct
    type t =
      [ `Up of Button.t
      | `Down of Button.t
      | `Move
      ]
  end

  module Click = struct
    type t =
      { pos    : Pos.t
      ; button : Button.t
      }
  end

  module Drag = struct
    type t = int * int
  end

  module Event = struct
    type t =
      { movement : Movement.t
      ; pos      : Pos.t
      }
  end

  let set_data (t : Jq.t) k v : unit =
    Js.Unsafe.(meth_call t "data" [|inject (Js.string k); inject v|])

  let get_data (t : Jq.t) k : 'a option =
    Js.Optdef.to_option
      Js.Unsafe.(meth_call t "data" [|inject (Js.string k)|])

  let cached (t : Jq.t) (s : string) (create : unit -> 'a) : 'a =
    match get_data t s with
    | Some x -> x
    | None   -> let x = create () in set_data t s x; x
  ;;

  let events_on t =
    cached t "events_on" (fun () ->
      Frp.Stream.create () ~start:(fun trigger ->
        let trigger_event movement e =
          trigger { Event.movement; pos = (unsafe_get e "pageX", unsafe_get e "pageY") }
        in
        setup_handlers t [|
          "mousemove", (fun e ->
            trigger_event `Move e);
          "mouseup", (fun e ->
            trigger_event (`Up (Button.from_code (unsafe_get e "which"))) e);
          "mousedown", (fun e ->
            trigger_event (`Down (Button.from_code (unsafe_get e "which"))) e);
        |]))
    ;;

  let events = events_on Jq.body

(*
  let clicks_on t =
    Frp.Stream.create () ~start:(fun trigger ->
      setup_handlers t [|
        "click", fun e -> trigger {
          Click.pos = (unsafe_get e "pageX", unsafe_get e "pageY");
          button    = Button.from_code (unsafe_get e "which")
        }
      |])

*)

  let exists_button (l, m, r) = l || m || r

  let buttons =
    Frp.scan events ~init:(false, false, false) ~f:(fun ((l, m, r) as s) e -> match e with
      | {Event.movement = `Down `Left}   -> (true, m, r)
      | {Event.movement = `Up `Left}     -> (false, m, r)
      | {Event.movement = `Down `Middle} -> (l, true, r)
      | {Event.movement = `Up `Middle}   -> (l, false, r)
      | {Event.movement = `Down `Right}  -> (l, m, true)
      | {Event.movement = `Up `Right}    -> (l, m, false)
      | _                                -> s)

  let clicks_on t =
    Frp.Stream.filter_map (events_on t) ~f:(function
      | {Event.movement = `Down button; pos} -> Some {Click.button; pos}
      | _                                    -> None)

  let clicks = clicks_on Jq.body

  let position =
    Frp.Stream.filter_map events ~f:(function
      | {Event.movement = `Move; pos} -> Some pos
      | _                             -> None)

  let diff_pos (x0, y0) (x1, y1) = (x1 - x0, y1 - y0)

  let movements = Frp.Stream.delta position ~f:diff_pos

  let movements_on t =
    cached t "movements_on" (fun () ->
      events_on t
      |> Frp.Stream.filter_map ~f:(function
        | {Event.movement = `Move; pos} -> Some pos
        | _                             -> None)
      |> Frp.Stream.delta ~f:diff_pos)

  let dragged_with button t = let open Frp.Stream in
    Frp.scan (merge (map events ~f:(fun e -> `Body e)) (map (events_on t) ~f:(fun e -> `Elt e)))
      ~init:false
      ~f:(fun b e -> match e with
        | `Elt {Event.movement}  when movement = `Down button -> true
        | `Body {Event.movement} when movement = `Up button   -> false
        | _                                                   -> b)

  let drags_with t ~button = 
    let s = match button with `Left -> "left" | `Middle -> "middle" | `Right -> "right" in
    cached t ("drags_with_" ^ s) (fun () ->
      Frp.when_ (dragged_with button t) (movements_on t))
end

module Key = struct
  type t = int

  let of_code t = t

  let to_code t = t

  let prevent_default = ref true

  let key_stream =
    Frp.Stream.create () ~start:(fun trigger ->
      setup_handlers Jq.body [|
        "keydown", (fun e ->
          if !prevent_default then Js.Unsafe.meth_call e "preventDefault" [||];
          trigger (`Down (unsafe_get e "which")));
        "keyup", (fun e ->
          if !prevent_default then Js.Unsafe.meth_call e "preventDefault" [||];
          trigger (`Up (unsafe_get e "which")))
      |])

  let keys =
    let pressed = Inttbl.create () in
    Frp.scan ~init:[||] key_stream ~f:(fun _ k -> 
      begin match k with
        | `Down n -> Inttbl.add pressed ~key:n ~data:()
        | `Up n -> Inttbl.remove pressed n
      end;
      Inttbl.keys pressed)

  let arrows = 
    let bool_to_int = function
      | true  -> 1
      | false -> 0
    in
    Frp.Behavior.map keys ~f:(fun ks ->
    ( bool_to_int (Array.mem ks (of_code 39)) - bool_to_int (Array.mem ks (of_code 37))
    , bool_to_int (Array.mem ks (of_code 38)) - bool_to_int (Array.mem ks (of_code 40))))
end

