open Core

module Mouse = struct
  module Pos = struct
    (* The absolute position (relative to the upper left corner of the page) *)
    type t = (int * int)

    let relative_to (x, y) jq =
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
    type t =
      { change  : int * int
      ; buttons : Button_state.t
      }
  end

  module Event = struct
    type t =
      { movement : Movement.t
      ; pos      : Pos.t
      }
  end

  let set_data (t : Jq.t) k v =
    Js.Unsafe.(meth_call t "data" [|inject (Js.string k); inject v|])

  let get_data (t : Jq.t) k : 'a option =
    Js.Optdef.to_option
      Js.Unsafe.(meth_call t "data" [|inject (Js.string k)|])

  let setup_handlers t hs =
    let toks = Array.map hs ~f:(fun (evt, handler) ->
      Jq.Event.on t evt handler)
    in
    fun () -> Array.iter toks ~f:Jq.Event.off

  let unsafe_get o s = Js.Unsafe.get o (Js.string s)

  let events_on t =
    match get_data t "events_on" with
    | Some s -> s
    | None   -> 
      let s =
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
          |])
      in
      set_data t "events_on" s;
      s
    ;;

  let events = events_on Jq.body

  let clicks_on t =
    Frp.Stream.create () ~start:(fun trigger ->
      setup_handlers t [|
        "click", fun e -> trigger {
          Click.pos = (unsafe_get "pageX", unsafe_get "pageY");
          button    = Button.from_code (unsafe_get e "which")
        }
      |])

  let buttons =
    Frp.scan events ~init:(false, false, false) ~f:(fun ((l, m, r) as s) e -> match e with
      | {Event.movement = `Down `Left}   -> (true, m, r)
      | {Event.movement = `Up `Left}     -> (false, m, r)
      | {Event.movement = `Down `Middle} -> (l, true, r)
      | {Event.movement = `Up `Middle}   -> (l, false, r)
      | {Event.movement = `Down `Right}  -> (l, m, true)
      | {Event.movement = `Up `Right}    -> (l, m, false))

  let clicks =
    Frp.Stream.filter_map events ~f:(function
      | {Event.movement = `Down button; pos} -> Some {Click.button; pos}
      | _                                    -> None)

  let position =
    Frp.Stream.filter_map events ~f:(function
      | {Event.movement = `Move; pos} -> Some pos
      | _                             -> None)

  let movements =
    Frp.Stream.delta position ~f:(fun (x0, y0) (x1, y1) -> (x1 - x0, y1 - y0))

  let drags =
    Frp.project_with buttons movements ~f:(fun btns change ->
      {Drag.buttons = btns, change})
    |> Frp.Stream.filter ~f:(fun {Drag.buttons; _} -> match buttons with
      | (true, _, _) | (_, true, _) | (_, _, true) -> true
      | _                                          -> false)
end

module Key = struct
  type t = int

  let of_code t = t

  let to_code t = t
end

