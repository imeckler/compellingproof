module Mouse : sig
  module Pos : sig
    type t

    val relative : t -> Jq.t -> (int * int)
    val absolute : t -> (int * int)
  end

  module Button : sig
    type t = [ `Left | `Middle | `Right ]
  end

  module Button_state : sig
    type t = bool * bool * bool
  end

  module Movement : sig
    type t =
      [ `Up of Button.t
      | `Down of Button.t
      | `Move
      ]
  end

  module Event : sig
    type t =
      { movement : Movement.t
      ; pos      : Pos.t
      }
  end

  module Click : sig
    type t =
      { pos    : Pos.t
      ; button : Button.t
      }
  end

  module Drag : sig
    type t = int * int
  end

  val buttons : Button_state.t Frp.Behavior.t

  val events : Event.t Frp.Stream.t
  val events_on : Jq.t -> Event.t Frp.Stream.t

  val drags_with : Jq.t -> button:Button.t -> Drag.t Frp.Stream.t

  val clicks : Click.t Frp.Stream.t

  val clicks_on : Jq.t -> Click.t Frp.Stream.t

  val position : Pos.t Frp.Stream.t

  val movements : (int * int) Frp.Stream.t
end

module Key : sig
  type t

  val of_code : int -> t

  val to_code : t -> int

  val keys : t array Frp.Behavior.t

  (* A vector corresponding to which arrow keys are being pressed down. *)
  val arrows : (int * int) Frp.Behavior.t
end

