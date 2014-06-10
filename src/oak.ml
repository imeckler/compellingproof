open Core

module Line_style = struct
  type t =
    { color : Color.t
    ; width : float
    ; cap : Line_cap.t
    ; join : Line_join.t
    ; dashing : int array
    ; dash_offset : int
    }
end

module Form = struct
  type basic =
    | Path of Line_style.t * Path.t
    | Shape of (Line_style.t, Fill_style.t) Either.t * Shape.t
    | Image of int * int * (int * int) * string
    | Group of Transform.t * t array
  and t = 
    { theta : float
    ; scale : float
    ; x : float
    ; y : float
    ; alpha : float
    ; form : basic
    }
end
