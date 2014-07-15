module Font = struct
  module Style = struct
    type t = Normal | Italic | Oblique
  end

  module Weight = struct
    type t =
      | Normal
      | Bold
      | Bolder
      | Lighter
      | N100
      | N200
      | N300
      | N400
      | N500
      | N600
      | N700
      | N800
      | N900
  end

  module Stretch = struct
    type t =
      | Normal
      | Wider
      | Narrower
      | Ultra_condensed
      | Extra_condensed
      | Condensed
      | Semi_condensed
      | Semi_expanded
      | Expanded
      | Extra_expanded
      | Ultra_expanded
  end

  module Size = struct
    type t =
      | Length of float
      | Point of float
      | Percentage of float
  end

  module Family = struct
    module Generic = struct
      type t =
        | Serif
        | Sans_serif
        | Cursive
        | Fantasy
        | Mono_space
    end

    type single = [`Named of string | `Generic of Generic.t]

    type t = single array
  end
end

type t =
  { style   : Font.Style.t option
  ; weight  : Font.Weight.t option
  ; stretch : Font.Stretch.t option
  ; size    : Font.Size.t option
  ; family  : Font.Family.t option
  }

