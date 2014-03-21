module Force : sig
  type t = float * float
end

module Interaction : sig
  type ('a, 'b) t = 'a -> 'b -> (Force.t * Force.t)
end

module Enitity : sig
  type t = 
end
