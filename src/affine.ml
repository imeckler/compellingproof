type t =
  { translation : Vector.t
  ; matrix      : Matrix.t
  }

let identity = 
  { translation = (0., 0.)
  ; matrix      = Matrix.identity
  }

let compose t1 t2 =
  { matrix      = Matrix.mul t1.matrix t2.matrix
  ; translation = Vector.add t1.translation (Matrix.apply t1.matrix t2.translation)
  }

