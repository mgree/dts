structure SVector: VECTOR =
  struct 
  open Error Object

  fun make_vector (k, fill) = 
      Vector.tabulate (k, fn _ => fill)
      handle
        Size => raise InputError ("make-vector", NUMBER_TAG k)
  val vector = Vector.vector
  val vector_length = Vector.length
  fun vector_ref (x as (v,n)) = Vector.sub x 
	handle Subscript => raise InputError ("vector-ref", NUMBER_TAG n)
  fun vector_set (v, k, obj) = Vector.sub (v, k) := !obj
	handle Subscript => raise InputError ("vector-set", NUMBER_TAG k)
  end
