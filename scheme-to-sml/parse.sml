structure KernelParse =
  struct

  (* 1st stage: parse input stream into an unannotated exp *)

  local open KernelExp
  in
  fun id x = x
  fun nothing _ = ()
  val no_atts = INIT { parameter = id,
                       variable = id,
                       exp = id,
                       args = id,
                       formals = id }
                                                 
  fun parse ip = read_exp nothing no_atts ip
  end

  end

