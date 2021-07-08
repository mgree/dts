  (* 1st stage: parse input stream into an unannotated exp *)

  local open Exp Coercion
  in
  fun nothing _ = ()
  val new_atts = INIT { parameter = nothing,
                       variable = nothing,
                       exp = new_coercion_sig }
                                                 
  val parse = read_exp new_atts
  end
