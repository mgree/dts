(*$SchemeCoercions: KernelTypes *)

structure SchemeCoercions =
  struct
  local open KernelTypes
  in
  
  type coercion_sig = atype * atype
  
  datatype cop =
  	CVAR of int
  | IDC
  | TAG of type_tag
  | CHECK of type_tag
  | MAP of type_tag * coercion_op list
  withtype coercion_op = cop ref

  type coercion = coercion_op * coercion_sig

  local 
  val counter = ref 0 
  in
  fun new_copvar() = 
      ref (!counter) before counter := !counter + 1
  end

  fun new_coercion () =
      (new_copvar(), (new_typevar(), new_typevar()))
       
  fun coercion_sig (_, s) = s
  fun coercion_op (ref cop, _) = cop
  
  fun interpret_cop (cop as ref (CVAR _), (t1, t2)) =
      (case (utype t1, utype t2) of
        (SIMPLE (tt, _), DYN _) => cop := TAG tt |
        (DYN _, SIMPLE (tt, _)) => cop := CHECK tt |
        (SIMPLE _, SIMPLE _) => cop := IDC |
        (DYN _, DYN _) => cop := IDC |
        (TVAR _, TVAR _) => cop := IDC |
        (_, _) => ())
    | interpret_cop _ = raise Unimplemented "interpret_cop"
 
  end
end

  