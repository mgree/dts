(*$SchemeCoercions: KernelTypes *)

structure SchemeCoercions =
  struct
  local open SchemeTypes
  in
  
  type coercion_sig = atype * atype

  datatype cop =
    CVAR of int
  | IDC
  | TAG of type_tag
  | CHECK of type_tag
  | MAP of type_tag * coercion list
  | COMP of coercion * coercion
  withtype coercion = cop * atype * atype

  fun lo_type (_, l, _) = l
  fun hi_type (_, _, h) = h

  local 
  val counter = ref 0 
  in
  fun new_cvar() = !counter before counter := !counter + 1
  end

  fun new_coercion () = (CVAR (new_cvar()), new_typevar(), new_typevar())

  fun make_coercion (t1, t2) = (CVAR (new_cvar()), t1, t2)
       
  fun coercion_sig (c as (_, l, h)) = (l, h)

  fun isid (IDC,_,_) = true
    | isid (MAP (tt, cl), _,_) = fold (fn (c,b) => b andalso isid c) cl true
    | isid (COMP (c1,c2),_,_) = isid c1 andalso isid c2
    | isid _ = false   

  fun interpret_coercion (c as (CVAR i, t1, t2)) =
      (case (utype t1, utype t2) of
        (SIMPLE (tt, _), DYN _) => (TAG tt, t1, t2)  |
        (DYN _, SIMPLE (tt, _)) => (CHECK tt, t1, t2) |
        (SIMPLE _, SIMPLE _) => (IDC, t1, t2) |
        (DYN _, DYN _) => (IDC, t1, t2) |
        (TVAR _, TVAR _) => (IDC, t1, t2)  |
        (_, _) => c)
    | interpret_coercion (IDC, t1, t2) = (IDC, t1, t2)
    | interpret_coercion (TAG tt, t1, t2) = (TAG tt, t1, t2)
    | interpret_coercion (CHECK tt, t1, t2) = (CHECK tt, t1, t2)
    | interpret_coercion (MAP (tt, cl), t1, t2) =
	let val c' = (MAP (tt, map interpret_coercion cl), t1, t2)
	in if isid c' then (IDC, t1,t2)
	   else c'
        end
    | interpret_coercion (COMP (c1,c2), t1,t2) =
	let val c' = (COMP (interpret_coercion c1, interpret_coercion c2), t1, t2)
	in if isid c' then (IDC, t1, t2)
	   else c'
        end
  end
end
