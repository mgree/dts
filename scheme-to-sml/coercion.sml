(*$SchemeCoercions: KernelTypes *)

structure Coercion =
  struct
  local open Type UnionFind
  in
  
  type coercion_sig = atype * atype

  fun lo_type (l, h) = l
  fun hi_type (l, h) = h

  fun new_coercion_sig() = (mk_tvar(), mk_tvar())

  datatype coercion =
     CVAR of int * atype * atype
   | IDC 
   | TAG of type_tag
   | CHECK of type_tag
   | MAP of type_tag * coercion list
   | COMP of coercion * coercion

  local val coercion_var_counter = ref 0
  in
  fun new_coercionvar(t1, t2) =
      (coercion_var_counter := !coercion_var_counter + 1;
       CVAR (!coercion_var_counter, t1, t2))
  end

  fun canonical_coercion (t1, t2) =
      (case (utype t1, utype t2) of
        (SIMPLE (tt, _), DYN _) => TAG tt |
        (DYN _, SIMPLE (tt, _)) => CHECK tt |
        (SIMPLE _, SIMPLE _) => IDC |
        (DYN _, DYN _) => IDC |
        (TVAR _, _) => 
           if equal(t1, t2) then IDC
           else new_coercionvar(t1, t2) |
        (_, TVAR _) => 
           if equal(t1, t2) then IDC
           else new_coercionvar(t1, t2))

  end
end
