  datatype coercion =
    CVAR of int * atype * atype
  | IDC of atype
  | TAG of type_tag * atype * atype
  | CHECK of type_tag * atype * atype
  | MAP of type_tag * coercion list
  | COMP of coercion * coercion

  fun lo_type (CVAR (_, l, _)) = l
    | lo_type (IDC t) = t
    | lo_type (TAG (tt, lo, hi)) = lo
    | lo_type (CHECK (tt, lo, hi)) = lo
    | lo_type (MAP (FUNC, [c1,c2])) = 
         make_type (FUNC, [hi_type c1, lo_type c2])
    | lo_type (MAP (tt, clist)) = make_type (tt, map lo_type clist)
    | lo_type (COMP (c,c')) = lo_type c

  and hi_type (CVAR (_, _, h)) = h
    | hi_type (IDC t) = t
    | hi_type (TAG (tt, lo, hi)) = hi
    | hi_type (CHECK (tt, lo, hi)) = hi
    | hi_type (MAP (FUNC, [c1,c2])) =
         make_type (FUNC, [lo_type c1, hi_type c2])
    | hi_type (MAP (tt, clist)) = make_type (tt, map hi_type clist)
    | hi_type (COMP (c,c')) = hi_type c'

  local 
  val counter = ref 0 
  in
  fun new_cvarid() = !counter before counter := !counter + 1
  end

  fun new_coercion () = 
        CVAR (new_cvarid(), new_typevar(), new_typevar())

  fun make_coercion (t1, t2) = CVAR (new_cvarid(), t1, t2)
       
  fun coercion_sig c = (lo_type c, hi_type c)

  fun isid (IDC _) = true
    | isid (MAP (_, cl)) = fold (fn (c,b) => b andalso isid c) cl true
    | isid (COMP (c1,c2)) = isid c1 andalso isid c2
    | isid _ = false   

  fun interpret_coercion (c as (CVAR (i, t1, t2))) =
      (case (utype t1, utype t2) of
        (SIMPLE (tt, _), DYN _) => TAG (tt, t1, t2) |
        (DYN _, SIMPLE (tt, _)) => CHECK (tt, t1, t2) |
        (SIMPLE _, SIMPLE _) => IDC t1 |
        (DYN _, DYN _) => IDC t1 |
        (TVAR _, TVAR _) => IDC t1 |
        (_, _) => c)
    | interpret_coercion (c as (IDC _)) = c
    | interpret_coercion (c as (TAG _)) = c
    | interpret_coercion (c as (CHECK _))= c
    | interpret_coercion (c as (MAP (tt, cl))) =
        let val c' = MAP (tt, map interpret_coercion cl)
        in if isid c' then IDC (lo_type c)
           else c'
        end
    | interpret_coercion (COMP (c1,c2)) =
        let val c' = COMP (interpret_coercion c1, interpret_coercion c2)
        in if isid c' then IDC (lo_type c1)
           else c'
        end
