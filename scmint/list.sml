(* structure SList: LIST =
  struct
  open Object Error *)

  val list = LIST_TAG
  fun length nil = 0
    | length (a::b) = 1 + length b
  local
  fun lappend nil = nil
    | lappend (l1::lr) = l1 @ lappend lr
  exception Fail
  fun list2listoflists nil = nil
    | list2listoflists [l] = ([LIST_UNTAG l] handle TypeError _ => raise Fail)
    | list2listoflists (v1::vr) = LIST_UNTAG v1 :: list2listoflists vr
  fun sappend nil = LIST_TAG nil
    | sappend [v] = v
    | sappend (v1::vr) = 
        let fun continue nil = sappend vr
              | continue (x1::xr) = PAIR_TAG (x1, continue xr)
        in continue (LIST_UNTAG v1)
        end
  in
  fun append l = 
    LIST_TAG (lappend (list2listoflists l)) handle Fail => sappend l
  end
  val reverse = rev
  fun list_ref (a::r, 0) = a
    | list_ref (a::r, n) = list_ref (r, n-1)
    | list_ref (nil, _) = raise InputError ("list-ref", UNSPECIFIED_TAG ())

  fun memq (v,nil) = nil
    | memq (v, v' as (a::r)) = if is_eq (v, a) then v' else memq (v, r)
  fun memv (v,nil) = nil
    | memv (v, v' as (a::r)) = if is_eqv (v, a) then v' else memv (v, r)
  fun member (v,nil) = nil
    | member (v, v' as (a::r)) = if is_equal (v, a) then v' else member (v, r)

  fun assq (v, nil) = None
    | assq (v, (a as (a1,a2)) :: b) = 
        if is_eq (v, a1) then Some a else assq (v, b)
  fun assv (v, nil) = None
    | assv (v, (a as (a1,a2)) :: b) = 
        if is_eqv (v, a1) then Some a else assv (v, b)
  fun assoc (v, nil) = None
    | assoc (v, (a as (a1,a2)) :: b) = 
        if is_equal (v, a1) then Some a else assoc (v, b)
(*
  end
*)



