structure SList: LIST =
  struc
  open Object Erro
  val list = LIST_TA
  fun length nil = 0
    | length (a::b) = 1 + length 
  local
  fun lappend nil = n
    | lappend (l1::lr) = l1 @ lappend l
  exception Fai
  fun list2listoflists nil = n
    | list2listoflists [l] = ([LIST_UNTAG l] handle TypeError _ => raise Fail
    | list2listoflists (v1::vr) = LIST_UNTAG v1 :: list2listoflists vr
  fun sappend nil = LIST_TAG n
    | sappend [v] 
    | sappend (v1::vr
	let fun continue nil = sappend 
	      | continue (x1::xr) = PAIR_TAG (x1, continue xr)
	in continue (LIST_UNTAG v1)
        en
  in
  fun append l 
    LIST_TAG (lappend (list2listoflists l)) handle Fail => sappend 
  end
  val reverse = re
  fun list_ref (a::r, 0) = a
    | list_ref (a::r, n) = list_ref (r, n-
    | list_ref (nil, _) = raise InputError ("list-ref", UNSPECIFIED_TAG ()
  fun memq (v,nil) = ni
    | memq (v, v' as (a::r)) = if is_eq (v, a) then v' else memq (v, 
  fun memv (v,nil) = ni
    | memv (v, v' as (a::r)) = if is_eqv (v, a) then v' else memv (v, 
  fun member (v,nil) = nil
    | member (v, v' as (a::r)) = if is_equal (v, a) then v' else member (v, r)

  fun assq (v, nil) = None
    | assq (v, (a as (a1,a2)) :: b) = 
	if is_eq (v, a1) then Some a else assq (v, b)
  fun assv (v, nil) = None
    | assv (v, (a as (a1,a2)) :: b) = 
	if is_eqv (v, a1) then Some a else assv (v, b
  fun assoc (v, nil) = Non
    | assoc (v, (a as (a1,a2)) :: b) = 
	if is_equal (v, a1) then Some a else assoc (v, b)
  en
