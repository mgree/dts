structure Runtime =
  struct

  structure Boolean =
    struct
    val tt = BOOL_TAG true	(* External representation: #t *)
    val ff = BOOL_TAG false	(* External representation: #f *)
    fun not [ref (BOOL false)] = tt
      | not [_] = ff
      | not _ = wrong_args ("not", 1)
    fun is_boolean [ref (BOOL _)] = tt
      | is_boolean [_] = ff
      | is_boolean _ = wrong_args ("boolean?", 1)
    val bindings = [("not", not), ("boolean?", is_boolean)]
    end

  structure EquivPred =
    struct
    fun is_eqv [ref (BOOL b), ref (BOOL b')] = (b=b') |
        is_eqv [ref (SYMBOL s), ref (SYMBOL s')] = (s=s') |
        is_eqv [ref (CHAR c), ref (CHAR c')] = (c=c') |
        is_eqv [d as ref (VECTOR v), d' as ref (VECTOR v')] = (d=d') |
        is_eqv [d as ref (PAIR p), d' as ref (PAIR p')] = (d=d') |
        is_eqv [ref (NUMBER n), ref (NUMBER n')] = (n=n') |
        is_eqv [d as ref (STRING s), d' as ref (STRING s')] = (d=d') |
        is_eqv [d as ref (PROCEDURE p), d' as ref (PROCEDURE p')] = (d=d') |
        is_eqv [ref (LIST nil), ref (LIST nil)] = true |
        is_eqv [d as ref (LIST l), d' as ref (LIST l')] = (d=d') |
        is_eqv [_,_] = false |
        is_eqv _ = wrong_args ("eqv?", 2)
    fun is_eq [ref (BOOL b), ref (BOOL b')] = (b=b') |
        is_eq [ref (SYMBOL s), ref (SYMBOL s')] = (s=s') |
        is_eq [ref (CHAR c), ref (CHAR c')] = (c=c') |
        is_eq [d as ref (VECTOR v), d' as ref (VECTOR v')] = (d=d') |
        is_eq [d as ref (PAIR p), d' as ref (PAIR p')] = (d=d') |
        is_eq [d as ref (NUMBER n), d' as ref (NUMBER n')] = (d=d') |
        is_eq [d as ref (STRING s), d' as ref (STRING s')] = (d=d') |
        is_eq [d as ref (PROCEDURE p), d' as ref (PROCEDURE p')] = (d=d') |
        is_eq [ref (LIST nil), ref (LIST nil)] = true |
        is_eq [d as ref (LIST l), d' as ref (LIST l')] = (d=d') |
        is_eq [_,_] = false |
	is_eq _ = wrong_args ("eq?", 2)
    fun is_equal [ref (BOOL b), ref (BOOL b')] = (b=b') |
        is_equal [ref (SYMBOL s), ref (SYMBOL s')] = (s=s') |
        is_equal [ref (CHAR c), ref (CHAR c')] = (c=c') |
        is_equal [ref (VECTOR v), ref (VECTOR v')] = Vector.eq [v, v'] |
        is_equal [ref (PAIR p1), ref (PAIR p2)] = Pair.eq [p1, p2] |
        is_equal [ref (NUMBER n), ref (NUMBER n')] = Number.eq [n, n'] |
        is_equal [ref (STRING s), ref (STRING s')] = String.eq [s, s'] |
        is_equal [d as ref (PROCEDURE p), d' as ref (PROCEDURE p')] = (d=d') |
        is_equal [ref (LIST nil), ref (LIST nil)] = true |
        is_equal [ref (LIST l), ref (LIST l')] = List.eq [l, l'] |
        is_equal [ref (PAIR (l,r)), ref (LIST (l'::r'))] =
              is_equal [l,l'] andalso is_equal [r, ref (LIST r')] |
        is_equal [ref (LIST (l::r)), ref (PAIR (l',r'))] =
              is_equal [l,l'] andalso is_equal [ref (LIST r), r'] |
        is_equal [_,_] = false |
	is_equal _ = wrong_args ("equal?", 2)
    val bindings = [("eqv?", is_eqv), ("eq?", is_eq), ("equal?", is_equal)]
    end

  structure Pair =
    struct
    fun is_pair [ref (PAIR _)] = Bool.tt
      | is_pair [_] = Bool.ff
      | is_pair _ = wrong_args ("pair?", 1)
    fun cons [v1, v2] = PAIR_TAG (v1, v2)
      | cons _ = wrong_args ("cons", 2)
    fun car [ref (PAIR (v1, v2))] = v1
      | car [v] = type_error "car" v
      | car _ = wrong_args ("car", 1)
    fun cdr [ref (PAIR (v1, v2))] = v2
      | cdr [v] = type_error "cdr" v
      | cdr _ = wrong_args ("cdr", 1)
    fun set_car [ref (PAIR (v1, v2)), v3] = v1 := !v3
      | set_car [v,_] = type_error "set-car!" v
      | set_car _ = wrong_args ("set_car!", 2)
    fun set_cdr [ref (PAIR (v1, v2)), v3] = v2 := !v3
      | set_cdr [v,_] = type_error "set-cdr!" v
      | set_cdr _ = wrong_args ("set-cdr!", 2)
    fun caar [x] = car [car [x]]
      | caar _ = wrong_args ("caar", 1)
    fun cadr [x] = car [cdr [x]]
      | cadr _ = wrong_args ("cadr", 1)
    fun cdar [x] = cdr [car [x]]
      | cdar _ = wrong_args ("cdar", 1)
    fun cddr [x] = cdr [cdr [x]]
      | cddr _ = wrong_args ("cddr", 1)
    fun caaar [x] = car [caar [x]]
      | caaar _ = wrong_args ("caaar", 1)
    fun caadr [x] = car [cadr [x]]
      | caadr _ = wrong_args ("caadr", 1)
    fun cadar [x] = car [cdar [x]]
      | cadar _ = wrong_args ("cadar", 1)
    fun caddr [x] = car [cddr [x]]
      | caddr _ = wrong_args ("caddr", 1)
    fun cdaar [x] = cdr [caar [x]]
      | cdaar _ = wrong_args ("cdaar", 1)
    fun cdadr [x] = cdr [cadr [x]]
      | cdadr _ = wrong_args ("cdadr", 1)
    fun cddar [x] = cdr [cdar [x]]
      | cddar _ = wrong_args ("cddar", 1)
    fun cdddr [x] = cdr [cddr [x]]
      | cdddr _ = wrong_args ("cdddr", 1)
    fun caaaar [x] = car [caaar [x]]
      | caaaar _ = wrong_args ("caaaar", 1)
    fun caaadr [x] = car [caadr [x]]
      | caaadr _ = wrong_args ("caaadr", 1)
    fun caadar [x] = car [cadar [x]]
      | caadar _ = wrong_args ("caadar", 1)
    fun caaddr [x] = car [caddr [x]]
      | caaddr _ = wrong_args ("caaddr", 1)
    fun cadaar [x] = car [cdaar [x]]
      | cadaar _ = wrong_args ("cadaar", 1)
    fun cadadr [x] = car [cdadr [x]]
      | cadadr _ = wrong_args ("cadadr", 1)
    fun caddar [x] = car [cddar [x]]
      | caddar _ = wrong_args ("caddar", 1)
    fun cadddr [x] = car [cdddr [x]]
      | cadddr _ = wrong_args ("cadddr", 1)
    fun cdaaar [x] = cdr [caaar [x]]
      | cdaaar _ = wrong_args ("cdaaar", 1)
    fun cdaadr [x] = cdr [caadr [x]]
      | cdaadr _ = wrong_args ("cdaadr", 1)
    fun cdadar [x] = cdr [cadar [x]]
      | cdadar _ = wrong_args ("cdadar", 1)
    fun cdaddr [x] = cdr [caddr [x]]
      | cdaddr _ = wrong_args ("cdaddr", 1)
    fun cddaar [x] = cdr [cdaar [x]]
      | cddaar _ = wrong_args ("cddaar", 1)
    fun cddadr [x] = cdr [cdadr [x]]
      | cddadr _ = wrong_args ("cddadr", 1)
    fun cdddar [x] = cdr [cddar [x]]
      | cdddar _ = wrong_args ("cdddar", 1)
    fun cddddr [x] = cdr [cdddr [x]]
      | cddddr _ = wrong_args ("cddddr", 1)
    val bindings = 
        [("pair?", is_pair), ("cons", cons), ("car", car), ("cdr", cdr),
	("set-car!", set_car), ("set-cdr!", set_cdr),
	("caar", caar), ("cadr", cadr), ("cdar", cdar), ("cddr", cddr),
	("caaar", caaar), ("caadr", caadr), ("cadar", cadar), ("caddr", caddr),
	("cdaar", cdaar), ("cdadr", cdadr), ("cddar", cddar), ("cdddr", cdddr),
	("caaaar", caaaar), ("caaadr", caaadr), ("caadar", caadar),
	("caaddr", caaddr), ("cadaar", cadaar), ("cadadr", cadadr),
	("caddar", caddar), ("cadddr", cadddr), ("cdaaar", cdaaar),
	("cdaadr", cdaadr), ("cdadar", cdadar), ("cdaddr", cdaddr),
	("cddaar", cddaar), ("cddadr", cddadr), ("cdddar", cdddar),
	("cddddr", cddddr)]
    end

  structure List =
    struct
    fun is_null [ref (LIST nil)] = Bool.tt
      | is_null [_] = Bool.ff
      | is_null _ = wrong_args ("null?", 1)
    fun is_list l [ref (LIST _)] = Bool.tt
      | is_list l [v as (ref (PAIR (_, v')))] = 
	   BOOL_TAG (not (member v l) andalso list (v::l) v')
      | is_list l [_] = Bool.ff
      | is_list l _ = wrong_args ("list?", 1)
    val list = LIST_TAG
    fun append nil = LIST_TAG nil
      | append (ref (LIST nil) :: vr) = append vr
      | append (ref (LIST (a::b)) :: vr) = 
	  PAIR_TAG (a, append (LIST_TAG b :: vr))
      | append (ref (PAIR (a, b)) :: vr) =
	  PAIR_TAG (a, append (b :: vr))
      | append v = type_error "append" v
    fun reverse [v] = 
	let fun lreverse (ref (LIST l), r) = rev l @ r
	      | lreverse (ref (PAIR (a, b)), r) = lreverse (b, a::r)
	      | lreverse _ = type_error "reverse" v
        in LIST_TAG (lreverse (v, nil))
        end
      | reverse _ = wrong_args ("reverse", 1)
    fun list_ref (v as [l, ref (NUMBER n)]) = 
        let fun list_ref_checked (a::r, 0) = a |
	        list_ref_checked (a::r, n) = list_ref_checked (r, n-1) |
	        list_ref_checked (nil, _) = input_error "list-ref" v
        in if n < 0 
              then input_error "list_ref" v 
  	   else list_ref_checked (LIST_UNTAG l, n) handle 
		  TypeError _ => type_error "list-ref" l
        end
      | list_ref [_, n] = type_error "list-ref" n
      | list_ref _ = wrong_args ("list-ref", 2)
    local 
	exception NotFound
 	fun mem eq (obj, nil) = raise NotFound
          | mem eq (obj, v as (a::b)) = 
		if eq [obj, a] then v else mem eq (obj, b)
    in
    fun memq [obj, list] = 
	(LIST_TAG (mem EquivPred.is_eq (obj, LIST_UNTAG list)) handle
	    TypeError _ => type_error "memq" list
          | NotFound => Bool.ff)
      | memq _ = wrong_args ("memq", 2)
    fun memv [obj, list] = 
	(LIST_TAG (mem EquivPred.is_eqv (obj, LIST_UNTAG list)) handle
	    TypeError _ => type_error "memv" list
          | NotFound => Bool.ff)
      | memv _ = wrong_args ("memv", 2)
    fun member [obj, list] = 
	(LIST_TAG (mem EquivPred.is_equal (obj, LIST_UNTAG list)) handle
	    TypeError _ => type_error "member" list
          | NotFound => Bool.ff)
      | member _ = wrong_args ("member", 2)
    end
    local 
	exception NotFound
	fun ass eq (obj, nil) = raise NotFound
	  | ass eq (obj, (p as (k,v)::b)) = 
		if eq [obj, k] then p else ass eq (obj,b)
    in
    fun assq [obj, alist] =
	let val l = UNTAG_LIST alist
	    fun is_alist nil = true
	      | is_alist (ref (PAIR _) :: r) = is_alist r
	      | is_alist (ref (LIST (a::b)) :: r) = is_alist r
              | is_alist _ = false
	    	  
