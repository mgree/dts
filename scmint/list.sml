(*$SCHEMELIST *)

signature LIST =
sig

(* LISTS


Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "list".

*)

(* TYPES *)

type 'a slist

(* CONSTRUCTORS *)

val list: 'a list -> 'a slist

(* STANDARD PROCEDURES *)

val is_empty: 'a slist -> bool
val length: 'a slist -> int
val append: 'a slist list -> 'a slist
val reverse: 'a slist -> 'a slist
val list_tail: 'a slist * int -> 'a slist
val list_ref: 'a slist * int -> 'a
val mem: ('a * 'a -> bool) -> 'a * 'a slist -> 'a slist
val ass: ('a * 'a -> bool) -> 'a * ('a * 'b) slist -> ('a * 'b) General.Option

(* EQUALITY PREDICATE *)

val eq: ('a * 'a -> bool) -> 'a slist * 'a slist -> bool

(* CONVERSIONS *)

val slist2list: 'a slist -> 'a list

end


structure List: LIST =
  struct 
  open General

  type 'a slist = 'a list 

  fun list (l: 'a list) = l

  val is_empty = null

  val length = length
  fun append nil = nil |
      append [a] = a |
      append (nil::r) = append r |
      append ((a::l)::r) = a :: append(l::r)
  val reverse = rev
  fun list_tail (l, n) =
      let fun list_tail_checked (l, 0) = l |
	      list_tail_checked (a::r, n) = list_tail_checked (r, n-1) |
              list_tail_checked (nil, _) = 
		raise IllegalInput ("Empty list argument in list-tail", "")
      in if n < 0 
	    then raise IllegalInput ("Negative index in list-tail", makestring n)
	 else list_tail_checked (l, n)
      end
  fun list_ref (l, n) =
      let fun list_ref_checked (a::r, 0) = a |
	      list_ref_checked (a::r, n) = list_ref_checked (r, n-1) |
	      list_ref_checked (nil, _) = raise IllegalInput ("Empty list argument in list-ref", "")
      in if n < 0 then raise IllegalInput ("Negative index in list-ref", makestring n)
	 else list_ref_checked (l, n)
      end
  fun mem eq (obj, l as a :: r) = if eq (obj, a) then l else mem eq (obj, r) |
      mem eq (obj, nil) = nil
  fun ass eq (obj, (p as (a, b)) :: r) = if eq (obj, a) then Some p else
  					 ass eq (obj, r) |
      ass eq (obj, nil) = None

  fun eq eq' (nil, nil) =  true |
      eq eq' (a::r, a'::r') = eq' (a,a') andalso list_eq eq' (r,r') |
      eq eq' _ = false

  fun slist2list (l: 'a slist) = l
  end 
