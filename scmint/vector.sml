(*$SCHEMEVECTOR *)

signature SCHEMEVECTOR =
sig

(* VECTORS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "vector".

*)

(* TYPES *)

type 'a vector
type natural
type 'a slist

(* CONSTRUCTORS *)

val make_vector: natural * 'a -> 'a vector
val vector: 'a list -> 'a vector
val vector_map: ('a -> 'b) -> 'a vector -> 'b vector

(* STANDARD PROCEDURES *)

val vector_length: 'a vector -> natural
val vector_ref: 'a vector * natural -> 'a
val vector_set: 'a ref vector * natural * 'a -> unit

val vector2list: 'a vector -> 'a slist
val list2vector: 'a slist -> 'a vector

val vector_fill: 'a ref vector * 'a -> unit

(* COMPARISONS *)

val vector_eq: ('a * 'a -> bool) -> 'a vector * 'a vector -> bool

(* INPUT/OUTPUT *)

(*
val read: instream -> (T, T Option) Result
val print: outstream -> T -> unit
*)

end


(*$SchemeVector: SCHEMEVECTOR SchemeGeneral SchemeList *)

structure SchemeVector: SCHEMEVECTOR =
  struct 
  local open SchemeGeneral SchemeList in

  type 'a vector = 'a list
  type natural = int
  type 'a slist = 'a slist
  
  fun make_vector (k, fill) = 
      if k < 0 then 
	  raise IllegalInput ("Negative index in make-vector", makestring k)
      else if k = 0 then nil 
	   else (* k > 0 *)   fill :: make_vector (k-1, fill)
  fun vector (l: 'a list) = l
  val vector_map = map

  val vector_length = length
  val vector_ref = nth
  fun vector_set (v, k, obj) = (nth(v,k) := obj)

  fun vector2list (v: 'a vector) = v
  fun list2vector (l: 'a list) = l

  fun vector_fill (nil, fill) = () |
      vector_fill (a::r, fill) = (a := fill ; vector_fill (r,fill))

  fun vector_eq eq (nil, nil) = true |
      vector_eq eq (a::r, a'::r') = eq (a,a') andalso vector_eq eq (r,r') |
      vector_eq eq (_,_) = false
  end
  end
