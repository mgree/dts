(*$ SCHEMEPAIR *)

signature SCHEMEPAIR =
sig

(* PAIRS


Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "pair".

*)

(* TYPES *)

type ('a, 'b) pair

(* CONSTANTS, PROCEDURES *)

val cons: 'a * 'b -> ('a, 'b) pair
infix ||
val || : ('a -> 'b) * ('c -> 'd) -> ('a, 'c) pair -> ('b, 'd) pair

val car: ('a, 'b) pair -> 'a
val cdr: ('a, 'b) pair -> 'b

val set_car: ('a ref, 'b) pair * 'a -> unit
val set_cdr: ('a, 'b ref) pair * 'b -> unit

val caar: (('a, 'b) pair, 'c) pair -> 'a
val cadr: ('a, ('b, 'c) pair) pair -> 'b
val cdar: (('a, 'b) pair, 'c) pair -> 'b
val cddr: ('a, ('b, 'c) pair) pair -> 'c
val caaar: ((('a, 'b) pair, 'c) pair, 'd) pair -> 'a
val caadr: ('a, (('b, 'c) pair, 'd) pair) pair -> 'b
val cadar: (('a, ('b, 'c) pair) pair, 'd) pair -> 'b
val caddr: ('a, ('b, ('c, 'd) pair) pair) pair -> 'c
val cdaar: ((('a, 'b) pair, 'c) pair, 'd) pair -> 'b
val cdadr: ('a, (('b, 'c) pair, 'd) pair) pair -> 'c
val cddar: (('a, ('b, 'c) pair) pair, 'd) pair -> 'c
val cdddr: ('a, ('b, ('c, 'd) pair) pair) pair -> 'd
val caaaar: (((('a, 'b) pair, 'c) pair, 'd) pair, 'e) pair -> 'a
val caaadr: ('a, ((('b, 'c) pair, 'd) pair, 'e) pair) pair -> 'b
val caadar: (('a, (('b, 'c) pair, 'd) pair) pair, 'e) pair -> 'b
val caaddr: ('a, ('b, (('c, 'd) pair, 'e) pair) pair) pair -> 'c
val cadaar: ((('a, ('b, 'c) pair) pair, 'd) pair, 'e) pair -> 'b
val cadadr: ('a, (('b, ('c, 'd) pair) pair, 'e) pair) pair -> 'c
val caddar: (('a, ('b, ('c, 'd) pair) pair) pair, 'e) pair -> 'c
val cadddr: ('a, ('b, ('c, ('d, 'e) pair) pair) pair) pair -> 'd
val cdaaar: (((('a, 'b) pair, 'c) pair, 'd) pair, 'e) pair -> 'b
val cdaadr: ('a, ((('b, 'c) pair, 'd) pair, 'e) pair) pair -> 'c
val cdadar: (('a, (('b, 'c) pair, 'd) pair) pair, 'e) pair -> 'c
val cdaddr: ('a, ('b, (('c, 'd) pair, 'e) pair) pair) pair -> 'd
val cddaar: ((('a, ('b, 'c) pair) pair, 'd) pair, 'e) pair -> 'c
val cddadr: ('a, (('b, ('c, 'd) pair) pair, 'e) pair) pair -> 'd
val cdddar: (('a, ('b, ('c, 'd) pair) pair) pair, 'e) pair -> 'd
val cddddr: ('a, ('b, ('c, ('d, 'e) pair) pair) pair) pair -> 'e

(* EQUALITY PREDICATE *)

val pair_eq: ('a * 'a -> bool) * ('b * 'b -> bool) -> 
	('a, 'b) pair * ('a, 'b) pair -> bool
end
