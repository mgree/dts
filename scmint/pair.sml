(*$SCHEMEPAIR *)

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
(* sharing type pair = * -- NOTE: This cannot be written in Standard ML! *)

(* CONSTANTS, PROCEDURES *)

val cons: 'a * 'b -> ('a, 'b) pair
(* infix || *)
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


(*$SchemePair: SCHEMEPAIR *)

structure SchemePair: SCHEMEPAIR =
  struct

  type ('a, 'b) pair = 'a * 'b

  fun cons (p: ('a, 'b) pair) = p
  infix ||
  fun (f || g) (x, y) = (f x, g y)

  fun car (x, _) = x
  fun cdr (_, y) = y
  
  fun set_car ((x, _), z) = (x := z)
  fun set_cdr ((_, y), z) = (y := z)
  
  fun caar x = car (car x)
  fun cadr x = car (cdr x)
  fun cdar x = cdr (car x)
  fun cddr x = cdr (cdr x)
  fun caaar x = car (caar x)
  fun caadr x = car (cadr x)
  fun cadar x = car (cdar x)
  fun caddr x = car (cddr x)
  fun cdaar x = cdr (caar x)
  fun cdadr x = cdr (cadr x)
  fun cddar x = cdr (cdar x)
  fun cdddr x = cdr (cddr x)
  fun caaaar x = car (caaar x)
  fun caaadr x = car (caadr x)
  fun caadar x = car (cadar x)
  fun caaddr x = car (caddr x)
  fun cadaar x = car (cdaar x)
  fun cadadr x = car (cdadr x)
  fun caddar x = car (cddar x)
  fun cadddr x = car (cdddr x)
  fun cdaaar x = cdr (caaar x)
  fun cdaadr x = cdr (caadr x)
  fun cdadar x = cdr (cadar x)
  fun cdaddr x = cdr (caddr x)
  fun cddaar x = cdr (cdaar x)
  fun cddadr x = cdr (cdadr x)
  fun cdddar x = cdr (cddar x)
  fun cddddr x = cdr (cdddr x)

  fun pair_eq (eq1, eq2) ((x1,y1), (x2,y2)) = 
      eq1 (x1,x2) andalso eq2 (y1,y2)
  end
