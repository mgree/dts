(*$SCHEMEPROCEDURE *)

signature SCHEMEPROCEDURE =
sig

(* CONTROL FEATURES

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Control features of Scheme.

*)

(* TYPES *)

type ('a, 'b) procedure 
type 'a slist
(* sharing type procedure = ->  NOTE: This is not legal Standard ML! *)

(* CONSTRUCTORS *)

val --> : ('a, 'b) procedure * ('c, 'd) procedure -> 
	('b, 'c) procedure -> ('a, 'd) procedure


(* STANDARD PROCEDURES *)

val apply: ('a list, 'b) procedure * 'a slist -> 'b
val map1: ('a, 'b) procedure * 'a slist -> 'b slist
val map2: ('a * 'b, 'c) procedure * 'a slist * 'b slist -> 'c slist
val mapn: ('a list, 'b) procedure * 'a slist list -> 'b slist
val for_each1: ('a, 'b) procedure * 'a slist -> unit
val for_each2: ('a * 'b, 'c) procedure * 'a slist * 'b slist -> unit
val for_each: ('a list, 'b) procedure * 'a slist list -> unit
val delay: 'b -> (unit, 'b) procedure
val force: (unit, 'b) procedure -> 'b
val call_with_current_continuation: 
	(('_a, '_b) procedure, '_a) procedure -> '_a 

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

end


(*$SchemeProcedure: SCHEMEPROCEDURE SchemeList SchemeGeneral *)

structure SchemeProcedure: SCHEMEPROCEDURE = 
  struct 
  local open SchemeGeneral SchemeList in

  type ('a, 'b) procedure = 'a -> 'b
  type 'a slist = 'a slist

  infixr -->
  fun (g --> h) f x = h (f (g x))

  fun apply (p, l) = p l
  fun map1 (p, l) = map p l
  fun map2 (p, nil, nil) = nil |
      map2 (p, a::l, a'::l') = p(a,a') :: map2 (p, l, l') |
      map2 _ = raise IllegalInput ("Lists of unequal lengths in map2", "")
  fun mapn (p, nil) = raise IllegalInput ("No argument list given in map", "") | 
      mapn (p, [l]) = map1 (fn x => p [x], l) |
      mapn (p, [l,l']) = map2 (fn (x,y) => p [x,y], l, l') |
      mapn (p, (l::l'::rl)) = raise Unimplemented "mapn"
  fun for_each1 (p, l) = app p l
  fun for_each2 (p, nil, nil) = () |
      for_each2 (p, a::l, a'::l') = (p(a,a') ; for_each2 (p, l, l')) |
      for_each2 _ = raise IllegalInput ("Lists of unequal lengths in for-each2", "")
  fun for_each (p, nil) = raise IllegalInput ("No argument list given in for-each", "") |
      for_each (p, [l]) = for_each1 (fn x => p [x], l) |
      for_each (p, [l,l']) = for_each2 (fn (x,y) => p [x,y], l, l') |
      for_each (p, (l::l'::rl)) = raise Unimplemented "for_each"
  fun delay v () = v
  fun force v = v ()
  fun callcc _ = raise Unimplemented "callcc"
  fun throw _ = raise Unimplemented "throw"
  fun call_with_current_continuation p = callcc (p o throw) 
  end
  end
