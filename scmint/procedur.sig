(*$ SCHEMEPROCEDURE *)

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
(* 
   nonfix ->
   sharing type procedure = ->
   infix -> 
*)
type 'a slist

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

(* CONSTRUCTORS *)

val procedure: ('a -> 'b) -> ('a, 'b) procedure
val --> : ('a -> 'b) * ('c -> 'd) -> 
	('b, 'c) procedure -> ('a, 'd) procedure


(* STANDARD PROCEDURES *)

exception UnequalListLength of string
exception EmptyArgumentList of string

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
end
