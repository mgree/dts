(*$ SCHEMELIST *)

signature SCHEMELIST =
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
val list_map: ('a -> 'b) -> 'a slist -> 'b slist

(* STANDARD PROCEDURES *)

val is_empty: 'a slist -> bool
val length: 'a slist -> int
val append: 'a slist list -> 'a slist
val reverse: 'a slist -> 'a slist
exception EmptyList of string
exception NegativeIndex of string * int
val list_tail: 'a slist * int -> 'a slist
val list_ref: 'a slist * int -> 'a
val mem: ('a * 'a -> bool) -> 'a * 'a slist -> 'a slist
val ass: ('a * 'a -> bool) -> 'a * ('a * 'b) slist -> 'a * 'b

(* EQUALITY PREDICATE *)

val list_eq: ('a * 'a -> bool) -> 'a slist * 'a slist -> bool

(* CONVERSIONS *)

val list2bool: 'a slist -> bool
val slist2list: 'a slist -> 'a list

end
