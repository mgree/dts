(*$ SCHEMEVECTOR *)

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
type number
type 'a slist

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

(* CONSTRUCTORS *)

val make_vector: number * 'a -> 'a vector
val vector: 'a list -> 'a vector
val vector_map: ('a -> 'b) -> 'a vector -> 'b vector

(* STANDARD PROCEDURES *)

val vector_length: 'a vector -> number
val vector_ref: 'a vector * number -> 'a
val vector_set: 'a ref vector * number * 'a -> unit

val vector2list: 'a vector -> 'a slist
val list2vector: 'a slist -> 'a vector

val vector_fill: 'a ref vector * 'a -> unit

(* COMPARISONS *)

val vector_eq: ('a * 'a -> bool) -> 'a vector * 'a vector -> bool

end
