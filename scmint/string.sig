(*$ SCHEMESTRING *)

signature SCHEMESTRING =
sig

(* STRINGS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "string".

*)

(* TYPES *)

type sstring
type char
type number
type symbol
type 'a slist

(* CONSTRUCTORS *)

exception StringIndex of string * int
exception FixedString

val make_string: int * char -> sstring
val string: char list -> sstring

(* STANDARD PROCEDURES *)

val string_length: sstring -> int
val string_ref: sstring * int -> char
val string_set: sstring * int * char -> unit
val string_eq: sstring * sstring -> bool
val string_ci_eq: sstring * sstring -> bool
val string_lt: sstring * sstring -> bool
val string_gt: sstring * sstring -> bool
val string_le: sstring * sstring -> bool
val string_ge: sstring * sstring -> bool
val string_ci_lt: sstring * sstring -> bool
val string_ci_gt: sstring * sstring -> bool
val string_ci_le: sstring * sstring -> bool
val string_ci_ge: sstring * sstring -> bool
val substring: sstring * int * int -> sstring
val string_append: sstring list -> sstring
val string_copy: sstring -> sstring
val string_fill: sstring * char -> unit

(* CONVERSIONS *)

val string2list: sstring -> char slist
val list2string: char slist -> sstring
val symbol2string: symbol -> sstring
val string2symbol: sstring -> symbol
exception IllegalBase of int
exception IllegalCharacter of string
val number2string: number * int -> sstring
val string2number: sstring * int -> number

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

val str2sstring: string -> sstring
val sstring2str: sstring -> string

end

