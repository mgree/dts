(*$ SCHEMECHAR *)

signature SCHEMECHAR =
sig

(* CHARS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "character".

*)

(* TYPES *)

type char

(* STANDARD PROCEDURES *)

val char_eq: char * char -> bool
val char_lt: char * char -> bool
val char_gt: char * char -> bool
val char_le: char * char -> bool
val char_ge: char * char -> bool
val char_ci_eq: char * char -> bool
val char_ci_lt: char * char -> bool
val char_ci_gt: char * char -> bool
val char_ci_le: char * char -> bool
val char_ci_ge: char * char -> bool
val is_char_alphabetic: char -> bool
val is_char_numeric: char -> bool
val is_char_whitespace: char -> bool
val is_char_upper_case: char -> bool
val is_char_lower_case: char -> bool
val char_upcase: char -> char
val char_downcase: char -> char

(* CONVERSIONS *)

val char2integer: char -> int
val integer2char: int -> char

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

exception IllegalCharacter of string

val str2char: string -> char
val char2str: char -> string

end
