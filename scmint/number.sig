(*$ SCHEMENUMBER *)

signature SCHEMENUMBER =
sig

(* NUMBERS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "number".
Caveat: SML integers are used exclusively for numbers!

*)

(* TYPES *)

type number

(* STANDARD PROCEDURES *)

val is_complex: number -> bool
val is_real: number -> bool
val is_rational: number -> bool
val is_integer: number -> bool
val is_exact: number -> bool
val is_inexact: number -> bool

val number_eq: number list -> bool
val number_lt: number list -> bool
val number_gt: number list -> bool
val number_le: number list -> bool
val number_ge: number list -> bool

val is_zero: number -> bool
val is_positive: number -> bool
val is_negative: number -> bool
val is_odd: number -> bool
val is_even: number -> bool

exception UndefinedNumber of string

val max: number list -> number
val min: number list -> number

val plus: number list -> number
val mult: number list -> number
val minus: number * number list -> number
val divide: number * number list -> number
val abs: number -> number

val quotient: number * number -> number
val remainder: number * number -> number
val modulo: number * number -> number

val gcd: number list -> number
val lcm: number list -> number

val floor: number -> number
val ceiling: number -> number
val truncate: number -> number
val round: number -> number

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

exception IllegalDigit of int

val str2number: string -> number
val number2str: number -> string

end
