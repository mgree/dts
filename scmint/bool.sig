(*$ SCHEMEBOOL *)

signature SCHEMEBOOL =
sig

(* BOOLEANS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "boolean".

*)

(* TYPES *)

type bool

(* OBSERVERS *)

val bool_eq: bool * bool -> bool
val not: bool -> bool

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

exception IllegalBoolean of string

val str2bool: string -> bool
val bool2str: bool -> string

end
