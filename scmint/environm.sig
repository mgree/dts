(*$ SCHEMEENVIRONMENT *)

signature SCHEMEENVIRONMENT =
sig

(* ENVIRONMENT

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for environments, including top-level environment

*)

(* TYPES *)

eqtype key
type value
type env

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

(* CONSTRUCTORS *)

exception DoubleKey of key

val init_top_env: unit -> unit
val top_env: env ref
val empty_env: env
val add: key * value * env -> env
val add_top: key * value -> unit
val push: (key * value) list * env -> env

(* OBSERVERS *)

exception Lookup of key

val lookup: key * env -> value
val lookup_top: key -> value

end
