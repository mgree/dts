(*$ SCHEMESYMBOL *)

signature SCHEMESYMBOL =
sig

(* SYMBOLS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "symbol".

*)

(* TYPES *)

type symbol
  
(* STANDARD PROCEDURES *)

val symbol_eq: symbol * symbol -> bool

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

val str2symbol: string -> symbol
val symbol2str: symbol -> string

end

