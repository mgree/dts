(*$ SCHEMEGENERAL *)

signature SCHEMEGENERAL =
sig

(* GENERAL

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

General utility types and routines for Scheme system

*)

(* TYPES *)

datatype 'a option = NONE | SOME of 'a

(* DESTRUCTORS *)

val check: 'a -> ('b -> 'a) -> 'b option -> 'a

exception Unimplemented of string

val foldappend: 'a list list -> 'a list

end



