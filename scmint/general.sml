(* $Header: /net/fenris/home/fenris/dts/RCS/general.sml,v 1.12 94/01/20 10:55:58 rehof Exp Locker: rehof $ *)


(*$SCHEMEGENERAL *)

signature SCHEMEGENERAL =
sig

(* GENERAL

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

General utility types and routines for the Scheme system.  Duplicates some
of the facilities of the Edinburgh library General 

*)

(* TYPES *)

datatype 'a Option = None | Some of 'a
datatype ('a, 'b) Result = OK of 'a | Fail of 'b
type natural sharing type natural = int

(* EXCEPTIONS *)

exception Unimplemented of string 
     (* argument: name of unimplemented function *)
exception IllegalInput of string * string
     (* argument: (error message, offending argument as a string) *)
exception EOF
     (* Raised on seeing a legal end-of-input *)
exception EXIT
     (* Raised on executing library function exit *)


(* UTILITIES *)

val foldappend: 'a list list -> 'a list
val member: ''a -> ''a list -> bool
end



(*$SchemeGeneral: SCHEMEGENERAL *)

structure SchemeGeneral: SCHEMEGENERAL = 
  struct

  datatype 'a Option = None | Some of 'a
  datatype ('a, 'b) Result = OK of 'a | Fail of 'b
  type natural = int

  exception Unimplemented of string
  exception IllegalInput of string * string 
  exception EOF
  exception EXIT

  fun foldappend ls = fold (fn (l,l') => l @ l') ls nil

  fun member x y =  case y of
                         [] => false
                    |    (y'::tl) =>  if x=y' then true else (member x tl)


  end

