(* special constants - Definition v3 page 3 *)

(*
$File: Common/SCON.sml $
$Date: 1991/11/08 13:20:47 $
$Revision: 1.6 $
$Locker: birkedal $
*)

(*$SCON*)
(* I'd like two views of SCON, one with the datatype hidden, but that seems
   to cause Poly/ML problems with the local/sharing/open stuff. *)

signature SCON =    
sig 
  datatype scon = INTEGER of int
  		| STRING of string
		| REAL of real

  val pr_scon: scon -> string
end;
