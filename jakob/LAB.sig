(* Labels - Definition v3 page 4 *)

(*
$File: Common/LAB.sml $
$Date: 1995/04/17 07:55:32 $
$Revision: 1.2 $
$Locker:  $
*)

(*$LAB*)
signature LAB =
  sig
    eqtype lab

    val mk_IdentLab: string -> lab
    val mk_IntegerLab: int -> lab	(* NEW PARSER *)

    val < : lab * lab -> bool
    val is_LabN: lab * int -> bool	(* Needed when examining records
					   for tupleness. *)

    val pr_Lab: lab -> string
  end;
