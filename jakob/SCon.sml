(* special constants - Definition v3 page 3 *)

(*
$File: Common/SCon.sml $
$Date: 1995/04/17 07:55:32 $
$Revision: 1.2 $
$Locker:  $
*)

(*$SCon : SCON*)

functor SCon(): SCON =
struct
  datatype scon = INTEGER of int | STRING of string | REAL of real

  fun pr_scon(INTEGER i) = Int.string i
   |  pr_scon(STRING s) = String.string s
   |  pr_scon(REAL r) = Real.string r
end;
