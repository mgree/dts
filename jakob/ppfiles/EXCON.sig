(* exception constructors - Definition v3 page 4 *)

(*
$File: Common/EXCON.sml $
$Date: 1992/08/19 14:45:33 $
$Revision: 1.11 $
$Locker: birkedal $
*)

(*$EXCON *)

signature EXCON =
  sig 
    type id and longid and strid

    eqtype longexcon

    val mk_longexcon : longid -> longexcon
    and pr_longexcon : longexcon -> string

    eqtype excon

    val mk_excon : id -> excon
    and un_excon : excon -> id
    and pr_excon : excon -> string

    val mk_ExCon: string -> excon	(* NEW PARSER *)

    val decompose : longexcon -> strid list * excon

    val bogus : longexcon

    val ex_ABS : excon
    val ex_NEG : excon
    val ex_SUM : excon
    val ex_DIFF : excon
    val ex_PROD : excon

    val < : excon * excon -> bool	(* Top-level printing. *)
  end;
