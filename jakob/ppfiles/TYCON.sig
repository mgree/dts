(* type constructors - Definition v3 page 4 *)

(*
$File: Common/TYCON.sml $
$Date: 1992/11/10 17:01:26 $
$Revision: 1.10 $
$Locker: birkedal $
*)

(*$TYCON*)
signature TYCON =
  sig
    eqtype strid
    eqtype tycon
    eqtype longtycon

    val mk_TyCon: string -> tycon		(* NEW PARSER *)
    val mk_LongTyCon: string list -> longtycon	(* NEW PARSER *)

    val implode_LongTyCon : strid list * tycon -> longtycon
    and explode_LongTyCon : longtycon -> strid list * tycon

    val tycon_INT    : tycon
    and tycon_REAL   : tycon
    and tycon_STRING : tycon
    and tycon_EXN    : tycon
    and tycon_REF    : tycon
    and tycon_BOOL   : tycon

    val pr_TyCon : tycon -> string
    val pr_LongTyCon : longtycon  -> string

    val < : tycon * tycon -> bool		(* Needed to order
						   top-level printout. *)
  end;
