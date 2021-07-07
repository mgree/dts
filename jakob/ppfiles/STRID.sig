(* Structure identifiers *)

(*
$File: Common/STRID.sml $
$Date: 1992/01/29 15:05:43 $
$Revision: 1.11 $
$Locker: birkedal $
*)
(*$STRID*)
signature STRID =
  sig
    eqtype strid
    type longstrid

    val mk_StrId: string -> strid		(* NEW PARSER *)
    val mk_LongStrId: string list -> longstrid	(* NEW PARSER *)
    val inventStrId: unit -> strid		(* NEW PARSER *)
    val longStrIdOfStrId: strid -> longstrid	(* NEW PARSER *)

    val implode_longstrid : strid list * strid -> longstrid
    and explode_longstrid : longstrid -> strid list * strid
					(* MEMO: elsewhere we use the
					   name `decompose' for this kind
					   of thing. *)

    val pr_StrId: strid -> string
    val pr_LongStrId: longstrid -> string

   (* Needed for top-level printing: *)
    val < : strid * strid -> bool
  end;
