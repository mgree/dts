(* Identifiers - variables or constructors *)

(*
$File: Common/IDENT.sml $
$Date: 1992/08/19 14:45:57 $
$Revision: 1.18 $
$Locker: birkedal $
*)

(*$IDENT*)
signature IDENT =
  sig
    eqtype strid

    eqtype longid
    val pr_longid : longid -> string

    eqtype id
    val inventId: unit -> id
    and pr_id : id -> string

    val mk_Id: string -> id			(* NEW PARSER *)
    val mk_LongId: string list -> longid	(* NEW PARSER *)
    val inventLongId: unit -> longid		(* NEW PARSER *)
    val idToLongId: id -> longid

    val unqualified: longid -> bool
    val decompose: longid -> strid list * id
    val decompose0: longid -> id

    val < : id * id -> bool


   (* Identifiers needed for derived form conversion. *)

    val id_NIL: id
    val id_CONS: id
    val id_TRUE: id
    val id_FALSE: id
    val id_REF: id
    val id_PRIM: id
    val id_IT: id

    (* Identifiers needed for initial environment - because of overloading *)
    val id_ABS: id
    val id_NEG: id
    val id_PLUS: id
    val id_MINUS: id
    val id_MUL: id
    val id_LESS: id
    val id_GREATER: id
    val id_LESSEQ: id
    val id_GREATEREQ: id


   (* Bogus identifier *)
    val bogus: longid
  end;
