(* exception constructors - Definition v3 page 4 *)

(*
$File: Common/Excon.sml $
$Date: 1992/08/19 14:45:48 $
$Revision: 1.11 $
$Locker: birkedal $
*)

(*$Excon: IDENT EXCON*)
functor Excon(structure Ident: IDENT): EXCON =
  struct
    type id     = Ident.id
    type longid = Ident.longid
    type strid  = Ident.strid

    datatype longexcon = LONGEXCON of longid

    fun mk_longexcon longid = (LONGEXCON longid)
    and pr_longexcon (LONGEXCON longid) = Ident.pr_longid longid

    datatype excon = EXCON of id
    type KnownExcon = excon

    fun mk_excon id = (EXCON id)
    and un_excon (EXCON id) = id
    and pr_excon (EXCON id) = Ident.pr_id id

    fun decompose (LONGEXCON longid) =
      let
	val (strid_list, id) = Ident.decompose longid
      in
	(strid_list, EXCON id)
      end

    val bogus = mk_longexcon Ident.bogus

    val mk_ExCon = mk_excon o Ident.mk_Id

    val ex_ABS  = mk_ExCon "Abs"
    val ex_NEG  = mk_ExCon "Neg"
    val ex_SUM  = mk_ExCon "Sum"
    val ex_DIFF = mk_ExCon "Diff"
    val ex_PROD = mk_ExCon "Prod"

    fun (EXCON id1) < (EXCON id2) = Ident.<(id1, id2)
  end;
