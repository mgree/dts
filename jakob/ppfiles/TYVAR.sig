(* type variables - Definition v3 page 4 *)

(*
$File: Common/TYVAR.sml $
$Date: 1992/03/09 15:02:30 $
$Revision: 1.8 $
$Locker: birkedal $
*)

(*$TYVAR*)
signature TYVAR = 
  sig 
    eqtype SyntaxTyVar

    val mk_TyVar: string -> SyntaxTyVar	(* NEW PARSER *)
    and pr_tyvar: SyntaxTyVar -> string

    val isEquality: SyntaxTyVar -> bool
    and isImperative: SyntaxTyVar -> bool
  end;
