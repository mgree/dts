(* type variables - Definition v3 page 4 *)

(*
$File: Common/TYVAR.sml $
$Date: 1995/04/17 07:55:32 $
$Revision: 1.2 $
$Locker:  $
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
