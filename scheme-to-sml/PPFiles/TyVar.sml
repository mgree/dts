(* Type variables - Definition v3 page ?? *)

(*
$File: Common/TyVar.sml $
$Date: 1992/04/07 14:57:13 $
$Revision: 1.8 $
$Locker: birkedal $
*)

(*$TyVar: CRASH TYVAR*)
functor TyVar(structure Crash: CRASH): TYVAR =
  struct
    datatype SyntaxTyVar = TYVAR of string

    val mk_TyVar = TYVAR
    fun pr_tyvar(TYVAR str) = str

    local
      fun snd(TYVAR str) =
	StringListOps.nth 1 str
        handle StringListOps.Subscript _ => Crash.impossible "TyVar.snd"
    in
      fun isEquality tv = (snd tv = "'")
      fun isImperative tv = (snd tv = "_")
    end
  end;
