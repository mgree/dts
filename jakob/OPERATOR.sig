(*
$File: Common/OPERATOR.sml $
$Date: 1995/04/17 07:55:32 $
$Revision: 1.2 $
$Locker:  $
*)

(*$OPERATOR: IDENT
 *)

signature OPERATOR =
  sig
    structure Ident : IDENT
    type id
    type infixenv
    datatype assoc = LEFT | RIGHT

    exception opNotFound

    val basisenv : infixenv
    
    val isInfix : infixenv -> id -> bool
    val lookupOp : infixenv -> id -> int * assoc
    val registerInfix  : infixenv -> int Option -> id list -> infixenv
    val registerInfixr : infixenv -> int Option -> id list -> infixenv
    val registerNonfix : infixenv -> id list -> infixenv
  end
