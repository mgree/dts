(*
$File: Common/OPERATOR.sml $
$Date: 23.02.95 $
$Revision:  $
$Locker: panic@diku.dk $
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
