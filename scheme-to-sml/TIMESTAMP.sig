(*
$File: Common/TIMESTAMP.sml $
$Date: 1995/04/17 07:55:32 $
$Revision: 1.2 $
$Locker:  $
*)

(*$TIMESTAMP*)
signature TIMESTAMP =
  sig
    eqtype stamp
    val new: unit -> stamp
    val print: stamp -> string
  end;
