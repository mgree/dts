(*
$File: Common/TIMESTAMP.sml $
$Date: 1992/06/16 16:48:22 $
$Revision: 1.1 $
$Locker: birkedal $
*)

(*$TIMESTAMP*)
signature TIMESTAMP =
  sig
    eqtype stamp
    val new: unit -> stamp
    val print: stamp -> string
  end;
