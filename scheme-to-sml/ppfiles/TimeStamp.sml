(*
$File: Common/Timestamp.sml $
$Date: 1992/06/16 16:48:37 $
$Revision: 1.1 $
$Locker: birkedal $
*)

(*$Timestamp: TIMESTAMP*)
functor Timestamp(): TIMESTAMP =
  struct
    type stamp = int

    val r = ref 0
    fun new() = (r := !r + 1; !r)

    fun print i = "$" ^ Int.string i
  end;
