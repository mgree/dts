(*
$File: Common/Timestamp.sml $
$Date: 1995/04/17 07:55:32 $
$Revision: 1.2 $
$Locker:  $
*)

(*$Timestamp: TIMESTAMP*)
functor Timestamp(): TIMESTAMP =
  struct
    type stamp = int

    val r = ref 0
    fun new() = (r := !r + 1; !r)

    fun print i = "$" ^ Int.string i
  end;
