(*
$File: Common/Crash.sml $
$Date: 1995/04/17 07:55:32 $
$Revision: 1.2 $
$Locker:  $
*)
(*$Crash : BASIC_IO CRASH*)

functor Crash(structure BasicIO: BASIC_IO): CRASH =
  struct
    exception CRASH

    fun impossible msg =
      let
	val msg = "Impossible: " ^ msg
      in
	BasicIO.println msg;
	raise CRASH
      end

    fun assert(msg, condition) =
      if condition then ()
      else
	let
	  val msg = "Assert fails: " ^ msg
	in
	  BasicIO.println msg;
	  raise CRASH
	end

    fun unimplemented msg =
      let
	val msg = "Unimplemented: " ^ msg
      in
	BasicIO.println msg;
	raise CRASH
      end
  end;
