(*
$File: Common/BasicIO.sml $
$Date: 1991/11/13 10:03:57 $
$Revision: 1.5 $
$Locker: birkedal $
*)

(*$BasicIO: BASIC_IO*)
functor BasicIO(): BASIC_IO =
  struct
    val WIDTH = 75

    val len = ref 0
    val dots = ref false

    fun P x = Outstream.output(Outstream.std_out, x) 
                
		(* Qualified names since we've disabled `input' and
		   `output' and so on at top-level. *)

    fun sym s = (if !len = 75 then (P "\n"; len := 0) else ();
                 Outstream.flush Outstream.std_out;
                 P s;
                 len := !len + 1;
                 dots := true)

    fun dot() = sym "."

    fun print s = (if !dots then (P "\n"; len := 0) else ();
		   if !len + size s >= 75 then (P "\n"; len := 0) else ();
                   dots := false;
                   P s;
                   len := !len + size s)

    fun break() = (if !len > 0 then P "\n" else ();
		   len := 0; dots := false)

    fun println s = (print s; P "\n"; len := 0)

    fun withSpace pr = fn x => (pr x; print " ")
    fun withNewline pr = fn x => (pr x; println " ")

    infix before
    fun a before b = a

    fun withDot f a = f a before dot()

   (* Eventually we might have a more abstract I/O mechanism. But, for
      now, we just mirror the standard one. *)

    val open_in = Instream.open_in
    val input = Instream.input
    val close_in = Instream.close_in
  end;
