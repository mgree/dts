(* Reporting of errors, binding, and so on. Tidier than the stuff
   generated by the pretty-printer. *)

(*
$File: Common/REPORT.sml $
$Date: 1992/02/13 11:32:31 $
$Revision: 1.2 $
$Locker: birkedal $
*)

(*$REPORT*)
signature REPORT =
  sig
    type Report				(* Some lines of text for the user. *)
    val null: Report			(* Nothing to report. *)
    val line: string -> Report		(* One line of text. *)
    val // : Report * Report -> Report
    val flatten: Report list -> Report
    val indent: int * Report -> Report
    val decorate: string * Report -> Report
					(* A flashy indent which puts a str
					   in front of the first line and
					   indents the rest. *)
    val print: Report -> unit		(* Output. *)
  end;
