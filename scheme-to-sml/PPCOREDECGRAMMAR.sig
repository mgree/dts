
(*
$File: Common/PPDECGRAMMAR.sml $
$Date: 1995/04/17 07:55:32 $
$Revision: 1.2 $
$Locker:  $
*)

(*$PPDECGRAMMAR: DEC_GRAMMAR*)

(* Some of the layout routines are needed by the modules level prettyprinter
   and/or the compiler. *)

signature PPDECGRAMMAR =
  sig
    structure G: DEC_GRAMMAR
    type StringTree

    val layoutTyvarseq: G.tyvar list -> StringTree Option
    val layoutTy:	G.ty	     -> StringTree
    val layoutAtpat:	G.atpat	     -> StringTree
    val layoutPat:	G.pat	     -> StringTree
    val layoutExp:	G.exp	     -> StringTree
    val layoutDec:      G.dec	     -> StringTree
  end;
