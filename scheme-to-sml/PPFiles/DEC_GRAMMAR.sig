(*************************************************************)
(* Grammar for Bare language - Definition v3 pages 8,9,70,71 *)
(* modified to have ident in place of con  var and excon     *)
(*************************************************************)

(*
$File: Common/DEC_GRAMMAR.sml $
$Date: 1993/03/05 14:38:10 $
$Revision: 1.14 $
$Locker: birkedal $
*)

(*$DEC_GRAMMAR *)

signature DEC_GRAMMAR =
sig
  type lab        (* labels *)
   and scon       (* special constants *)
   and con        (* constructors *)
   and id         (* identifiers - variables or constructors *)
   and longid     (* long identifiers - variables or constructors *)
  eqtype tyvar    (* type variables *)
   and   tycon    (* type constructors *)
   and   excon    (* exception constructors *)
  type longtycon  (* long type constructors *)
   and longstrid  (* structure identifiers *)

  type info       (* info about the position in the source text, errors etc *)

  datatype 'a op_opt = OP_OPT of 'a * bool
  datatype 'a WithInfo = WITH_INFO of info * 'a

  datatype atexp =
	SCONatexp of info * scon |         
	IDENTatexp of info * longid op_opt |
	RECORDatexp of info * exprow Option |
	LETatexp of info * dec * exp |
	PARatexp of info * exp

  and exprow =
	EXPROW of info * lab * exp * exprow Option

  and exp =
	ATEXPexp of info * atexp |
	APPexp of info * exp * atexp |
	TYPEDexp of info * exp * ty |
	HANDLEexp of info * exp * match |
	RAISEexp of info * exp |
	FNexp of info * match |
	UNRES_INFIXexp of info * atexp list
      
  and match =
        MATCH of info * mrule * match Option

  and mrule =
        MRULE of info * pat * exp

  and dec = 
	VALdec of info * valbind |
	UNRES_FUNdec of info * FValBind |
		(* TEMPORARY: removed when resolving infixes after parsing. *)
	TYPEdec of info * typbind |
	DATATYPEdec of info * datbind |
	ABSTYPEdec of info * datbind * dec |
	EXCEPTIONdec of info * exbind |
	LOCALdec of info * dec * dec |
	OPENdec of info * longstrid WithInfo list |
	SEQdec of info * dec * dec |
	INFIXdec of info * int Option * id list |
	INFIXRdec of info * int Option * id list |
	NONFIXdec of info * id list |
	EMPTYdec of info

  and valbind =
	PLAINvalbind of info * pat * exp * valbind Option |
	RECvalbind of info * valbind

  and FValBind = FVALBIND of info * FClause * FValBind Option
  and FClause = FCLAUSE of info * atpat list * ty Option * exp * FClause Option

  and typbind =
        TYPBIND of info * tyvar list * tycon * ty * typbind Option

  and datbind =
        DATBIND of info * tyvar list * tycon * conbind * datbind Option

  and conbind =
        CONBIND of info * con op_opt * ty Option * conbind Option

  and exbind =
        EXBIND of info * excon op_opt * ty Option * exbind Option |
        EXEQUAL of info * excon op_opt * longid op_opt * exbind Option

  and atpat =
        WILDCARDatpat of info |
	SCONatpat of info * scon |
	LONGIDatpat of info * longid op_opt |
	RECORDatpat of info * patrow Option |
	PARatpat of info * pat

  and patrow =
        DOTDOTDOT of info |
        PATROW of info * lab * pat * patrow Option

  and pat =
        ATPATpat of info * atpat |
        CONSpat of info * longid op_opt * atpat |
        TYPEDpat of info * pat * ty |
        LAYEREDpat of info * id op_opt * ty Option * pat |
	UNRES_INFIXpat of info * atpat list

  and ty =
        TYVARty of info * tyvar |
        RECORDty of info * tyrow Option |
        CONty of info * ty list * longtycon |
        FNty of info * ty * ty |
        PARty of info * ty

  and tyrow =
        TYROW of info * lab * ty * tyrow Option

  val getExplicitTyVarsTy      : ty -> tyvar list
  and getExplicitTyVarsConbind : conbind -> tyvar list

end;
