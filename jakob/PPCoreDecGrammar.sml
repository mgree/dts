(*
$File: Common/PPCoreDecGrammar.sml $
$Date: 1995/04/17 07:55:32 $
$Revision: 1.2 $
$Locker:  $
 *)

(* Changed version of PPDecGrammar by panic@diku.dk to pretty-print *)
(* and convert bare language constructs into core language, and     *)
(* adding neccessary parenthesis.                                   *)

(*$PPDecGrammar:
	DEC_GRAMMAR SCON IDENT LAB CON EXCON TYVAR TYCON STRID
	OPERATOR PRETTYPRINT PPDECGRAMMAR CRASH
 *)

functor PPDecGrammar(structure DecGrammar: DEC_GRAMMAR

		     structure SCon: SCON
		       sharing type SCon.scon = DecGrammar.scon

		     structure Lab: LAB
		       sharing type Lab.lab = DecGrammar.lab

		     structure Con: CON
		       sharing type Con.con = DecGrammar.con

		     structure Excon: EXCON
		       sharing type Excon.excon = DecGrammar.excon

		     structure Ident: IDENT
		       sharing type Ident.longid = DecGrammar.longid
			   and type Ident.id = DecGrammar.id
				 
		     structure Id:
		       sig
			 type id
			 val pr_id: id -> string
		       end
		       sharing type Id.id = DecGrammar.id

		     structure Operator : OPERATOR
		       sharing Ident = Operator.Ident
		           and type Ident.id = Operator.id
			 
		     structure TyVar: TYVAR
		       sharing type TyVar.SyntaxTyVar = DecGrammar.tyvar

		     structure TyCon: TYCON
		       sharing type TyCon.tycon = DecGrammar.tycon
			   and type TyCon.longtycon = DecGrammar.longtycon

		     structure StrId: STRID
		       sharing type StrId.longstrid = DecGrammar.longstrid

		     structure PP: PRETTYPRINT

		     structure Crash: CRASH
		    ): PPDECGRAMMAR =
  struct
    structure G = DecGrammar
    open G

    type StringTree = PP.StringTree
    type minipage = PP.minipage

    (* layoutXXX: convert grammar of declarations into a StringTree. *)

    val INDENT = 3			(* Standard indentation level. *)

    val appprec = 10  (* precedence of application *)
    val typeprec = ~1
    val andprec = ~2
    val orprec = ~3
    val handleprec = ~4
    val raiseprec = ~5
    val ifprec = ~6
    val whileprec = ~7
    val caseprec = ~8
    val fnprec = ~9
    val lowprec = ~99
      
    exception notOperator and notAndalso and notOrelse and
      notIf and notCase and notList and notTuple


    fun layoutExpRecordAsTuple infixenv atexp : StringTree =
      let
	fun listOfExprow labno (EXPROW(_, lab, exp, exprow_opt)) =
	      if Lab.is_LabN(lab, labno) then
		exp :: (case exprow_opt of
			  Some exprow => listOfExprow (labno + 1) exprow
			| None => nil)
	      else 
		raise notTuple
      in
	case atexp of
	  RECORDatexp(i, None) =>
	    layoutAtexp infixenv lowprec false (NILTUPLEatexp(i))
	| RECORDatexp(i, Some exprow) =>
	    layoutAtexp infixenv lowprec false
	      (NTUPLEatexp(i, listOfExprow 1 exprow))
	| _ => raise notTuple
      end
      
    and layoutAtexp infixenv prec protmatch atexp : StringTree =
      case atexp
	of SCONatexp(_, scon) => PP.LEAF(SCon.pr_scon scon)

	 | IDENTatexp(_, OP_OPT(longid, withOp)) =>
	     PP.LEAF((if withOp then "op " else "") ^ Ident.pr_longid longid)

	 | RECORDatexp(_, exprow_opt) =>
	     (layoutExpRecordAsTuple infixenv atexp
	      handle notTuple =>
	     (case exprow_opt
		of Some exprow =>
		     PP.NODE{start="{", finish="}", indent=1,
			     children=[layoutExprow infixenv exprow],
			     childsep=PP.NONE
			    }

	         | None =>
		     PP.LEAF "{}"	(* Keep this atomic... *)
	     ))

	 | RECSELatexp(_, lab) => PP.LEAF ("#" ^ Lab.pr_Lab lab)

	 | NILTUPLEatexp(_) => PP.LEAF "()"

	 | NTUPLEatexp(_, explist) =>
	     PP.NODE{start="(", finish=")", indent=1,
		     children=map (layoutExp infixenv lowprec false) explist,
		     childsep=PP.RIGHT ", "
		     }

	 | LISTatexp(_, explist) => 
	     PP.NODE{start="[", finish="]", indent=1,
		     children=map (layoutExp infixenv lowprec false) explist,
		     childsep=PP.RIGHT ", "
		     }

	 | SEQatexp(_, explist) => 
	     PP.NODE{start="(", finish=")", indent=1,
		     children=map (layoutExp infixenv lowprec false) explist,
		     childsep=PP.RIGHT "; "
		     }
		
	 | LETatexp(_, dec, exp) =>
	     let
	       val (decT,_) = layoutDec infixenv dec
	       val expT = layoutExp infixenv lowprec false exp
	     in
	       PP.NODE{start="let ", finish=" end", indent=4,
		       children=[decT, expT],
		       childsep=PP.LEFT " in "
		      }
	     end

	 | PARatexp(_, exp) =>
	     PP.NODE{start="(", finish=")", indent=1,
		     children=[layoutExp infixenv lowprec false exp],
		     childsep=PP.NONE
		     }

	 | EXPatexp(_, exp) => layoutExp infixenv prec protmatch exp

    and layoutExprow infixenv row: StringTree =
      let
	fun treesOfExprow(EXPROW(_, lab, exp, exprow_opt)): StringTree list =
	  let
	    val this =
	      PP.NODE{start="", finish="", indent=0,
		      children=[PP.LEAF(Lab.pr_Lab lab),
				layoutExp infixenv lowprec false exp],
		      childsep=PP.RIGHT " = "
		     }
	  in
	    this :: (case exprow_opt
		       of Some row => treesOfExprow row
		        | None => nil
		    )
	  end
      in
	PP.NODE{start="", finish="", indent=0,
		children=treesOfExprow row,
		childsep=PP.RIGHT ", "
	       }
      end

    (* panic@diku.dk, 22.02.95:                                     *)
    (* prec = precedence of surrounding operator; if exp is an      *)
    (*        application of an infix operator of LOWER precedence, *)
    (*        the expression should be parenthesised.               *)
    (* protmatch = indicates whether the expression is followed     *)
    (*             by another match rule (in which case FNexp's     *)
    (*             should be parenthesised).                        *)

    and layoutAppexp infixenv prec protmatch appexp =
      case appexp of
	ATEXPappexp(_, atexp) => layoutAtexp infixenv prec protmatch atexp
	  
      | APPEXPappexp(_, appexp, atexp) =>
	  let
	    val appexpT = layoutAppexp infixenv appprec false appexp
	    val atexpT = layoutAtexp infixenv prec protmatch atexp
	  in
	    PP.NODE{start="", finish="", indent=0,
		    children=[appexpT, atexpT],
		    childsep=PP.RIGHT " "
		    }
	  end

    and layoutInfixAsList infixenv infexp =
      let
	fun layoutList (APPEXPinfexp(_,
			  ATEXPappexp(_,
		            IDENTatexp(_, OP_OPT(longidnil, _))))) =
	      if Ident.decompose0 longidnil = Ident.id_NIL then
		[]
	      else
		raise notList
	  | layoutList (INFEXPinfexp(_, infexphd, id, infexptl)) =
	      if id = Ident.id_CONS then
		layoutInfexp infixenv lowprec false infexphd ::
		layoutList infexptl
	      else
		raise notList
	  | layoutList _ = raise notList
      in
	PP.NODE{start="[", finish="]", indent=1,
		children=layoutList infexp,
		childsep=PP.RIGHT ", "
		}
      end
	
    and layoutInfexp infixenv prec protmatch infexp =
      case infexp of
	APPEXPinfexp(_, appexp) =>
	  layoutAppexp infixenv prec protmatch appexp

      | INFEXPinfexp(_, infexpl, id, infexpr) =>
	  ((layoutInfixAsList infixenv infexp) handle notList =>
	  let
	    val (opprec, assoc) = Operator.lookupOp infixenv id
	    val (lprec, rprec) =
	      case assoc of
		Operator.LEFT => (opprec, opprec + 1)
	      | Operator.RIGHT => (opprec + 1, opprec)
	    val infexplT =
	      layoutInfexp infixenv lprec protmatch infexpl
	    val infexprT =
	      layoutInfexp infixenv rprec protmatch infexpr
	    val (start, finish, indent) =
	      if opprec < prec then ("(", ")", 1) else ("", "", 0)
	  in
	    PP.NODE{start=start, finish=finish, indent=indent,
		    children=[infexplT, infexprT],
		    childsep=PP.RIGHT (" " ^ Ident.pr_id id ^ " ")
		    }
	  end)
	      
  
    and layoutAppAsOp infixenv prec protmatch exp =
      case exp of
	APPexp(i, ATEXPexp(_, IDENTatexp(_, OP_OPT(longid, _))),
	       RECORDatexp(_, Some (EXPROW(il, labl, expl, Some
			           (EXPROW(ir, labr, expr, None))))
			   )
	       ) =>
	if Operator.isInfix infixenv (Ident.decompose0 longid) andalso
	  Lab.is_LabN(labl, 1) andalso
	  Lab.is_LabN(labr, 2) then
	  layoutInfexp infixenv prec protmatch
  	    (INFEXPinfexp(i,
	       APPEXPinfexp(il,
		 ATEXPappexp(il, EXPatexp(il, expl))),
	       (Ident.decompose0 longid),
	       APPEXPinfexp(ir,
		 ATEXPappexp(ir, EXPatexp(ir, expr)))))
	else raise notOperator
      | _ => raise notOperator

    and layoutAppAsAndalso infixenv prec protmatch exp =
      case exp of
	APPexp(i,
	  FNexp(_,
	    MATCH(_,
	      MRULE(_,
		ATPATpat(_, LONGIDatpat(_, OP_OPT(longidt, _))),
		exp2),
	    Some (MATCH(_,
	      MRULE(_,
		ATPATpat(_, LONGIDatpat(_, OP_OPT(longidf, _))),
		ATEXPexp(_, IDENTatexp(_, OP_OPT(longidf', _)))),
	    None)))),
	  atexp1) =>
	if Ident.decompose0 longidt = Ident.id_TRUE andalso
	   Ident.decompose0 longidf = Ident.id_FALSE andalso
	   Ident.decompose0 longidf'= Ident.id_FALSE then
	   layoutExp infixenv prec protmatch
	     (ANDALSOexp(i, ATEXPexp(i, atexp1), exp2))
	else raise notAndalso
      | _ => raise notAndalso

    and layoutAppAsOrelse infixenv prec protmatch exp =
      case exp of
	APPexp(i,
	  FNexp(_,
	    MATCH(_,
	      MRULE(_,
		ATPATpat(_, LONGIDatpat(_, OP_OPT(longidt, _))),
		ATEXPexp(_, IDENTatexp(_, OP_OPT(longidt', _)))),
	    Some (MATCH(_,
	      MRULE(_,
		ATPATpat(_, LONGIDatpat(_, OP_OPT(longidf, _))),
		exp2),
	    None)))),
	  atexp1) =>
	if Ident.decompose0 longidt = Ident.id_TRUE andalso
	   Ident.decompose0 longidf = Ident.id_FALSE andalso
	   Ident.decompose0 longidt'= Ident.id_TRUE then
	   layoutExp infixenv prec protmatch
	     (ORELSEexp(i, ATEXPexp(i, atexp1), exp2))
	else raise notOrelse
      | _ => raise notOrelse

    and layoutAppAsIf infixenv prec protmatch exp =
      case exp of
	APPexp(i,
	  FNexp(_,
	    MATCH(_,
	      MRULE(_,
		ATPATpat(_, LONGIDatpat(_, OP_OPT(longidt, _))),
		exp2),
	    Some (MATCH(_,
	      MRULE(_,
		ATPATpat(_, LONGIDatpat(_, OP_OPT(longidf, _))),
		exp3),
	    None)))),
	  atexp1) =>
	if Ident.decompose0 longidt = Ident.id_TRUE andalso
	   Ident.decompose0 longidf = Ident.id_FALSE then
	   layoutExp infixenv prec protmatch
	     (IFexp(i, ATEXPexp(i, atexp1), exp2, exp3))
	else raise notIf
      | _ => raise notIf

    and layoutAppAsCase infixenv prec protmatch exp =
      case exp of
	APPexp(i, FNexp(_, match), atexp) =>
	  layoutExp infixenv prec protmatch
	    (CASEexp(i, ATEXPexp(i, atexp), match))
      | _ => raise notCase


    and layoutExp infixenv prec protmatch exp : StringTree =
      case exp
	of ATEXPexp(_, atexp) =>
	  layoutAtexp infixenv prec protmatch atexp

      | INFEXPexp(_, infexp) =>
	  layoutInfexp infixenv prec protmatch infexp
	  
      | APPexp(_, exp', atexp) =>
 	  (layoutAppAsOp infixenv prec protmatch exp
	handle notOperator =>
	  layoutAppAsAndalso infixenv prec protmatch exp
	handle notAndalso =>
	  layoutAppAsOrelse infixenv prec protmatch exp
	handle notOrelse =>
	  layoutAppAsIf infixenv prec protmatch exp 
        handle notIf =>
	  layoutAppAsCase infixenv prec protmatch exp
	handle notCase =>
	  let
	    val expT = layoutExp infixenv appprec false exp'
	    val atexpT = layoutAtexp infixenv prec protmatch atexp
	  in
	    PP.NODE{start="", finish="", indent=0,
		    children=[expT, atexpT],
		    childsep=PP.RIGHT " "
		    }
	  end)

      | TYPEDexp(_, exp, ty) =>
	  let
	    val expT = layoutExp infixenv typeprec false exp
	    val tyT = layoutTy ty
	  in
	    PP.NODE{start="", finish="", indent=0,
		    children=[expT, tyT],
		    childsep=PP.LEFT " : "
		    }
	  end
	
      | ANDALSOexp(_, exp1, exp2) =>
	  let
	    val exp1T = layoutExp infixenv andprec false exp1
	    val exp2T = layoutExp infixenv (andprec + 1) protmatch exp2
	    val (start, finish) =
	      if andprec < prec then ("(", ")") else ("", "")
	  in
	    PP.NODE{start=start, finish=finish, indent=8,
		    children=[exp1T, exp2T],
		    childsep=PP.LEFT " andalso "
		    }
	  end

      | ORELSEexp(_, exp1, exp2) =>
	  let
	    val exp1T = layoutExp infixenv orprec false exp1
	    val exp2T = layoutExp infixenv (orprec + 1) protmatch exp2
	    val (start, finish) =
	      if orprec < prec then ("(", ")") else ("", "")
	  in
	    PP.NODE{start=start, finish=finish, indent=7,
		    children=[exp1T, exp2T],
		    childsep=PP.LEFT " orelse "
		    }
	  end
	      
      | HANDLEexp(_, exp, match) =>
	  let
	    val expT = layoutExp infixenv handleprec false exp
	    val matchT = layoutMatch infixenv match
	    val (start, finish) =
	      if protmatch orelse handleprec < prec then
		("(", ")")
	      else
		("", "")
	  in
	    PP.NODE{start=start, finish=finish, indent=2,
		    children=[expT, matchT],
		    childsep=PP.LEFT " handle "
		    }
	  end

      | RAISEexp(_, exp) =>
	  let
	    val expT = layoutExp infixenv raiseprec protmatch exp
	    val (start, finish, indent) =
	      if raiseprec < prec then
		("(raise ", ")", 7)
	      else
		("raise ", "", 6)
	  in
	    PP.NODE{start=start, finish=finish, indent=indent,
		    children=[expT],
		    childsep=PP.NONE
		    }
	  end

      | IFexp(_, exp1, exp2, exp3) =>
	  let
	    val exp1T = layoutExp infixenv lowprec false exp1
	    val exp2T = layoutExp infixenv lowprec false exp2
	    val exp3T = layoutExp infixenv ifprec protmatch exp3
	    val (start, finish, indent) =
	      if ifprec < prec then
		("(", ")", 1)
	      else
		("", "", 0)
	  in
	    PP.NODE{start=start, finish=finish, indent=indent,
		    children=[PP.NODE{start="if ", finish=" then ",
				      indent=3,
				      children=[exp1T],
				      childsep=PP.NONE
				      },
			      PP.NODE{start="", finish="",
				      indent=2,
				      children=[exp2T, exp3T],
				      childsep=PP.LEFT " else "
				      }
			      ],
		    childsep=PP.NONE
		    }
	  end

      | WHILEexp(_, exp1, exp2) =>
	  let
	    val exp1T = layoutExp infixenv lowprec false exp1
	    val exp2T = layoutExp infixenv whileprec protmatch exp2
	    val (start, finish, indent) =
	      if whileprec < prec then
		("(while ", ")", 3)
	      else
		("while ", "", 2)
	  in
	    PP.NODE{start=start, finish=finish, indent=indent,
		    children=[exp1T, exp2T],
		    childsep=PP.RIGHT " do "
		    }
	  end
	
      | CASEexp(_, exp, match) =>
	  let
	    val expT = layoutExp infixenv lowprec false exp
	    val matchT = layoutMatch infixenv match
	    val (start, finish, indent) =
	      if protmatch then
		("(", ")", 1)
	      else
		("", "", 0)
	  in
	    PP.NODE{start=start, finish=finish, indent=indent,
		    children=[PP.NODE{start="case ", finish="",
				      indent=5,
				      children=[expT],
				      childsep=PP.NONE
				      },
			      PP.NODE{start="", finish="", indent=0,
				      children=[matchT],
				      childsep=PP.NONE
				      }],
		    childsep=PP.RIGHT " of "
		    }
	  end
	
      | FNexp(_, match) =>
	  let
	    val matchT = layoutMatch infixenv match
	    val (start, finish, indent) =
	      if fnprec < prec orelse protmatch then
		("(fn ", ")", 2)
	      else
		("fn ", "", 1)
	  in
	    PP.NODE{start=start, finish=finish, indent=indent,
		    children=[matchT],
		    childsep=PP.NONE
		    }
	  end

      | UNRES_INFIXexp(_, atexps) =>
	  PP.NODE{start="<UNRES_INFIX ", finish=">", indent=3,
		  children=map (layoutAtexp infixenv prec protmatch) atexps,
		  childsep=PP.RIGHT " "
		  }

    and layoutMatch infixenv match : StringTree =
      let
	fun treesOfMatch (MATCH(_, mrule, None)) : StringTree list =
	      [layoutMrule infixenv false mrule]
	  | treesOfMatch (MATCH(_, mrule, Some match)) =
	      layoutMrule infixenv true mrule :: treesOfMatch match
      in
	PP.NODE{start="", finish="", indent=2,
		children=treesOfMatch match,
		childsep=PP.LEFT " | "
	       }
      end

    and layoutMrule infixenv protmatch (MRULE(_, pat, exp)) : StringTree =
      let
	val patT = layoutPat pat
	val expT = layoutExp infixenv lowprec protmatch exp
      in
	PP.NODE{start="", finish="", indent=0,
		children=[patT, expT],
		childsep=PP.RIGHT " => "
	       }
      end

    and layoutDec infixenv dec : (StringTree * Operator.infixenv) =
      case dec of
	VALdec(_, valbind) =>
	  (PP.NODE{start="val ", finish="", indent=4,
		   children=[layoutValbind infixenv valbind],
		   childsep=PP.NONE
		   },
	   infixenv): (StringTree * Operator.infixenv)

      | FUNdec(_, fvalbind) =>
	  (PP.NODE{start="fun ", finish="", indent=2,
		   children=[layoutFvalbind fvalbind],
		   childsep=PP.NONE
		   },
	   infixenv)
	     
      | UNRES_FUNdec _ =>
	  (PP.LEAF "<UNRES_FUN>", infixenv)
	  
      | TYPEdec(_, typbind) =>
	  (PP.NODE{start="type ", finish="", indent=5,
		   children=[layoutTypbind typbind],
		   childsep=PP.NONE
		   },
	   infixenv)

      | DATATYPEdec(_, datbind) =>
	  (PP.NODE{start="datatype ", finish="", indent=INDENT,
		   children=[layoutDatbind datbind],
		   childsep=PP.NONE
		   },
	   infixenv)

      | ABSTYPEdec(_, datbind, dec) =>
	  let
	    val datbindT = layoutDatbind datbind
	    val (decT, _) = layoutDec infixenv dec
	  in
	    (PP.NODE{start="abstype ", finish=" end", indent=INDENT,
		     children=[datbindT, decT],
		     childsep=PP.LEFT " with "
		     },
	     infixenv)
	  end

      | EXCEPTIONdec(_, exbind) =>
	  (PP.NODE{start="exception ", finish="", indent=INDENT,
		   children=[layoutExbind exbind],
		   childsep=PP.NONE
		   },
	   infixenv)

      | LOCALdec(_, dec1, dec2) =>
	  let
	    val (dec1T, infixenv') = layoutDec infixenv dec1
	    val (dec2T, infixenv'') = layoutDec infixenv dec2
	  in
	    (PP.NODE{start="local ", finish=" end", indent=INDENT,
		     children=[dec1T, dec2T],
		     childsep=PP.LEFT " in "
		     },
	     infixenv'')
	     (* strictly speaking the returned fixity environment *)
	     (* should be infixenv + (infixenv'' - infixenv')     *)
	  end

      | OPENdec(_, list) =>
	  (PP.NODE{start="open ", finish="", indent=5,
		   children=map (fn WITH_INFO(_, id) =>
				 PP.LEAF(StrId.pr_LongStrId id)
				 ) list,
		   childsep=PP.RIGHT " "
		   },
	   infixenv)
	   (* hope for God that the open declaration doesn't affect *)
	   (* the fixity of any of the operators in the environment *)

      | SEQdec(_, dec1, dec2) =>
	  let
	    val (dec1T, infixenv) = layoutDec infixenv dec1
	    val (dec2T, infixenv) = layoutDec infixenv dec2
	  in
	    (PP.NODE{start="", finish="", indent=0,
		     children=[dec1T, dec2T],
		     childsep=PP.RIGHT "; "
		     },
	     infixenv)
	  end

      | INFIXdec(_, prec, ids) =>
	  let
	    val infixenv = Operator.registerInfix infixenv prec ids
	  in
	    (PP.NODE{start="infix ", finish="", indent=6,
		     children=(case prec of
				 Some p => [PP.LEAF(Int.string p)]
			       | None => nil)
			          @ map (PP.LEAF o Id.pr_id) ids,
		     childsep=PP.RIGHT " "
		     },
	     infixenv)
	  end

      | INFIXRdec(_, prec, ids) =>
	  let
	    val infixenv = Operator.registerInfixr infixenv prec ids
	  in
	    (PP.NODE{start="infixr ", finish="", indent=7,
		     children=(case prec of
				 Some p => [PP.LEAF(Int.string p)]
			       | None => nil
			         ) @ map (PP.LEAF o Id.pr_id) ids,
		     childsep=PP.RIGHT " "
		     },
	     infixenv)
	  end

      | NONFIXdec(_, ids) =>
	  let
	    val infixenv = Operator.registerNonfix infixenv ids
	  in
	    (PP.NODE{start="nonfix ", finish="", indent=7,
		     children=map (PP.LEAF o Id.pr_id) ids,
		     childsep=PP.RIGHT " "
		     },
	     infixenv)
	  end
	
      | EMPTYdec _ =>
	  (PP.LEAF "", infixenv)

    and layoutValbind infixenv valbind : StringTree =
      let
	fun treesOfValbind valbind : StringTree list =
	  case valbind
	    of PLAINvalbind(_, pat, exp, valbind_opt) =>
	      let
		val patT = layoutPat pat
		val expT = layoutExp infixenv lowprec false exp
		val this =
		  PP.NODE{start="", finish="", indent=0,
			  children=[patT, expT],
			  childsep=PP.RIGHT " = "
			  }
	      in
		this :: (case valbind_opt
			   of Some valbind => treesOfValbind valbind
		            | None => nil
			)
	      end

	  | RECvalbind(_, valbind) =>
	      [PP.NODE{start="rec ", finish="", indent=4,
		       children=treesOfValbind valbind,
		       childsep=PP.LEFT " and "
		      }
	      ]
	    (* conversion from `val rec f = fn x =>' to `fun f x' *)
	    (* should occur here.                                 *)
      in
	PP.NODE{start="", finish="", indent=0,
		children=treesOfValbind valbind,
		childsep=PP.LEFT " and "
	       }
      end

    and layoutFvalbind fvalbind =
      let
	fun treesOfFValbind (FVALBIND(_, OP_OPT(var, withop), fclause, fvalbind_opt))
	  : StringTree list =
	  let
	    val start=(if withop then "op " else "") ^ Ident.pr_longid var
	    fun layoutFclause
	      (FCLAUSE(_, atpatlist, ty_opt, exp, fclause_opt)) =
	      PP.NODE{start=start, finish="", indent=2,
		      children=map layoutAtpat atpatlist,
		      childsep=PP.RIGHT " "
		      } ::
	      (case fclause_opt of
		Some fclause => layoutFclause fclause
	      | None => nil)
	  in
	    PP.NODE{start="", finish="", indent=2,
		    children=layoutFclause fclause,
		    childsep=PP.RIGHT " | "
		    } ::
	    (case fvalbind_opt of
	      Some fvalbind => treesOfFValbind fvalbind
	    | None => nil)
	  end	    
      in
	PP.NODE{start="", finish="", indent=2,
		children=treesOfFValbind fvalbind,
		childsep=PP.LEFT " and "
		}
      end
 
    
    and layoutTyvarseq tyvars =
      case tyvars
	of nil => None
	 | [tv] => Some(PP.LEAF(TyVar.pr_tyvar tv))
	 | tvs => Some(PP.NODE{start="(", finish=")", indent=1,
			       children=map (PP.LEAF o TyVar.pr_tyvar) tvs,
			       childsep=PP.RIGHT ", "
			      }
		      )

    and layoutTypbind typbind : StringTree =
      let
	fun treesOfTypbind(TYPBIND(_, tyvars, tycon, ty, typbind_opt))
	  : StringTree list =
	  let
	    val tyvars_opt = layoutTyvarseq tyvars
	    val tyconT = PP.LEAF(TyCon.pr_TyCon tycon)
	    val eqT = PP.LEAF " = "
	    val tyT = layoutTy ty

	    val this =
	      PP.NODE{start="", finish="", indent=0,
		      children=(case tyvars_opt of Some x => [x]
		    				 | None => nil
			       ) @  [tyconT, eqT, tyT],
		      childsep=PP.NONE
		     }
	  in
	    this :: (case typbind_opt
		       of Some typbind => treesOfTypbind typbind
		        | None => nil
		    )
	  end
      in
	PP.NODE{start="", finish="", indent=0,
		children=treesOfTypbind typbind,
		childsep=PP.LEFT " and "
	       }
      end

    and layoutDatbind datbind : StringTree =
      let
	fun treesOfDatbind(DATBIND(_, tyvars, tycon, conbind, datbind_opt))
	  : StringTree list =
	  let
	    val tyvarsT_opt = layoutTyvarseq tyvars
	    val tyconT = PP.LEAF(TyCon.pr_TyCon tycon)
	    val tyBindingT =
	      case tyvarsT_opt
		of None => tyconT
		 | Some x => PP.NODE{start="", finish="", indent=0,
				     children=[x, tyconT],
				     childsep=PP.RIGHT " "
				    }
	    val eqT = PP.LEAF " = "
	    val conbindT = layoutConbind conbind

	    val this =
	      PP.NODE{start="", finish="", indent=0,
		      children=[tyBindingT, eqT, conbindT],
		      childsep=PP.NONE
		     }
	  in
	    this :: (case datbind_opt
		       of Some datbind => treesOfDatbind datbind
		        | None => nil
		    )
	  end
      in
	PP.NODE{start="", finish="", indent=0,
		children=treesOfDatbind datbind,
		childsep=PP.LEFT " and "
	       }
      end

    and layoutConbind conbind : StringTree =
      let
	fun treesOfConbind(CONBIND(_, OP_OPT(con, withOp), ty_opt, conbind_opt))
	  : StringTree list =
	  let
	    val conT =
	      PP.LEAF((if withOp then "op " else "") ^ Con.pr_con con)

	    val this =
	      case ty_opt
		of Some ty => PP.NODE{start="", finish="", indent=0,
				      children=[conT, layoutTy ty],
				      childsep=PP.LEFT " of "
				     }
	      | None => conT
	  in
	    this :: (case conbind_opt
		       of Some conbind => treesOfConbind conbind
		        | None => nil
		    )
	  end
      in
	PP.NODE{start="", finish="", indent=0,
		children=treesOfConbind conbind,
		childsep=PP.LEFT " | "
	       }
      end

    and layoutExbind exbind : StringTree =
      let
	fun layoutIdSubTRest(excon, withOp, sep, subT, rest) : StringTree list =
	  let
	    val this =
	      PP.NODE{start="", finish="", indent=0,
		      children=[PP.LEAF((if withOp then "op " else "")
					^ Excon.pr_excon excon
				       ),
				subT
			       ],
		      childsep=PP.LEFT sep
		     }
	  in
	    this :: (case rest
		       of Some exbind => treesOfExbind exbind
		        | None => nil
		    )
	  end

	and treesOfExbind exbind : StringTree list =
	  case exbind
	    of EXBIND(_, OP_OPT(excon, withOp), Some ty, exbind_opt) =>
	      layoutIdSubTRest(excon, withOp, " of ", layoutTy ty, exbind_opt)

	  | EXBIND(_, OP_OPT(excon, withOp), None, exbind_opt) =>
	      PP.LEAF((if withOp then "op " else "") ^ Excon.pr_excon excon)
	      :: (case exbind_opt
		    of Some exbind => treesOfExbind exbind
		     | None => nil
		 )

	  | EXEQUAL(_, OP_OPT(excon, exconOp),
		       OP_OPT(longid, longidOp),
		       exbind_opt
		   ) =>
	      layoutIdSubTRest(excon, exconOp, " = ",
			       PP.LEAF((if longidOp then "op " else "")
				       ^ Ident.pr_longid longid
				      ),
			       exbind_opt
			      )
      in
	PP.NODE{start="", finish="", indent=0,
		children=treesOfExbind exbind,
		childsep=PP.LEFT " and "
	       }
      end

    and layoutPatRecordAsTuple atpat : StringTree =
      let
	fun listOfPatrow labno (PATROW(_, lab, pat, patrow_opt)) =
	      if Lab.is_LabN(lab, labno) then
		pat :: (case patrow_opt of
			  Some patrow => listOfPatrow (labno + 1) patrow
			| None => nil)
	      else 
		raise notTuple
	  | listOfPatrow _ _ = raise notTuple
      in
	case atpat of
	  RECORDatpat(i, None) =>
	    layoutAtpat (NILTUPLEatpat(i))
	| RECORDatpat(i, Some patrow) =>
	    layoutAtpat (NTUPLEatpat(i, listOfPatrow 1 patrow))
	| _ => raise notTuple
      end

    and layoutAtpat atpat : StringTree =
      case atpat
	of WILDCARDatpat _ => PP.LEAF "_"

	 | SCONatpat(_, scon) => PP.LEAF(SCon.pr_scon scon)

	 | LONGIDatpat(_, OP_OPT(longid, withOp)) =>
	     PP.LEAF((if withOp then "op " else "") ^ Ident.pr_longid longid)

	 | RECORDatpat(_, patrow_opt) =>
	     (layoutPatRecordAsTuple atpat
	      handle notTuple =>
	     (case patrow_opt
		of Some patrow =>
		     PP.NODE{start="{", finish="}", indent=1,
			     children=[layoutPatrow patrow],
			     childsep=PP.NONE
			     }

	         | None =>
		     PP.LEAF "{}"
	     ))

	 | NILTUPLEatpat(_) => PP.LEAF "()"

	 | NTUPLEatpat(_, patlist) =>
	     PP.NODE{start="(", finish=")", indent=1,
		     children=map layoutPat patlist,
		     childsep=PP.LEFT ", "
		     }

	 | LISTatpat(_, patlist) => 
	     PP.NODE{start="[", finish="]", indent=1,
		     children=map layoutPat patlist,
		     childsep=PP.LEFT ", "
		     }
	     
	 | PARatpat(_, pat) =>
	     PP.NODE{start="(", finish=")", indent=1,
		     children=[layoutPat pat],
		     childsep=PP.NONE
		     }

    and layoutPatrow row : StringTree =
      let
	fun treesOfPatrow row : StringTree list =
	  case row
	    of DOTDOTDOT _ =>
	         [PP.LEAF "..."]

	     | PATROW(_, lab, pat, patrow_opt) =>
		 let
		   val this =
		     PP.NODE{start="", finish="", indent=0,
			     children=[PP.LEAF(Lab.pr_Lab lab), layoutPat pat],
			     childsep=PP.RIGHT " = "
			     }
		 in
		   this :: (case patrow_opt
			      of Some row => treesOfPatrow row
			       | None => nil
			    )
		 end
	     | LABELpatrow(_, id, ty_opt, pat_opt, patrow_opt) =>
		 PP.NODE{start="", finish="", indent=0,
			 children=(PP.NODE{start="", finish=" ", indent=0,
					   children=PP.LEAF(Id.pr_id id)::
					   (case ty_opt of
					      Some ty => [layoutTy ty]
					    | None => nil),
				           childsep=PP.RIGHT " : "}) ::
			          (case pat_opt of
				     Some pat =>
				       [PP.NODE{start="as ", finish="",
					       indent=0,
					       children=[layoutPat pat],
					       childsep=PP.NONE
					       }]
				   | None => nil),
			 childsep=PP.NONE
			 } ::
		 (case patrow_opt of
		    Some patrow => treesOfPatrow patrow
		  | None => nil)
      in
	PP.NODE{start="", finish="", indent=0,
		children=treesOfPatrow row,
		childsep=PP.RIGHT ", "
		}
      end

    and layoutPat pat : StringTree =
      case pat
	of ATPATpat(_, atpat) =>
	     layoutAtpat atpat

	 | CONSpat(_, OP_OPT(longid, withOp), atpat) =>
	     PP.NODE{start=(if withOp then "op " else "")
		     	   ^ Ident.pr_longid longid ^ " ",
		     finish="",
		     indent=INDENT,
		     children=[layoutAtpat atpat],
		     childsep=PP.NONE
		    }

	 | INFIXVALpat(_, pat, con, pat') =>
	     PP.NODE{start="", finish="", indent=0,
		     children=[layoutPat pat,
			       PP.LEAF(Con.pr_con con),
			       layoutPat pat'],
		     childsep=PP.RIGHT " "
		     }
	     
	 | INFIXEXCpat(_, pat, excon, pat') =>
	     PP.NODE{start="", finish="", indent=0,
		     children=[layoutPat pat,
			       PP.LEAF(Excon.pr_excon excon),
			       layoutPat pat'],
		     childsep=PP.RIGHT " "
		     }
	     
	 | TYPEDpat(_, pat, ty) =>
	     let
	       val patT = layoutPat pat
	       val tyT = layoutTy ty
	     in
	       PP.NODE{start="", finish="", indent=0,
		       children=[patT, tyT],
		       childsep=PP.LEFT " : "
		       }
	     end

	 | LAYEREDpat(_, OP_OPT(id, withOp), ty_opt, pat) =>
	     let
	       val idT = PP.LEAF((if withOp then "op " else "") ^ Id.pr_id id)

	       val identColonTyT =
		 case ty_opt
		   of Some ty =>
			PP.NODE{start="", finish="", indent=0,
				children=[idT, layoutTy ty],
				childsep=PP.LEFT " : "
				}
		    | None =>
			idT

	       val patT = layoutPat pat
	     in
	       PP.NODE{start="", finish="", indent=0,
		       children=[identColonTyT, patT],
		       childsep=PP.LEFT " as "
		       }
	     end

         | UNRES_INFIXpat(_, atpats) =>
	     PP.NODE{start="<UNRES_INFIX ", finish=">", indent=3,
		     children=map layoutAtpat atpats, childsep=PP.RIGHT " "
		    }

    and layoutTy ty : StringTree =
      case ty
	of TYVARty(_, tyvar) =>
	     PP.LEAF(TyVar.pr_tyvar tyvar)

	 | RECORDty(_, tyrow_opt) =>
	     (case tyrow_opt
		of Some tyrow =>
		     PP.NODE{start="{", finish="}", indent=1,
			     children=[layoutTyrow tyrow],
			     childsep=PP.NONE
			     }

		 | None =>
		     PP.LEAF "{}"	(* "unit" ? *)
		     )

	 | CONty(_, tys, longtycon) =>
	     let
	       fun idTail t =
		 PP.NODE{start="", finish=" " ^ TyCon.pr_LongTyCon longtycon,
			 indent=0, children=[t], childsep=PP.NONE
			 }
	     in
	       case tys
		 of nil => PP.LEAF(TyCon.pr_LongTyCon longtycon)

		  | [ty] => idTail(layoutTy ty)

		  | tys => idTail(PP.NODE{start="(", finish=")", indent=1,
					  children=map layoutTy tys,
					  childsep=PP.RIGHT ", "
					 }
				 )
	     end

	 | TUPLEty(_, tylist) =>
	     PP.NODE{start="", finish="", indent=0,
		     children=map layoutTy tylist,
		     childsep=PP.RIGHT " * "
		     }
	   
	 | FNty(_, ty1, ty2) =>
	     PP.NODE{start="", finish="", indent=0,
		     children=[layoutTy ty1, layoutTy ty2],
		     childsep=PP.LEFT " -> "
		    }

	 | PARty(_, ty) =>
	     PP.NODE{start="(", finish=")", indent=1,
		     children=[layoutTy ty],
		     childsep=PP.NONE
		    }

    and layoutTyrow row : StringTree =
      let
	fun treesOfTyrow(TYROW(_, lab, ty, tyrow_opt)) =
	  let
	    val this =
	      PP.NODE{start="", finish="", indent=0,
		      children=[PP.LEAF(Lab.pr_Lab lab), layoutTy ty],
		      childsep=PP.LEFT " : "
		     }
	  in
	    this :: (case tyrow_opt
		       of Some row => treesOfTyrow row
		        | None => nil
		    )
	  end
      in
	PP.NODE{start="", finish="", indent=0,
		children=treesOfTyrow row,
		childsep=PP.RIGHT ", "
	       }
      end

    val layoutExp = layoutExp Operator.basisenv lowprec false
    val layoutDec = (fn x => #1x) o layoutDec Operator.basisenv			       
  end;
