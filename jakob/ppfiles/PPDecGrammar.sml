(*
$File: Common/PPDecGrammar.sml $
$Date: 1992/04/07 14:56:46 $
$Revision: 1.16 $
$Locker: birkedal $
*)

(*$PPDecGrammar:
	DEC_GRAMMAR SCON LAB CON EXCON TYVAR TYCON STRID
	PRETTYPRINT PPDECGRAMMAR CRASH
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

		     structure Ident:
		       sig
			 type longid
			 val pr_longid: longid -> string
		       end
		       sharing type Ident.longid = DecGrammar.longid

		     structure Id:
		       sig
			 type id
			 val pr_id: id -> string
		       end
		       sharing type Id.id = DecGrammar.id

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

    fun layoutAtexp atexp : StringTree =
      case atexp
	of SCONatexp(_, scon) => PP.LEAF(SCon.pr_scon scon)

	 | IDENTatexp(_, OP_OPT(longid, withOp)) =>
	     PP.LEAF((if withOp then "op " else "") ^ Ident.pr_longid longid)

	 | RECORDatexp(_, exprow_opt) =>
	     (case exprow_opt
		of Some exprow =>
		     PP.NODE{start="{", finish="}", indent=1,
			     children=[layoutExprow exprow],
			     childsep=PP.NONE
			    }

	         | None =>
		     PP.LEAF "{}"	(* Keep this atomic... *)
	     )

	 | LETatexp(_, dec, exp) =>
	     let
	       val decT = layoutDec dec
	       val expT = layoutExp exp
	     in
	       PP.NODE{start="let ", finish=" end", indent=4,
		       children=[decT, expT],
		       childsep=PP.LEFT " in "
		      }
	     end

	 | PARatexp(_, exp) =>
	     PP.NODE{start="(", finish=")", indent=1,
		     children=[layoutExp exp],
		     childsep=PP.NONE
		    }

    and layoutExprow row: StringTree =
      let
	fun treesOfExprow(EXPROW(_, lab, exp, exprow_opt)): StringTree list =
	  let
	    val this =
	      PP.NODE{start="", finish="", indent=0,
		      children=[PP.LEAF(Lab.pr_Lab lab), layoutExp exp],
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

    and layoutExp exp : StringTree =
      case exp
	of ATEXPexp(_, atexp) =>
	     layoutAtexp atexp

	 | APPexp(_, exp, atexp) =>
	     let
	       val expT = layoutExp exp
	       val atexpT = layoutAtexp atexp
	     in
	       PP.NODE{start="", finish="", indent=0,
		       children=[expT, atexpT],
		       childsep=PP.RIGHT " "
		      }
	     end

	 | TYPEDexp(_, exp, ty) =>
	     let
	       val expT = layoutExp exp
	       val tyT = layoutTy ty
	     in
	       PP.NODE{start="", finish="", indent=0,
		       children=[expT, tyT],
		       childsep=PP.LEFT " : "
		      }
	     end

	 | HANDLEexp(_, exp, match) =>
	     let
	       val expT = layoutExp exp
	       val matchT = layoutMatch match
	     in
	       PP.NODE{start="", finish="", indent=0,
		       children=[expT, matchT],
		       childsep=PP.LEFT " handle "
		      }
	     end

	 | RAISEexp(_, exp) =>
	     PP.NODE{start="raise ", finish="", indent=6,
		     children=[layoutExp exp],
		     childsep=PP.NONE
		    }

	 | FNexp(_, match) =>
	     PP.NODE{start="fn ", finish="", indent=3,
		     children=[layoutMatch match],
		     childsep=PP.NONE
		    }

	 | UNRES_INFIXexp(_, atexps) =>
	     PP.NODE{start="<UNRES_INFIX ", finish=">", indent=3,
		     children=map layoutAtexp atexps, childsep=PP.RIGHT " "
		    }

    and layoutMatch match : StringTree =
      let
	fun treesOfMatch(MATCH(_, mrule, match_opt)) : StringTree list =
	  layoutMrule mrule
	  :: (case match_opt
	        of Some match => treesOfMatch match
	         | None => nil
	     )
      in
	PP.NODE{start="", finish="", indent=0,
		children=treesOfMatch match,
		childsep=PP.LEFT " | "
	       }
      end

    and layoutMrule (MRULE(_, pat, exp)) : StringTree =
      let
	val patT = layoutPat pat
	val expT = layoutExp exp
      in
	PP.NODE{start="", finish="", indent=0,
		children=[patT, expT],
		childsep=PP.RIGHT " => "
	       }
      end

    and layoutDec dec : StringTree =
      case dec
	of VALdec(_, valbind) =>
	     PP.NODE{start="val ", finish="", indent=4,
		     children=[layoutValbind valbind],
		     childsep=PP.NONE
		     }


	 | UNRES_FUNdec _ =>
	     PP.LEAF "<UNRES_FUN>"

	 | TYPEdec(_, typbind) =>
	     PP.NODE{start="type ", finish="", indent=5,
		     children=[layoutTypbind typbind],
		     childsep=PP.NONE
		    }

	 | DATATYPEdec(_, datbind) =>
	     PP.NODE{start="datatype ", finish="", indent=INDENT,
		     children=[layoutDatbind datbind],
		     childsep=PP.NONE
		    }

	 | ABSTYPEdec(_, datbind, dec) =>
	     let
	       val datbindT = layoutDatbind datbind
	       val decT = layoutDec dec
	     in
	       PP.NODE{start="abstype ", finish=" end", indent=INDENT,
		       children=[datbindT, decT],
		       childsep=PP.LEFT " with "
		      }
	     end

	 | EXCEPTIONdec(_, exbind) =>
	     PP.NODE{start="exception ", finish="", indent=INDENT,
		     children=[layoutExbind exbind],
		     childsep=PP.NONE
		    }

	 | LOCALdec(_, dec1, dec2) =>
	     let
	       val dec1T = layoutDec dec1
	       val dec2T = layoutDec dec2
	     in
	       PP.NODE{start="local ", finish=" end", indent=INDENT,
		       children=[dec1T, dec2T],
		       childsep=PP.LEFT " in "
		      }
	     end

	 | OPENdec(_, list) =>
	     PP.NODE{start="open ", finish="", indent=5,
		     children=map (fn WITH_INFO(_, id) =>
				     PP.LEAF(StrId.pr_LongStrId id)
				  ) list,
		     childsep=PP.RIGHT " "
		    }

	 | SEQdec(_, dec1, dec2) =>
	     let
	       val dec1T = layoutDec dec1
	       val dec2T = layoutDec dec2
	     in
	       PP.NODE{start="", finish="", indent=0,
		       children=[dec1T, dec2T],
		       childsep=PP.RIGHT "; "
		      }
	     end

         | INFIXdec(_, prec, ids) =>
	     PP.NODE{start="infix ", finish="", indent=6,
		     children=(case prec
				 of Some p => [PP.LEAF(Int.string p)]
				  | None => nil
			      ) @ map (PP.LEAF o Id.pr_id) ids,
		     childsep=PP.RIGHT " "
		    }

         | INFIXRdec(_, prec, ids) =>
	     PP.NODE{start="infixr ", finish="", indent=7,
		     children=(case prec
				 of Some p => [PP.LEAF(Int.string p)]
				  | None => nil
			      ) @ map (PP.LEAF o Id.pr_id) ids,
		     childsep=PP.RIGHT " "
		    }

         | NONFIXdec(_, ids) =>
	     PP.NODE{start="nonfix ", finish="", indent=7,
		     children=map (PP.LEAF o Id.pr_id) ids,
		     childsep=PP.RIGHT " "
		    }

	 | EMPTYdec _ =>
	     PP.LEAF ""

    and layoutValbind valbind : StringTree =
      let
	fun treesOfValbind valbind : StringTree list =
	  case valbind
	    of PLAINvalbind(_, pat, exp, valbind_opt) =>
	      let
		val patT = layoutPat pat
		val expT = layoutExp exp
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
      in
	PP.NODE{start="", finish="", indent=0,
		children=treesOfValbind valbind,
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

    and layoutAtpat atpat : StringTree =
      case atpat
	of WILDCARDatpat _ => PP.LEAF "_"

	 | SCONatpat(_, scon) => PP.LEAF(SCon.pr_scon scon)

	 | LONGIDatpat(_, OP_OPT(longid, withOp)) =>
	     PP.LEAF((if withOp then "op " else "") ^ Ident.pr_longid longid)

	 | RECORDatpat(_, patrow_opt) =>
	     (case patrow_opt
		of Some patrow =>
		     PP.NODE{start="{", finish="}", indent=1,
			     children=[layoutPatrow patrow],
			     childsep=PP.NONE
			     }

	         | None =>
		     PP.LEAF "{}"
	     )

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
  end;
