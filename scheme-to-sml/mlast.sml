structure MlAst =
(* construction of SML-Kit AST *)
struct

local open DecGrammar Ident Lab SCon 
           TyVar TyCon SchemeTypes
           Excon 
in

  exception TranslationUnimplemented;

  local val c = ref 0 in fun counter() = (c := !c + 1; !c) end
  fun fresh_varname() = "v"^(Integer.makestring(counter()))
  fun Tinfo x = ()
  fun dummy_info () = ()
  fun mk_IDENTatexp(info, id, op_opt) = 
                  IDENTatexp(info, OP_OPT(Ident.mk_LongId [id], op_opt))
  fun mk_newIDENTatexp() = mk_IDENTatexp((), "v"^(fresh_varname()), false)
  fun mk_lab i = Lab.mk_IntegerLab i
  fun mk_EXPROW (lab, exp, exprow_opt) =
         EXPROW(dummy_info(), lab, exp, exprow_opt)
  fun mk_pair_atexp(info,x,y) = RECORDatexp(info, Some (mk_EXPROW(mk_lab 1, x,
                                            Some (mk_EXPROW(mk_lab 2, y, None)))))
  fun mk_pair_exp(x,y) = ATEXPexp((), mk_pair_atexp((), x, y))
  fun mk_cons () = ATEXPexp(dummy_info(), mk_IDENTatexp(dummy_info(), "::", true))
  fun mk_CONSpat(conname, opt, atpat) = CONSpat(dummy_info(), 
                                         OP_OPT(Ident.mk_LongId [conname], opt),
                                         atpat)
  fun mk_LONGIDatpat(info, idname) = LONGIDatpat(info, OP_OPT(Ident.mk_LongId [idname], false))
  fun mk_ATPATpat(info, x) = ATPATpat(dummy_info(), mk_LONGIDatpat(info, x))
  fun mk_RECORDatpat(l1, p1, l2, p2) = RECORDatpat((), Some(PATROW((), l1, p1,
                                                       Some(PATROW((), l2, p2, None)))))
  fun mk_flexRECORDatpat(l, p) = RECORDatpat((), Some(PATROW((), l, p, Some(DOTDOTDOT ())))) 
  fun paren e = ATEXPexp(dummy_info(), PARatexp(dummy_info(), e))
      (* construct ML-application (e e') from ML-exps e,e' *)
  fun mk_APPexp(e, e') =  APPexp(dummy_info(), paren e, PARatexp((), e'))
      (* construct abstraction fn x => e(x) *)
  fun mk_simpleFNexp(varname, e) =  
      let val mrule = MRULE((), ATPATpat((), mk_LONGIDatpat((), varname)), e)
          val match = MATCH((), mrule, None)
      in
          FNexp((), match)
      end
  fun mk_newTYVARty() = TYVARty((),TyVar.mk_TyVar("'a"^makestring(counter())))
  fun mk_FNty(ty1, ty2) = FNty((), ty1, ty2)
  fun mk_PAIRty(ty1, ty2) = RECORDty((), Some(TYROW((), mk_lab 1, ty1,
                                       Some(TYROW((), mk_lab 2, ty2, None)))))
  fun mk_LongTyCon tyvars tycon_str = CONty((), tyvars, TyCon.mk_LongTyCon [tycon_str])
  fun mk_tyConst tycon_str = mk_LongTyCon [] tycon_str
  exception ConbindsError;
  fun mk_CONBINDs bindlst = 
         case bindlst of
           [(con, tyopt, opt)]   => Some(CONBIND((), OP_OPT(con, opt), tyopt, None))
         | (con, tyopt, opt)::tl => Some(CONBIND((), OP_OPT(con, opt), tyopt, mk_CONBINDs tl))
         | _                       => raise ConbindsError
   fun mk_simplePLAINvalbind(valname, exp) =
      PLAINvalbind((), ATPATpat((), mk_LONGIDatpat((), valname)), exp, None)
   fun mk_simpleVALdec(valname, exp) =
       let val valbind = mk_simplePLAINvalbind(valname, exp) 
       in
           VALdec((), valbind)
       end
  fun mk_EXCEPTIONdec(exname, tyoption) =
      let val excon = Excon.mk_excon (Ident.mk_Id exname)
          val exbind = EXBIND((), OP_OPT(excon, false), tyoption, None)
      in
          EXCEPTIONdec((), exbind)
      end
  fun mk_RAISEexp exnname = 
         RAISEexp((), ATEXPexp((), mk_IDENTatexp((), exnname, false)))

      (* global declaration of type DYNAMIC *)
  fun mk_dyn_dec() =
      let fun mk_new_TyVartys n = 
              if n = 0 then [] else (mk_new_TyVartys (n-1)) @ [mk_newTYVARty()]
          fun take_TyVars TyVartys = 
              case TyVartys of
               []                   => []
              | (TYVARty(_, tv))::tl => tv ::(take_TyVars tl)
              | _                    => raise Match
          val TVtys      = mk_new_TyVartys 4
          val TVs        = take_TyVars TVtys 
          val int_ty     = mk_tyConst "int"
          val bool_ty    = mk_tyConst "bool"
          val string_ty  = mk_tyConst "string"
          val unit_ty    = mk_tyConst "unit"
          val pair_ty    = mk_PAIRty(nth(TVtys, 0),  nth(TVtys, 1))
          val fn_ty      = mk_FNty(nth(TVtys, 2), nth(TVtys, 3))
          val dyn_tycon  = mk_TyCon "dyn"
          val int_con    = Con.mk_Con "in_INT"
          val bool_con   = Con.mk_Con "in_BOOL"
          val string_con = Con.mk_Con "in_STRING"
          val unit_con   = Con.mk_Con "in_UNIT"
          val pair_con   = Con.mk_Con "in_PAIR"
          val func_con   = Con.mk_Con "in_FUNC"
          val bindlst    = [(int_con,    Some int_ty, false),
                            (bool_con,   Some bool_ty, false),
                            (string_con, Some string_ty, false),
                            (unit_con,   Some unit_ty, false),
                            (pair_con,   Some pair_ty, false),
                            (func_con,   Some fn_ty, false)]
          fun get_conbind bl = case bl of Some conbind => conbind | _ => raise Match
          val conbind = get_conbind(mk_CONBINDs bindlst)
      in
          DATATYPEdec((), DATBIND((), TVs, dyn_tycon, conbind, None))
      end 

      (* make a check-operation : fn <in_name> x => x | raise TypeError *)
  fun mk_checkfun in_name =
      let val mrule1 = MRULE((), mk_CONSpat(in_name, false, mk_LONGIDatpat((), "x")),
                                 ATEXPexp((), mk_IDENTatexp((), "x", false)))
          val mrule2 = MRULE((), ATPATpat((), WILDCARDatpat ()),
                                 mk_RAISEexp "TypeError")
          val match  = MATCH((), mrule1, Some(MATCH((), mrule2, None)))
      in
          FNexp((), match)
      end

  local open SchemeTypes in

  fun type_tag2str tag =
      case tag of
        FUNC   => "FUNC"
      | BOOL   => "BOOL"
      | NIL    => "UNIT"
      | PAIR   => "PAIR"
      | STRING => "STRING"
      | NUMBER => "INT"
      | _      => raise TranslationUnimplemented
  fun type_tag2in_name tag    = "in_"^(type_tag2str tag)
  fun type_tag2check_name tag = "check_"^(type_tag2str tag)

  fun mk_check_dec type_tag = 
      let val check_name              = type_tag2check_name type_tag
          val in_name                 = type_tag2in_name type_tag
          val checkfun                = mk_checkfun in_name 
      in
          mk_simpleVALdec(check_name, checkfun) 
      end

      (* global declaration of names of negative primitives *)
  val check_decs = map mk_check_dec [BOOL, STRING, NUMBER, NIL, PAIR, FUNC]

(* construction of coercion expressions *)
                    
  fun mk_TAG type_tag = ATEXPexp((), mk_IDENTatexp((), type_tag2in_name type_tag, false))
  fun mk_CHECK type_tag = ATEXPexp((), mk_IDENTatexp((), type_tag2check_name type_tag, false))
  fun mk_CID() = ATEXPexp((), mk_IDENTatexp((), "ID", false))
  fun mk_CAPP(c, e) = mk_APPexp(c, e)

       (* construct n'th projection, #n = fn {n : var ...} => var *)
  fun mk_proj n = 
      let val varname = fresh_varname()
          val varexp = ATEXPexp((), mk_IDENTatexp((), varname, false))
          val recpat = ATPATpat((), mk_flexRECORDatpat(mk_lab n, ATPATpat((), 
                                    mk_LONGIDatpat((), varname))))
          val mrule = MRULE((), recpat, varexp)
          val match = MATCH((), mrule, None)
      in
          FNexp((), match)
      end
  
      (* construct induced coercion application where c,d,e are SML-exps *)
  fun mk_map(type_tag, c, d, e) = 
      let  fun mk_arrow_coercion(c,d,e) = (*  (c -> d)  *)
               let val x        = fresh_varname()
                   val idx      = ATEXPexp((), mk_IDENTatexp((), x, false))
                   val app_cx   = mk_APPexp(c, idx)
                   val app_Mcx  = mk_APPexp(e, app_cx)
                   val app_dMcx = mk_APPexp(d, app_Mcx)
                   val mrule    = MRULE((), ATPATpat((), mk_LONGIDatpat((), x)), app_dMcx)
                   val match    = MATCH((), mrule, None)
               in
                   FNexp((), match)
               end
           fun mk_pair_coercion(c,d,e) =  (*  (c * d)  *)
               let val proj1   = mk_proj 1
                   val proj2   = mk_proj 2
                   val proj1_e = mk_APPexp(proj1, e)
                   val proj2_e = mk_APPexp(proj2, e)
               in
                   mk_pair_exp(mk_APPexp(c, proj1_e), mk_APPexp(d, proj2_e))
               end
      in
           case type_tag of
             FUNC => mk_arrow_coercion(c,d,e)
           | PAIR => mk_pair_coercion(c,d,e)
           | _    => raise TranslationUnimplemented
      end 

  end (* local open SchemeTypes *)

      (* construct SML-expression for coercion composition c o d, when c,d are SML-exps *)
  fun mk_comp(c,d) = 
      let val comp = ATEXPexp((), mk_IDENTatexp((), "o", true))
          val pair = mk_pair_exp(c, d)
      in
          mk_APPexp(comp, pair)
      end   

      (* construct the identity function *)
  fun mk_ID() = 
      let val mrule = MRULE((), ATPATpat((), mk_LONGIDatpat((), "x")), 
                                ATEXPexp((), mk_IDENTatexp((), "x", false)))
          val match = MATCH((), mrule, None)
      in
          FNexp((), match)
      end
  
(* top level declarations *)

  val topdecs = [mk_dyn_dec()] @ [mk_EXCEPTIONdec("TypeError", None)] @ 
                [mk_simpleVALdec("ID", mk_ID())] @ check_decs 


(* test functions *)

  val varexp = ATEXPexp((), mk_IDENTatexp((), "x", false))
  val trueexp = ATEXPexp((), mk_IDENTatexp((), "true", false))
  fun mk_TYPEDexp(e, ty) = TYPEDexp((), e, ty)
  fun mk_let(dec, exp) = ATEXPexp((), LETatexp((), dec, exp))
  
  fun mk_SEQdec declst =
      fold (fn (dec1, dec2) => SEQdec((), dec1, dec2)) declst (EMPTYdec ())
  fun mk_topexp e = mk_let(mk_SEQdec topdecs, e)

end
end







