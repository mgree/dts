structure Translation (* : TRANSLATION *) =
  struct

  local open DecGrammar Datum Exp Ident Lab SCon Excon
             MlAst PrettyPrint Coercion 
             UnionFind  PolyType Environment 
  in

local open Type  SchemeGeneral
in


 
(* translation of single coercions to exp *)
      (* translate coercion parameter *)
  fun mk_CVARname n = "CV"^(Integer.makestring n) 
  fun mk_CVARexp n = ATEXPexp((), mk_IDENTatexp((), mk_CVARname n, false))
  fun mk_LST2PAIR() = ATEXPexp((), mk_IDENTatexp((), "LST2PAIR", false))
  fun mk_PAIR2LST()  = ATEXPexp((), mk_IDENTatexp((), "PAIR2LST", false))


      (* decide whether k(t1, t2) is the identity *)
  fun isid(t1, t2) =
      case (utype t1, utype t2) of
        (SIMPLE(tt, _), SIMPLE(tt',_)) => tt = tt'
     |  (DYN _, DYN _) => true
     |  (TVAR _, _) => 
           if equal(t1, t2) then true
           else false
     |  (_, TVAR _) => 
           if equal(t1, t2) then true
           else false
    |   _ => false

      (* Translate coercion_sig (t1, t2) to Sml-exp 
         representing the canonical coercion k(t1, t2).
         NB! This function assumes that k(t1, t2) is NOT
         the identity, i.o.w. it should only be called
         under the condition isid(t1, t2) = false.
      *)
  local val cvs = ref (empty_env: (coercion_sig, int) env) 
                                (* list of indices of coercion vars
                                    generated during coercion translation,
                                    kept for subsequent abstraction. 
                                 *)

      (* is signature s in cvs *)
  fun in_cvs s =
      let fun look(s as (t1, t2), Env) =
              case Env of
                [] => Fail ()
              | ((t1', t2'), n)::Env' => if equal(t1, t1') andalso
                                            equal(t2, t2') then OK n
                                         else look(s, Env') 
      in
          look (s, !cvs)
      end                   


           (* create a new coercion name with signature s,
              if that signature is not already associated with a name
              in cvs; the new signature is recorded (with side-effect)
              in cvs; otherwise, return the name associated with s
              in cvs. 
              This function is only used in Tcoercion. *)
       fun new_cv s =
           case in_cvs s of
             Fail()  => let val n = counter() 
                        in (* generate new coercion index *)
                           (cvs := extend(!cvs, s, n); (* record new index *)
                            n)
                        end
           | OK n => n (* coercion index of that sig already
                                      generated. Return that index.     
                       *)          
  in (* local val cvs ... *)

  fun get_cvs() = !cvs
  fun reset_cvs() = cvs := (empty_env: (coercion_sig, int) env) 

(*
  fun isrecursive t = let val col = get #color t
                      in
                          case !col of
                            BLACK => true
                          | _ => false
                      end
*)

  fun isrecursive t = case !(get #name t) of
                        None => false
                      | _ => true
                   
  
  fun Tcoercion(t1, t2) =

      (

(* DO NOT mix this with other write-outs of types, since
   the administrative refs like *isnamed* get destructively
   updated !

       if isrecursive t1 then 
            (output(std_out, "\nInner type recursive:\n");  
             output(std_out, ty2str t1))
          else ();
       if isrecursive t2 then 
            (output(std_out, "\nOuter type recursive:\n"); 
             output(std_out, ty2str t2)) 
          else ();
*)

      (case (utype t1, utype t2) of
        (SIMPLE (LST, _), SIMPLE(PAIR, _)) =>
           mk_LST2PAIR() 
      | (SIMPLE(PAIR, _), SIMPLE(LST,_)) =>
           mk_PAIR2LST()
      | (SIMPLE(tt, _), SIMPLE(tt',_)) =>
           if tt = tt' then mk_CID()
           else 
                (* paren(mk_ERR()) *) mk_ERR()
     |  (SIMPLE(tt, _), DYN _) => paren(mk_TAG tt)
     |  (DYN _, SIMPLE (tt, _)) => paren(mk_CHECK tt)
     |  (TVAR _, _) => 
           if equal(t1, t2) then mk_CID()
           else mk_CVARexp (new_cv(t1, t2))
                (* Fresh coercion variable. 
                   In a better implementation all coercion variables
                   with identical signatures are collapsed 
                   (at an earlier stage.) 
                   This requires equality on recursive type expressions 
                   which we don't have as yet.           
                *)
     |  (_, TVAR _) => 
           if equal(t1, t2) then mk_CID()
           else mk_CVARexp (new_cv(t1, t2))
                (* Fresh coercion variable. 
                   See previous case for comment on this.
                *)
    |  _ =>  mk_CID())


 ) 


  end (* local cvs = ... *)

               
      (* given coercion signature s = (T1, T2) and Sml-exp e, 
         construct Sml-exp for (c e) 
      *)
  fun TCapp(s, e) =
          if isid s  then e (* discard coercion if trivial *)
          else
               let val cexp = Tcoercion s
               in
                   mk_APPexp(cexp, e)
               end
      
end (* local open  Type SchemeGeneral*)




      (* Translate Scheme Datum to SML atexp *)
  fun Tdatum d =
      let  fun str2int s = 
               let fun strlst2int l = 
                       case l of
                         [] => 0
                       | x::tl => (ord x - (ord "0")) + 10 * (strlst2int tl)
               in
                   strlst2int (explode s)
               end
           fun booldat (a, b)      = TCapp(a, ATEXPexp((), mk_IDENTatexp((), 
                                              Bool.string b, false)))
           fun chardat (a, c)      = TCapp(a, ATEXPexp((),SCONatexp ((), 
                                              SCon.STRING c)))
           fun stridat (a, s)      = TCapp(a, ATEXPexp((), SCONatexp ((), 
                                              SCon.STRING s)))
           fun symbdat (a, s)      = TCapp(a, ATEXPexp((), SCONatexp ((), 
                                              SCon.STRING s)))
           fun numbdat (a, n)      = TCapp(a, ATEXPexp((), SCONatexp ((), 
                                              SCon.INTEGER (str2int n))))
           fun vectdat x           = raise TranslationUnimplemented
           fun pairdat (a, d1, d2) = TCapp(a, ATEXPexp((), 
                                              mk_pair_atexp((), d1, d2)))
           fun nildat a            = TCapp(a, ATEXPexp((), 
                                              LISTatexp((), [])))

           val dhom                = DHOM {booldat = booldat,
                                           chardat = chardat,
                                           stridat = stridat,
                                           symbdat = symbdat,
                                           numbdat = numbdat,
                                           vectdat = vectdat,
                                           pairdat = pairdat,
                                           nildat  = nildat}
           in
               apply_dhom dhom d
           end




  val Tenv = [("null?", "isnull"), ("equal?", "isequal")]


      (* Translate Scheme Expression to SML expression *)
  fun Texp e  =
          let
          fun get_var_str (s, _) = s
          fun noexp a = RAISEexp(Tinfo a, ATEXPexp(dummy_info(), 
                                 mk_IDENTatexp(dummy_info(), "Match", false)))
          fun literal d = Tdatum d 
          fun variable(ea, (s, tyscheme)) = 
              (* Translation of typed variable s in three cases,
                 according to whether tyscheme has (1) no quantified
                 coercion parameters (2) one quantified parameter, or
                 (3) more than one quantified parameter. These cases
                 are in accordance with the function abstract_cvars
                 below, to which the reader is referred. 
             *)

                let val s' = (lookup s Tenv) handle Lookup => s
                    val varexp = ATEXPexp((), mk_IDENTatexp((), s', false))
                in
                    case tyscheme of
                      TYPE _ => TCapp(ea, varexp) 
                                           (* no quantified *)
                    | TSCHEME(siglist, _) =>
                        (case siglist of
                           nil => TCapp(ea, varexp) 
                                             (* no quantified *)
                         | [s] => (* one quantified *)
                                  let val coercion = Tcoercion s 
                                      val appexp = mk_APPexp(varexp, 
                                                             coercion)
                                  in
                                      TCapp(ea, appexp)
                                  end
                         | _ => (* more than one quantified *)
                                let val coercions = map Tcoercion siglist
                                    val appexp = mk_APPexp(varexp,
                                                          ATEXPexp((),
                                                          NTUPLEatexp((),
                                                          coercions)))
                                in
                                    TCapp(ea, appexp)
                                end)
                  end (* let val s' ... *)

          fun call(ea, e, l)  = TCapp(ea, mk_APPexp(e, l))
          fun lambda(ea, f, e)   = 
              let val mrule = MRULE(dummy_info(), f, e)
                  val match = MATCH(dummy_info(), mrule, None)
              in
                  TCapp(ea, paren(FNexp((), match)))
              end 
          fun ifexp(ea, e, e', e'')  = TCapp(ea, paren(IFexp((), e, e', e''))) 
          fun assign(ea, v, e)    = raise TranslationUnimplemented
                                    (* right-assocviative argument tuple *)
          fun pairarg(la, e, l)   = ATEXPexp((), NTUPLEatexp((),[e,l])) 
          fun nullarg la = ATEXPexp((),LISTatexp((),[]))
              (* parameter-expressions get translated into patterns *)
          fun avarpar tv        = mk_ATPATpat((), get_var_str tv)
          fun apairpar(tv, f)   = 
              let val varpat = mk_ATPATpat((), get_var_str tv)
              in  (* right-associative tuple pattern *)
                  ATPATpat((), NTUPLEatpat((),[varpat,f]))
              end
          fun anullpar()            = ATPATpat((), LISTatpat((),[]))
          val ehom                  = EHOM{noexp    = noexp,
                                           literal  = literal,
                                           variable = variable,
                                           call     = call,
                                           lambda   = lambda,
                                           ifexp    = ifexp,
                                           assign   = assign,
                                           pairarg  = pairarg,
                                           nullarg  = nullarg,
                                           avarpar  = avarpar,
                                           apairpar = apairpar,
                                           anullpar = anullpar}
          in
              apply_ehom ehom e 
          end



       (* Coercion abstraction :
          T(Lambda p1 ... pn. e) = fn (p1,...,pn) => T(e)
          for !cvs = [p1,...,pn].
          There are three cases, according to n=0, n=1, n>1, where
          we get, respectively,
          (1) e
          (2) fn p1 => e
          (3) fn (p1,...,pn) => e
       *)
  fun abstract_cvars(cvnamelist, defname, e) =
      let
        fun mk_specialLETdec(dn, exp) =
              ATEXPexp((), LETatexp((), 
                           mk_simpleRECVALdec(dn, exp),
                           ATEXPexp((), mk_IDENTatexp((), dn, false))))
        val e' = if cvnamelist = [] then e
                     else mk_specialLETdec(defname, e)
        in
              case cvnamelist of
                 []       => e'
              |  [cvname] => mk_simpleFNexp(cvname, e')
              |  _        => mk_tupleFNexp(cvnamelist, e')
        end


      (* Remove toplevel parentheses. They were only introduced 
         in the first place because the PP doesn't handle parens properly.
         With a better PP this should all be ditched.
      *)
  fun clean exp =
        case exp of
          ATEXPexp((), PARatexp((), e)) => e
        | _ => exp
      
      (* Translate a single definition by first translating the
         body and then abstracting over the coercion vars (cvs) 
         introduced therein.
         Every def is translated into a VAL REC declaration.
      *)
  fun Tdef (DEFINE((defvar, _), e)) =
        (reset_cvs(); (* CRUCIAL: initialize cvs *)
        let val exp = Texp e (* coercion environment cvs
                                gets extended to contain just
                                the coercion variables (indices) 
                                occurring inside e. Ultimately, the 
                                extension occurs in function
                                Tcoercion.
                             *)
            val exp' = clean exp (* hack necessary because of wrong
                                    parenthesization in that g.d. PP *)
            val cvars = get_cvs() (* get the newly extended environment *)
            val SMLdef =
                   case cvars = empty_env of
                     true => (output(std_out, "\nEMPTY\n");
                              mk_simpleRECVALdec(defvar, exp'))  
                             (* no coercion vars *)
                  | false => let val cvnamelist = map mk_CVARname 
                                                      (values cvars)
                      in
                          mk_simpleRECVALdec(defvar, 
                          abstract_cvars(cvnamelist, defvar, exp'))
                      end
       in
            SMLdef
       end)
                
  fun Tprogram p =
      let val programdecs    = map Tdef p
      in
         mk_SEQdec programdecs
      end

  fun write_topdecs os =
      let val topdecstr =
"(* TOP LEVEL DECLARATIONS : *) \n\n"^
"datatype('a1, 'a2, 'a3, 'a4) dyn =\n"^
"in_INT    of int |\n"^
"in_BOOL   of bool |\n"^
"in_STRING of string |\n"^
"in_LST    of 'a1 list |\n"^
"in_NIL    of 'a1 list |\n"^
"in_PAIR   of 'a1 * 'a2 |\n"^
"in_FUNC   of 'a3 -> 'a4;\n"^
"\n"^
"exception TypeError;\n"^
"exception EmptyList;\n"^
"\n"^
"val ID           = fn x => x;\n"^ 
"val ERR          = fn x => raise TypeError;\n"^
"val check_BOOL   = fn in_BOOL x   => x | _ => raise TypeError;\n"^ 
"val check_STRING = fn in_STRING x => x | _ => raise TypeError;\n"^ 
"val check_INT    = fn in_INT x    => x | _ => raise TypeError;\n"^ 
"val check_LST    = fn in_LST x    => x | _ => raise TypeError;\n"^ 
"val check_PAIR   = fn in_PAIR x   => x | _ => raise TypeError;\n"^ 
"val check_FUNC   = fn in_FUNC x    => x | _ => raise TypeError;\n"^
"val PAIR2LST = fn (x, y) => x::y;\n"^ 
"val LST2PAIR = fn (x::y) => (x, y) | _ => raise EmptyList;\n"^
"\n"^
"val isnull  = fn ([], [])       => true | _ => false;\n"^
"val isequal = fn (x, (y, []))   => x = y;\n"^
"val car     = fn ((x, y), [])   => x | _ => raise TypeError;\n"^
"val cdr     = fn ((x, y), [])   => y | _ => raise TypeError;\n"^
"val cons    = fn ((x, (y, []))) => (x, y) | _ => raise TypeError;\n"^
"\n\n (* TRANSLATED PROGRAM : *) \n\n"
in
   output(os, topdecstr)
end

  fun PPprog p = 
      (write_topdecs std_out;
       PPsmlDec (Tprogram p))

 fun  transl_def() = 
      let val ud = parse_def std_in 
      in
         PPprog [type_def ud topenv]
      end



fun translate_program(p, Env) =
    case p of
      [] => []            
    | d::p' => let val ad as (* type a single definition *) 
                        (DEFINE((name, ty), body)) = 
                           (output(std_out, "\n calling type_def...\n");
                                  type_def d Env )       
                     val SMLdef = (output(std_out, "\n calling Tdef...\n");
                                  Tdef ad )
                        (* translate to SML declaration. this defines the 
                           environment cvs of coercion signatures. *)
                     val csigs = (output(std_out, "\n calling keys...\n");
                                  keys (get_cvs())) 
                         (* get the newly defined environment of coercion 
                            variables and extract the signatures. *)  
                     val tyscheme = (output(std_out, "\n calling close...\n");
                                     close(csigs, ty))
                         (* tyscheme is closure of inferred type of the
                            definition just translated *)
                     val Env' = (output(std_out, "\n calling extend...\n");
                                 extend(Env, name, tyscheme)) 
                 in  (output(std_out, "\n calling translate...\n");
                     SMLdef :: (translate_program(p', Env')))
                 end
                      
                     
  fun PPdeclst decl = 
      (write_topdecs std_out;
       PPsmlDec (mk_SEQdec decl))


local open Type SchemeGeneral in
                
  fun scheme2sml instream =
      let val p = parse_program instream (* parse input stream into  
                                    preattributed definition list *)
                      (* translate to SML AST *)
          val SMLdecs = translate_program(p, topenv)
      in  
          (* send AST to pretty printer *)
          (PPdeclst SMLdecs;

           output(std_out, "\n\nTOPTYPE :\n");
           let val Some t = !topty in
               output(std_out, (ty2str t)^"\n")
           end      
           )
      end

end 




  fun T s = let val ip = open_in s
            in
                scheme2sml ip
            end




end
end









