structure SchemeTranslation (* : SCHEMETRANSLATION *) =
  struct

  local open DecGrammar SchemeDatum KernelExp Ident Lab SCon Excon
             MlAst KernelParse PrettyPrint SchemeCoercions
             SchemeProcess SchemeVariables KernelAttributes
  in

(* translation of single coercions to exp *)
      (* translate coercion parameter *)
  fun mk_CVARname n = "CV"^(Integer.makestring n) 
  fun mk_CVARexp n = ATEXPexp((), mk_IDENTatexp((), mk_CVARname n, false))


  val cv = ref ([]: (int * SchemeTypes.atype * SchemeTypes.atype) list);

      (* translate coercion to Sml-exp *)
  fun Tcoercion c =
      let val (cop, t1, t2) = c 
          fun seen_sigB4(s, cvl) =
              case cvl of
                [] => None
              | (k, t1', t2')::cvs => if equal_sig((t1', t2'), s) then Some k
                                    else seen_sigB4(s, cvs)
      in
      case cop of
        CVAR n   => (case seen_sigB4((t1, t2), !cv) of
                       None   => (cv := (n, t1, t2)::(!cv);
                                  mk_CVARexp n)  (* new coercion variable needed *)
                     | Some k =>  mk_CVARexp k)   (* coercion variable already generated at signature,
                                                    and we collapse all variables with identical signature *)
      | IDC      => mk_CID()
      | TAG t    => mk_TAG t
      | CHECK t  => mk_CHECK t
      | MAP(t, [c1, c2]) => 
           let val varname = fresh_varname()
               val varexp = ATEXPexp((), mk_IDENTatexp((), varname, false))
               val Tc1 = Tcoercion c1
               val Tc2 = Tcoercion c2
               val cmap = mk_map(t, Tc1, Tc2, varexp)
           in
               mk_simpleFNexp(varname, cmap)
           end
      | COMP(c1, c2) => 
            let val Tc1 = Tcoercion c1
                val Tc2 = Tcoercion c2
            in
                mk_comp(Tc1, Tc2)
            end
      | _        => raise TranslationUnimplemented
      end
               
      (* given coercion c and Sml-exp e, construct Sml-exp for (c e) *)
  fun TCapp(c, e) = 
      let val c' = interpret_coercion c
      in
          if isid c' then e (* discard coercion if trivial *)
          else
               let val cexp = Tcoercion c'
               in
                   mk_APPexp(cexp, e)
               end
      end


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
           fun booldat (a, b)      = TCapp(a, ATEXPexp((), mk_IDENTatexp((), Bool.string b, false)))
           fun chardat (a, c)      = TCapp(a, ATEXPexp((),SCONatexp ((), STRING c)))
           fun stridat (a, s)      = TCapp(a, ATEXPexp((), SCONatexp ((), STRING s)))
           fun symbdat (a, s)      = TCapp(a, ATEXPexp((), SCONatexp ((), STRING s)))
           fun numbdat (a, n)      = TCapp(a, ATEXPexp((), SCONatexp ((), INTEGER (str2int n))))
           fun vectdat x           = raise TranslationUnimplemented
           fun pairdat (a, d1, d2) = TCapp(a, ATEXPexp((), mk_pair_atexp((), d1, d2)))
           fun nildat a            = TCapp(a, ATEXPexp((), RECORDatexp((), None)))
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


      (* Translate Scheme Expression to SML expression *)
  fun Texp e =
      let fun get_param_str (p : KernelAttributes.parameter) =
              case p of
                LAMBOUND (s, _) => s
              | LETBOUND (s, _) => s
              | FREE (s, _)     => s
          fun get_var_str (v: KernelAttributes.variable) = let val (p, _) = v in get_param_str p end 

          fun noexp a               = RAISEexp(Tinfo a, ATEXPexp(dummy_info(), 
                                                        mk_IDENTatexp(dummy_info(), "Match", false)))
          fun literal(ea, d)        = TCapp(ea, Tdatum d) 
          fun variable(ea, v)       = TCapp(ea, ATEXPexp(Tinfo ea, mk_IDENTatexp((), get_var_str v, false)))
          fun call(ea, e, l)        = TCapp(ea, APPexp((), paren e, PARatexp(dummy_info(), l)))
          fun lambda(ea, f, e)      = 
              let val mrule = MRULE(dummy_info(), f, e)
                  val match = MATCH(dummy_info(), mrule, None)
              in
                  TCapp(ea, FNexp((), match))
              end 
          fun ifexp(ea, e, e', e'') = 
              let val mrule_true  = MRULE(dummy_info(), mk_ATPATpat(dummy_info(), "true"), e')
                  val mrule_false = MRULE(dummy_info(), mk_ATPATpat(dummy_info(), "false"), e'')
                  val match = MATCH(dummy_info(), mrule_true, Some(MATCH(dummy_info(), mrule_false, None)))
                  val cond_fn = FNexp(dummy_info(), match)
              in
                  TCapp(ea, APPexp((), paren cond_fn, PARatexp(dummy_info(), e)))
              end  
          fun assign(ea, v, e)      = raise TranslationUnimplemented
          fun pairarg(la, e, l)     = 
              let val p = mk_pair_atexp(dummy_info(),e,l) in
                  (* argument pair (x, y) is turned into argument list
                     [x, y] = op::(x, y) *)
                  TCapp(la, APPexp((), mk_cons(), p))
              end
          fun nullarg la            = TCapp(la, ATEXPexp((), mk_IDENTatexp(Tinfo la, "nil", false)))
              (* parameter-expressions are translated into patterns *)
          fun avarpar(fa, p)        = mk_ATPATpat((), get_param_str p)
          fun apairpar(fa, p, f)    = 
              let val p' = mk_RECORDatpat(mk_lab 1, mk_ATPATpat((), get_param_str p), mk_lab 2, f)
              in
                  mk_CONSpat("::", true, p')
              end
          fun anullpar fa           = mk_ATPATpat((), "nil")
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



  fun abstract_cvars(cvl, e) =
      case cvl of
        []            => e
      | (n, _,_)::cvs => let val cvar_name = mk_CVARname n
                             val fnexp     = mk_simpleFNexp(cvar_name, e)
                         in
                             abstract_cvars(cvs, fnexp)
                         end                   


  fun PP smlExp = PPsmlAst smlExp;

  fun translate() =
      let val e = process std_in
          val Te = Texp e
          val absTe = abstract_cvars(!cv, Te)
          val topexp = mk_topexp absTe 
      in
          (cv := []; PPsmlAst topexp)
      end 

  fun tstty ty = let val tye = mk_TYPEDexp(varexp, ty) in PP tye end
  



end
end



