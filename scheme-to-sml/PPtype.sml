
structure CoercionTranslation (* : COERIONTRANSLATION *) =
  struct

  local open DecGrammar Datum Exp Ident Lab SCon Excon
             MlAst PrettyPrint (* Coercion *) 
             UnionFind  PolyType Environment 
  in

local open Type  SchemeGeneral
in

  
  datatype coercion =
     CVAR of int
   | IDC 
   | ERR
   | TAG of type_tag
   | CHECK of type_tag
   | MAP of type_tag * coercion list
   | COMP of coercion * coercion

  type coercion_sig = atype * atype

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

                   
  
  fun Tcoercion(t1, t2) =

      case (utype t1, utype t2) of
      (SIMPLE(tt, _), SIMPLE(tt',_)) =>
           if tt = tt' then IDC
           else 
                (* paren(mk_ERR()) *) ERR
     |  (SIMPLE(tt, _), DYN _) => TAG tt
     |  (DYN _, SIMPLE (tt, _)) => CHECK tt
     |  (TVAR _, _) => 
           if equal(t1, t2) then IDC
           else CVAR (new_cv(t1, t2))
                (* Fresh coercion variable. 
                   In a better implementation all coercion variables
                   with identical signatures are collapsed 
                   (at an earlier stage.) 
                   This requires equality on recursive type expressions 
                   which we don't have as yet.           
                *)
     |  (_, TVAR _) => 
           if equal(t1, t2) then IDC
           else CVAR(new_cv(t1, t2))
                (* Fresh coercion variable. 
                   See previous case for comment on this.
                *)
    |  _ =>  IDC


 


  end (* local cvs = ... *)

               
      (* given coercion signature s = (T1, T2) and Sml-exp e, 
         construct Sml-exp for (c e) 
      *)
  fun TC s =
               let val (src, tgt) = s
                   val coercion = Tcoercion s
                   val PPsrc = ty2PPty src
                   val PPtgt = ty2PPty tgt
               in
                   (coercion, PPsrc, PPtgt)
               end
      





      (* Translate Scheme Datum to SML atexp *)
  fun Tdatum d =
       let fun booldat (a, b)      = BOOLDAT(TC a, b)
           fun chardat (a, c)      = CHARDAT(TC a, c)
           fun stridat (a, s)      = STRIDAT(TC a, s)
           fun symbdat (a, s)      = SYMBDAT(TC a, s)
           fun numbdat (a, n)      = NUMBDAT(TC a, n)
           fun vectdat x           = raise TranslationUnimplemented
           fun pairdat (a, d1, d2) = PAIRDAT(TC a, d1, d2)
           fun nildat a            = NILDAT(TC a)

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






  fun tyscheme2PPty t = 
      case t of 
        TSCHEME (siglist, ty) => 
           PPTSCHEME (map (fn (t1, t2) => (ty2PPty t1, ty2PPty t2)) siglist,
                      ty2PPty ty)
      | TYPE ty => PPTYPE(ty2PPty ty)





      (* Translate Scheme Expression to SML expression *)
  fun Texp e  =
          let
          fun get_var_str (s, _) = s
          fun noexp a = NOEXP(TC a)
          fun literal d = LITERAL(Tdatum d) 
          fun variable(ea, (s, tyscheme)) = 
              (* Translation of typed variable s in three cases,
                 according to whether tyscheme has (1) no quantified
                 coercion parameters (2) one quantified parameter, or
                 (3) more than one quantified parameter. These cases
                 are in accordance with the function abstract_cvars
                 below, to which the reader is referred. 
             *)
                             
                    case tyscheme of
                      TYPE _ => VARIABLE (TC ea, (s, ([], tyscheme2PPty tyscheme))) 
                                           (* no quantified *)
                    | TSCHEME(siglist, _) =>
                        (case siglist of
                           nil => VARIABLE(TC ea, (s, ([], tyscheme2PPty tyscheme))) 
                                             (* no quantified *)
                         | [signat] => (* one quantified *)
                                  let val coercion = TC signat 
                                  in
                                      VARIABLE(TC ea, (s, ([coercion], 
                                                       tyscheme2PPty tyscheme)))
                                  end
                         | _ => (* more than one quantified *)
                                let val coercions = map TC siglist
                                in
                                   VARIABLE(TC ea, (s,(coercions, 
                                                       tyscheme2PPty tyscheme)))
                                end)
                 

          fun call(ea, e, l)  = CALL(TC ea, e, l)
          fun lambda(ea, f, e)   = LAMBDA(TC ea, f, e)
     
          fun ifexp(ea, e, e', e'')  = IF(TC ea, e, e', e'')
          fun assign(ea, v, e)    = raise TranslationUnimplemented
                                    (* right-assocviative argument tuple *)
          fun pairarg(la, e, l)   = PAIRARG(TC la, e, l)
          fun nullarg la = NULLARG (TC la)
              (* parameter-expressions get translated into patterns *)
          fun avarpar (s, t) = AVARPAR (s, ty2PPty t)
          fun apairpar((s, t), f)   = APAIRPAR((s, ty2PPty t), f) 
          fun anullpar()            = ANULLPAR
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



  fun Tdef (DEFINE ((name, ty), body)) =
      DEFINE((name, ty2PPty ty), Texp body)

      (* analyze a definition *)
  fun Adef instream =
      let val ud as (DEFINE ((name, ty), body)) = parse_def instream
          val ad = type_def ud topenv
          val td = Tdef ad
      in
          td
      end


end (* local open  Type SchemeGeneral*)
end
end









