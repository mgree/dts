structure ProcessKernel (*: PROCESSKERNEL *) =
  struct
  local open SchemeDatum KernelExp KernelTypes 
                         SchemeCoercions SchemePolyTypes KernelConstraints
  in

  (* 1st stage: parse input stream into an unannotated exp *)

  val no_atts = INIT { parameter = id,
                       variable = id,
                       exp = id,
                       args = id,
                       formals = id }
                                                  
  fun parse ip = read_exp no_atts ip


  (* 2nd stage: annotate all expressions with fresh coercions and
     resolve variable references *)

  local   
  fun booldat (a,b) = BOOLDAT (new_coercion(), b)
  fun chardat (a,c) = CHARDAT (new_coercion(), c)
  fun stridat (a,c) = STRIDAT (new_coercion(), c)
  fun symbdat (a,s) = SYMBDAT (new_coercion(), c)
  fun vectdat (a,l) = VECTDAT (new_coercion(), l)
  fun pairdat (a,d1,d2) = PAIRDAT (new_coercion(), d1,d2)
  fun nildat a = NILDAT (new_coercion())
  fun noexp a Env = NOEXP (new_coercion())
  fun literal (a,d) Env = LITERAL ((new_coercion()),d)
  fun variable (a,v) Env = 
        VARIABLE (new_coercion(), 
                  (lookup v Env handle Lookup => FREE (v, new_typevar()),
                   ref []))
  fun call (a,e,l) Env = CALL (new_coercion(), e Env, l Env)
  fun lambda (a,f,e) Env = 
        let val (st,fenv) = f 
        in LAMBDA (new_coercion(), st, e (add (Env, fenv)))
        end
  fun ifexp (a,e,e',e'') Env = IF (new_coercion(), e Env, e' Env, e'' Env)
  fun assign (a,v,e) Env = ASSIGN (new_coercion(), lookup v Env, e Env)
  fun pairarg (a,e,l) Env = PAIRARG (new_coercion(), e Env, l Env)
  fun nullarg a Env = NULLARG (new_coercion())
  fun avarpar (a,p) = 
      (AVARPAR ((),p), make_env (p, LAMBOUND (p, new_typevar())))
  fun apairpar (a,p,f) = 
      let val (st, env) = f
      in 
      (lookup p env; raise StaticAnalysisError "Duplicate parameter")
       handle Lookup => 
          (APAIRPAR ((),p,st), 
           extend (env, p, LAMBOUND (p, new_typevar())))
      end 
  fun anullpar a = (ANULLPAR (), empty_env)
  val dvar_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val evar_hom = EHOM { noexp = noexp,
                   literal = literal,
                   variable = variable,
                   call = call,
                   lambda = lambda,
                   ifexp = ifexp,
                   assign = assign } 
  val fvar_hom = FHOM { avarpar = avarpar,
                   apairpar = apairpar,
                   anullpar = anullpar }
  val avar_hom = LHOM { pairarg = pairarg,
                   nullarg = nullarg }
  in
  val make_attributes = apply_ehom (dvar_hom, evar_hom, avar_hom, fvar_hom)
  end

 (* 2 1/2th stage: maybe translate to core language? *)

 (* 3rd stage: unify types according to equational type rules *)

  local   
  fun booldat (a,b) = hi_type a before unify (lo_type a, bool())
  fun chardat (a,c) = hi_type a before unify (lo_type a, char())
  fun stridat (a,c) = hi_type a before unify (lo_type a, string())
  fun symbdat (a,s) = hi_type a before unify (lo_type a, symbol())
  fun vectdat (a,l) = hi_type a before unify (lo_type a, vector l)
  fun pairdat (a,d1,d2) = hi_type a before unify (lo_type a, pair(d1,d2))
  fun nildat a = hi_type a before unify (lo_type a, unit())
  fun noexp a = hi_type a before unify (lo_type a, unspec())
  fun literal (a,d) = hi_type a before (unify (lo_type a, hi_type a);
                                        unify (lo_type a, d))
  fun variable (a,v) = hi_type a before unify (lo_type a, v)
  fun call (a,e,l) = hi_type a before unify (e, func(l, lo_type a))
  fun lambda (a,f,e) = hi_type a before unify (lo_type a, func(f,e))
  fun ifexp (a,e,e',e'') = hi_type a before (unify (lo_type a, e');
                                             unify (e', e'');
                                             unify (e, bool()))
  fun assign (a,v,e) = hi_type a before (unify (lo_type a, unspec());
                                        unify (v, e))
  fun pairarg (a,e,l) = hi_type a before unify (lo_type a, pair(e,l))
  fun nullarg a = hi_type a before unify (lo_type a, unit())
  fun avarpar (a,p) = #2 p
  fun apairpar (a,p,f) = pair (#2 p, f)
  fun anullpar a = unit()
  val dvar_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val evar_hom = EHOM { noexp = noexp,
                   literal = literal,
                   variable = variable,
                   call = call,
                   lambda = lambda,
                   ifexp = ifexp,
                   assign = assign } 
  val fvar_hom = FHOM { avarpar = avarpar,
                   apairpar = apairpar,
                   anullpar = anullpar }
  val avar_hom = LHOM { pairarg = pairarg,
                   nullarg = nullarg }
  in
  val unify_types = apply_ehom (dvar_hom, evar_hom, avar_hom, fvar_hom)
  end


 (* 4th stage: put coercions into normal form *)

  local   
  fun booldat (a,b) = ()
  fun chardat (a,c) = ()
  fun stridat (a,c) = ()
  fun symbdat (a,s) = ()
  fun vectdat (a,l) = ()
  fun pairdat (a,d1,d2) = ()
  fun nildat a = ()
  fun noexp a = ()
  fun literal (a,d) = ()
  fun variable (a,v) = ()
  fun call (a,e,l) = ()
  fun lambda (a,f,e) = determine a
  fun ifexp (a,e,e',e'') = ()
  fun assign (a,v,e) = ()
  fun pairarg (a,e,l) = ()
  fun nullarg a = ()
  fun avarpar (a,p) = ()
  fun apairpar (a,p,f) = ()
  fun anullpar a = ()
  val dvar_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val evar_hom = EHOM { noexp = noexp,
                   literal = literal,
                   variable = variable,
                   call = call,
                   lambda = lambda,
                   ifexp = ifexp,
                   assign = assign } 
  val fvar_hom = FHOM { avarpar = avarpar,
                   apairpar = apairpar,
                   anullpar = anullpar }
  val avar_hom = LHOM { pairarg = pairarg,
                   nullarg = nullarg }
  in
  val nf_coercions = apply_ehom (dvar_hom, evar_hom, avar_hom, fvar_hom)
  end

 (* 5th stage: collect constraints *)

  local   
  fun booldat (a,b) = constraints a
  fun chardat (a,c) = constraints a
  fun stridat (a,c) = constraints a
  fun symbdat (a,s) = constraints a
  fun vectdat (a,l) = APPEND [constraints a, l]
  fun pairdat (a,d1,d2) = APPEND [constraints a, d1, d2]
  fun nildat a = constraints a
  fun noexp a = constraints a
  fun literal (a,d) = APPEND [constraints a, d]
  fun variable (a,v) = constraints a
  fun call (a,e,l) = APPEND [constraints a, e, l]
  fun lambda (a,f,e) = APPEND [constraints a, e]
  fun ifexp (a,e,e',e'') = APPEND [constraints a, e, e', e'']
  fun assign (a,v,e) = APPEND [constraints a, e]
  fun pairarg (a,e,l) = APPEND [constraints a, e, l]
  fun nullarg a = constraints a
  fun avarpar (a,p) = ()
  fun apairpar (a,p,f) = ()
  fun anullpar a = ()
  val dvar_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val evar_hom = EHOM { noexp = noexp,
                   literal = literal,
                   variable = variable,
                   call = call,
                   lambda = lambda,
                   ifexp = ifexp,
                   assign = assign } 
  val fvar_hom = FHOM { avarpar = avarpar,
                   apairpar = apairpar,
                   anullpar = anullpar }
  val avar_hom = LHOM { pairarg = pairarg,
                   nullarg = nullarg }
  in
  val constraints = apply_ehom (dvar_hom, evar_hom, avar_hom, fvar_hom)
  end


 (* 6th stage: do induced unifications *)

  val induced_unifications = foreach equiv 

 (* 7th stage: set predecessors and successors *)

  val preds_succs = foreach pred_succ

 (* 8th stage: classify bound variables as either definitely used 
               or possibly unused *)

  fun split v EMPTYAO = (false, EMPTYAO)
    | split v (T as (SINGLE v')) = 
        if v = v' 
           then (true, EMPTYAO))
        else (false, T)
    | split v (OR (T1, T2)) =
        let val (b1, T1') = split v T1
            val (b2, T2') = split v T2
        in (b1 andalso b2, 
            if T1' = EMPTYAO andalso T2' = EMPTYAO
               then EMPTYAO
            else OR (T1', T2'))
        end
    | split v (AND (T1, T2)) =
        let val (b1, T1') = split v T1
            val (b2, T2') = split v T2
        in (b1 orelse b2,
            if T1' = EMPTYAO 
               then T2'
            else if T2' = EMPTYAO
                    then T1'
                 else AND (T1', T2'))
        end

  local   
  fun booldat (a,b) = EMPTYAO
  fun chardat (a,c) = EMPTYAO
  fun stridat (a,c) = EMPTYAO
  fun symbdat (a,s) = EMPTYAO
  fun vectdat (a,l) = EMPTYAO
  fun pairdat (a,d1,d2) = EMPTYAO
  fun nildat a = EMPTYAO
  fun noexp a = EMPTYAO
  fun literal (a,d) = EMPTYAO
  fun variable (a,v) = SINGLE (#1 v)
  fun call (a,e,l) = AND (e,l)
  val unusedvars = ref []
  fun lambda (a,f,e) = 
        fold (fn (v,T) => let val (b, T') = split v T
                          in if b 
                                then T'
                             else T' before unusedvars := v :: !unusedvars
                          end) f e
  fun ifexp (a,e,e',e'') = AND (e, OR(e',e'')) 
  fun assign (a,v,e) = AND (SINGLE (#1 v), e)
  fun pairarg (a,e,l) = AND (e,l)
  fun nullarg a = EMPTYAO
  fun avarpar (a,p) = p
  fun apairpar (a,p,f) = p :: f
  fun anullpar a = []
  val dvar_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val evar_hom = EHOM { noexp = noexp,
                   literal = literal,
                   variable = variable,
                   call = call,
                   lambda = lambda,
                   ifexp = ifexp,
                   assign = assign } 
  val fvar_hom = FHOM { avarpar = avarpar,
                   apairpar = apairpar,
                   anullpar = anullpar }
  val avar_hom = LHOM { pairarg = pairarg,
                   nullarg = nullarg }
  val free_pos_hom = apply_ehom (dvar_hom, evar_hom, avar_hom, fvar_hom)
  in
  fun free_pos_vars e = 
      (unusedvars := [];
       (free_pos_hom e, !unusedvars))
  end


  (* 9th stage: propagate positivity all throughout the constraints *)

  (* done by using function propagate on the type variables contained
     in the free_pos_vars plus the result type as positive type variables,
     and the types in the free variables as negative type variables *)


  (* 10th stage: interpret types according to information collected *)

  (* done by applying function interpret on all the types occurring in
     the constraints *)

  (* 11th stage: interpret coercions *)

  local   
  fun booldat (a,b) = cinterp a
  fun chardat (a,c) = cinterp a
  fun stridat (a,c) = cinterp a
  fun symbdat (a,s) = cinterp a
  fun vectdat (a,l) = cinterp a
  fun pairdat (a,d1,d2) = cinterp a
  fun nildat a = cinterp a
  fun noexp a = cinterp a
  fun literal (a,d) = cinterp a
  fun variable (a,v) = cinterp a
  fun call (a,e,l) = cinterp a
  fun lambda (a,f,e) = cinterp a
  fun ifexp (a,e,e',e'') = cinterp a
  fun assign (a,v,e) = cinterp a
  fun pairarg (a,e,l) = cinterp a
  fun nullarg a = cinterp a
  fun avarpar (a,p) = ()
  fun apairpar (a,p,f) = ()
  fun anullpar a = ()
  val dvar_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val evar_hom = EHOM { noexp = noexp,
                   literal = literal,
                   variable = variable,
                   call = call,
                   lambda = lambda,
                   ifexp = ifexp,
                   assign = assign } 
  val fvar_hom = FHOM { avarpar = avarpar,
                   apairpar = apairpar,
                   anullpar = anullpar }
  val avar_hom = LHOM { pairarg = pairarg,
                   nullarg = nullarg }
  in
  val interpret_coercions = apply_ehom (dvar_hom, evar_hom, avar_hom, fvar_hom)
  end


 (* 12th stage: pretty print and translate *)

 (* This is missing *)

fun process e =   
      let val (Cset, FVars, hi_type, PosTVars) = Cexp e
      in (foreach equiv Cset;
          foreach pred_succ Cset;
          propagate (hi_type :: PosTVars, FVars);
          foreach (fn (l,h) => (interpret l; interpret h)) Cset;  
          e)
      end

  datatype kexp =
    BOOLCONST of bool |
    CHARCONST of string |
    STRICONST of string |
    SYMBCONST of string |
    NUMBCONST of string |
    VECTCONST of kexp list |
    || of kexp * kexp |
    NIL |
    NOTHING |
    IDENT of string |
    FCALL of kexp * kexp |
    LAM of kexp * kexp |
    IFEXP of kexp * kexp * kexp |
    ASSIGNMENT of string * kexp |
    CAPP of coercion * kexp
  
  nonfix ||
  local fun f F (l,h) x = 
              let val c = coercion (l,h)
              in if c = ID 
                    then F x
                 else CAPP (c, F x)
              end
        fun f0 F (l,h) = 
              let val c = coercion (l,h)
              in if c = ID 
                    then F
                 else CAPP (c, F)
              end
         fun get_string (VAR (s,_)) = s
    in
    val Ddisplay =
         DHOM { booldat = f BOOLCONST,
                chardat = f CHARCONST,
                stridat = f STRICONST,
                symbdat = f SYMBCONST,
                numbdat = f NUMBCONST,
                vectdat = f VECTCONST,
                pairdat = f ||,
                nildat = f0 NIL
               }

    val Edisplay =
         EHOM { noexp = f0 NOTHING,
                literal = f (apply_dhom Ddisplay),
                variable = f (IDENT o get_string),
                call = f FCALL,
                lambda = f LAM,
                ifexp = f IFEXP,
                assign = f (fn (VAR(s,_),e) => ASSIGNMENT (s, e)),
                pairarg = f ||,
                nullarg = f0 NIL,
                avarpar = f (IDENT o get_string),
                apairpar = f (fn (VAR(s,_), e) => || (IDENT s, e)),
                anullpar = f0 NIL 
              }

    val display = apply_ehom Edisplay
    end 
                 
          
  end
end

