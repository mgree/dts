structure KernelAttributes =
  struct

  (* 2nd stage: annotate all expressions with fresh coercions and
     resolve variable references *)


  local   
  open SchemeDatum KernelExp SchemeTypes SchemeCoercions SchemeVariables 
       Environment
  in

  type parameter = (string * atype, string * atype, string * atype) var
  type variable = parameter * atype list ref
  type aexp = (coercion datum, parameter, variable, coercion, coercion, unit) exp
  type aexpf = parameter env -> aexp
  type aargs = (coercion datum, parameter, variable, coercion, coercion, unit) args
  type aargsf = parameter env -> aargs
  type aforms = (parameter, unit) formals
  type ftrip = aforms * parameter env * coercion

  exception StaticAnalysisError of string
  
  local
  fun booldat (a,b) = BOOLDAT (new_coercion(), b)
  fun chardat (a,c) = CHARDAT (new_coercion(), c)
  fun stridat (a,c) = STRIDAT (new_coercion(), c)
  fun symbdat (a,s) = SYMBDAT (new_coercion(), s)
  fun numbdat (a,s) = NUMBDAT (new_coercion(), s)
  fun vectdat (a,l) = VECTDAT (new_coercion(), l)
  fun pairdat (a,d1,d2) = PAIRDAT (new_coercion(), d1,d2)
  fun nildat a = NILDAT (new_coercion())
  val dvar_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val make_datt = apply_dhom dvar_hom

  fun noexp _ (Env: parameter env): aexp = NOEXP (new_coercion())
  fun literal (_, d: 'a datum) (Env:parameter env): aexp = LITERAL (new_coercion(), make_datt d)
  fun variable (_,s: string) (Env: parameter env): aexp = 
        VARIABLE (new_coercion(), 
                  (lookup s Env handle Lookup => FREE (s, new_typevar()),
                   ref []))
  fun call (_, e: aexpf ,l: aargsf) (Env: parameter env): aexp = CALL (new_coercion(), e Env, l Env)
  fun lambda (_,f: ftrip,e: aexpf) (Env: parameter env): aexp = 
        let val (st: aforms,fenv: parameter env, c: coercion) = f 
	    val nt = new_typevar()
	    val nt' = new_typevar()
        in LAMBDA ((COMP ((MAP (FUNC, [c,(IDC,nt,nt)]), func(hi_type c,nt), func(lo_type c,nt)), 
		         make_coercion(func(lo_type c,nt),nt')), func(hi_type c, nt), nt'), 
	           st, e (add (Env, fenv)))
        end
  fun ifexp (_,e:aexpf,e':aexpf,e'':aexpf) (Env: parameter env): aexp = 
	IF (new_coercion(), e Env, e' Env, e'' Env)
  fun assign (_,s:string,e:aexpf) (Env: parameter env): aexp = 
	ASSIGN (new_coercion(), 
		(lookup s Env handle Lookup => FREE (s, new_typevar()), ref []), 
 	        e Env)
  fun pairarg (_,e:aexpf,l:aargsf) (Env: parameter env): aargs = 
	PAIRARG (new_coercion(), e Env, l Env)
  fun nullarg _ (Env: parameter env): aargs = NULLARG (new_coercion())
  fun avarpar (_,s): ftrip = 
      let val nt = new_typevar()
	  val new_par = LAMBOUND (s, nt)
      in
      (AVARPAR ((),new_par), make_env (s, new_par), (IDC, nt, nt))
      end
  fun apairpar (_,s:string,f:ftrip): ftrip = 
      let val nt = new_typevar()
          val nt' = new_typevar()
	  val new_par = LAMBOUND (s, nt)
	  val (st, env, c) = f
      in 
      (lookup s env; raise StaticAnalysisError "Duplicate parameter")
       handle Lookup => 
          (APAIRPAR ((),new_par,st), 
           extend (env, s, new_par),
	   (COMP (make_coercion(nt', pair(nt,lo_type c)), 
	              (MAP (PAIR, [(IDC,nt,nt), c]), pair(nt,lo_type c), pair(nt, hi_type c))),
	    nt', pair(nt, hi_type c))) 
      end 
  fun anullpar _: ftrip = 
      let val nt = new_typevar()
      in
      (ANULLPAR (), empty_env, make_coercion(nt, unit()))
      end
  val evar_hom = EHOM { noexp = noexp,
                   literal = literal,
                   variable = variable,
                   call = call,
                   lambda = lambda,
                   ifexp = ifexp,
                   assign = assign,
	           avarpar = avarpar,
                   apairpar = apairpar,
                   anullpar = anullpar,
		   pairarg = pairarg,
                   nullarg = nullarg }
  in
  val make_attributes = apply_ehom evar_hom
  end

  val get_toptype = hi_type o eattrib 

  end
end
