  (* 2nd stage: annotate all expressions with fresh coercions and
     resolve variable references *)

  exception StaticAnalysisError of string
  
  local   
  open General Exp Environment Type PolyType
  
  fun noexp a (Env: typescheme env) = NOEXP a
  fun literal d (Env:typescheme env) = LITERAL d
  fun variable (a, (s: string, ())) (Env: typescheme env) = 
  	  let val (C, t) = instantiate (lookup s Env)
	      val ta = case C of
		   		 	nil => TYPE t
		 	       | _ => TSCHEME (C, t)
	  in VARIABLE (a, (s, ta))
	  end
  fun call (a, e, l) (Env: typescheme env) = CALL (a, e Env, l Env)
  fun lambda (a, (st, fenv), e) (Env: typescheme env) = 
        LAMBDA (a, st, e (add (Env, fenv)))
  fun ifexp (a, e , e', e'') (Env: typescheme env) = 
        IF (a, e Env, e' Env, e'' Env)
  fun assign (a,(s:string, ()), e) (Env: typescheme env) = 
  	  case lookup s Env of
  	  	t as (TYPE _) => ASSIGN (a, (s, t), e Env)
  	  | TSCHEME _ => error ("make_attributes", 
                  	"Polymorphic values cannot be assigned to")
  fun pairarg (a,e,l) (Env: typescheme env) = 
        PAIRARG (a, e Env, l Env)
  fun nullarg a (Env: typescheme env) = NULLARG a
  fun avarpar (s, ()) = 
      let val nt = new_typevar()
      in
      (AVARPAR (s, nt), make_env (s, TYPE nt))
      end
  fun apairpar ((s:string, ()), (st, env)) = 
      let val nt = new_typevar()
      in 
      if iskey s env
      	 then raise StaticAnalysisError "Duplicate parameter"
      else (APAIRPAR ((s, nt), st), extend (env, s, TYPE nt))
      end 
  fun anullpar () = (ANULLPAR, empty_env)
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
