structure KernelUnify =
  struct

 (* 4th stage: unify types according to equational type rules *)

  local   
  open SchemeDatum KernelExp SchemeTypes SchemeCoercions
  fun booldat (a,b) = hi_type a before link (lo_type a, bool())
  fun chardat (a,c) = hi_type a before link (lo_type a, char())
  fun stridat (a,c) = hi_type a before link (lo_type a, string())
  fun symbdat (a,s) = hi_type a before link (lo_type a, symbol())
  fun numbdat (a,s) = hi_type a before link (lo_type a, number())
  fun vectdat (a,l) = hi_type a before 
  	 let val nt = new_typevar()
         in apply (fn t => link (t, nt)) l; 
	    link (lo_type a, vector nt)
         end
  fun pairdat (a,d1,d2) = hi_type a before link (lo_type a, pair(d1,d2))
  fun nildat a = hi_type a before link (lo_type a, unit())
  val dunify_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val dunify = apply_dhom dunify_hom

  fun noexp a = hi_type a before link (lo_type a, unspec())
  fun literal (a,d) = hi_type a before union (lo_type a, dunify d)
  fun variable (a, (LAMBOUND (_, t), _)) = hi_type a before link (lo_type a, t)
    | variable (a, (FREE (_,t), _)) = hi_type a before link (lo_type a, t)
    | variable _ = raise Unimplemented "variable, in unify_types" 
  fun call (a,e,l) = hi_type a before link (e, func(l, lo_type a))
  fun lambda (a,f,e) = hi_type a 
  fun ifexp (a,e,e',e'') = hi_type a before (union (e', e'');
                                             union (lo_type a, e');
                                             link (e, bool()))
  fun assign (a,(LAMBOUND (_, t), _),e) = hi_type a before (link (lo_type a, unspec());
                                        link (e, t)) 
    | assign (a,(FREE (_, t), _),e) = hi_type a before (link (lo_type a, unspec());
                                        link (e, t)) 
    | assign _ = raise Unimplemented "assign, in unify"
  fun pairarg (a,e,l) = hi_type a before link (lo_type a, pair(e,l))
  fun nullarg a = hi_type a before link (lo_type a, unit())
  fun avarpar (a,p) = ()
  fun apairpar (a,p,f) = ()
  fun anullpar a = ()
  val unify_hom = EHOM { noexp = noexp,
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
  val unify_types = apply_ehom unify_hom
  end

  end

