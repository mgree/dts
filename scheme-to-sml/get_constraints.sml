(* Collect constraints from source program *)
   
  local
  open General Datum Exp Constraint PolyType
  fun booldat (a,b) = leaf a
  fun chardat (a,c) = leaf a
  fun stridat (a,c) = leaf a
  fun symbdat (a,s) = leaf a
  fun numbdat (a,s) = leaf a
  fun vectdat (a,l) = APPEND (leaf a :: l)
  fun pairdat (a,d1,d2) = APPEND [leaf a, d1, d2]
  fun nildat a = leaf a
  val dconstraints_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val dconstraints = apply_dhom dconstraints_hom

  fun noexp a = leaf a
  val literal = dconstraints
  fun variable (a, (s, TYPE _)) = leaf a
    | variable (a, (s, TSCHEME (cl, _))) = APPEND [constraints a, LIST cl]
  fun call (a,e,l) = APPEND [leaf a, e, l]
  fun lambda (a,_,e) = APPEND [constraints a, e]
  fun ifexp (a,e,e',e'') = APPEND [leaf a, e, e', e'']
  fun assign (a,v,e) = APPEND [leaf a, e]
  fun pairarg (a,e,l) = APPEND [leaf a, e, l]
  fun nullarg a = leaf a
  fun avarpar _ = ()
  fun apairpar _ = ()
  fun anullpar _ = ()
  val constraints_hom = EHOM { noexp = noexp,
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
  val get_constraints = apply_ehom constraints_hom
  end
