structure KernelConstraints =
  struct

 (* 5th stage: collect constraints *)

  local  
  open SchemeGeneral SchemeDatum KernelExp SchemeTypes SchemeCoercions 
  in

  datatype 'a applist =   LIST of 'a list 
                        | APPEND of 'a applist list

  fun leaf a = LIST [a]
  val anil = LIST []

  fun foreach f (LIST l) = apply f l
    | foreach f (APPEND l) = apply (foreach f) l

  fun constraints (CVAR _, t1, t2) = leaf (t1,t2)
    | constraints (IDC, _, _) = anil
    | constraints (TAG tt, _, _) = anil
    | constraints (CHECK tt, _, _) = anil
    | constraints (COMP (c,c'), _, _) = APPEND [constraints c, constraints c']
    | constraints (MAP (tt, cl), _, _) = APPEND (map constraints cl) 

  local
  fun booldat (a,b) = constraints a
  fun chardat (a,c) = constraints a
  fun stridat (a,c) = constraints a
  fun symbdat (a,s) = constraints a
  fun numbdat (a,s) = constraints a
  fun vectdat (a,l) = APPEND (constraints a :: l)
  fun pairdat (a,d1,d2) = APPEND [constraints a, d1, d2]
  fun nildat a = constraints a
  val dconstraints_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val dconstraints = apply_dhom dconstraints_hom

  fun noexp a = constraints a
  fun literal (a,d) = APPEND [constraints a, dconstraints d]
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

  (* 6th stage: do induced unifications *)

  val induced_unifications = foreach equiv 

  (* 7th stage: set predecessors and successors *)

  val preds_succs = foreach pred_succ

  end
end
