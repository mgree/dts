structure Constraint =
  struct

  local  
  open General Type UnionFind
  in
  
  fun decompose b t =
        (* decomposes a type into a type variable and a list of flow constraints *)
        case utype t of
                TVAR _ => (t, anil)
          | SIMPLE (FUNC, [td, tr]) =>
                  let val (ntd, Cd) = decompose (not b) td 
                          val (ntr, Cr) = decompose b tr
                          val nstype = make_type (FUNC, [ntd, ntr])
                          val ntvar = new_typevar()
                          val topconstraint =
                                if b then (nstype, ntvar) else (ntvar, nstype)
                  in (ntvar, APPEND [Cd, Cr, leaf topconstraint])
                  end
          | SIMPLE (ttag, atl) =>
                  let val tCs = map (decompose b) atl
                      val nstype = make_type (ttag, map #1 tCs)
                      val ntvar = new_typevar()
                      val topconstraint =
                        if b then (nstype, ntvar) else (ntvar, nstype)
                  in (ntvar, APPEND (leaf topconstraint :: map #2 tCs))
                  end
          | DYN _ => raise IllegalInput 
                        ("Error in decompose", "Argument cannot be a dynamic type")
        
  fun constraints (t1, t2) =
        (* generates flow constraints/primitive coercions from arbitrary 
        coercion signature *)
        let val (tv1, C1) = decompose true t1
            val (tv2, C2) = decompose false t2
        in (union (tv1, tv2);
            APPEND [C1, C2])
        end

  fun show_constraint (t1, t2) =
          (show_type t1, show_type t2)
           
  fun show_constraints C =
      map show_constraint (aflatten C)
          
  end
end
