(* Do the whole process *)

fun process ip =   
    let val ue = parse ip (* parse input stream into an unattributed expression *) 
        val ae = make_attributes ue empty_env (* translate unattributed expression into att. exp. *)
        val toptype = get_toptype ae (* get hi-type of ae *)
        val (Fvars, UUsedBvars) = free_pos_vars ae (* get free vars and positive bound variables *)
	fun get_type (LAMBOUND (s,t)) = t
          | get_type (FREE (s,t)) = t
          | get_type _ = raise Unimplemented "get_type"
        val Ftvars = map get_type (flatten Fvars)
        val UUsedBtvars = map get_type UUsedBvars
        val C = get_constraints ae (* collect all constraints *)
    in (unify_types ae; (* unify types according to local equational type rules *)
        induced_unifications C; (* unify types according to requirements of simple VFG *)
	preds_succs C; (* set predecessor and successor attributes in each type *)
        propagate (toptype::UUsedBtvars, Ftvars); (* propagate +/- labels through SVFG *)
        typeint C; (* interpret the types in C *)
        display ae) 
    end
