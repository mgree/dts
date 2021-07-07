(* Do the whole process *)
local
open Constraint Type Environment General Exp
val ue = parse std_in (* parse input stream into an unattributed expression *) 
val ae = make_attributes ue empty_env (* translate unattributed expression into att. exp. *)
val ke1 = display ae
val t = unify_types ae (* unify types according to local equational type rules *)
val C = get_constraints ae (* collect all constraints *)
in
val attexp = ke1
val canannot = display ae
val constraints = show_constraints [C]
val _ = induced_unify C (* unify types according to requirements of simple VFG *)
val svfg = display ae
val _ = pred_succs C (* set predecessor and successor attributes in each type *)
val _ = make_acyclic C (* collapse all strong components in SVFG *)
val asvfg = display ae
val _ = tvar_usage (ae, t) (* mark types that may come 
    							  from/escapte to context *)
val _ = type_int C (* interpret types according to nondirected 
        					  SVFG information *)
val final = display ae
end

