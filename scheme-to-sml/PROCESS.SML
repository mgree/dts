(* Do the whole process *)
local
open Constraint Type Environment General Exp
val ue = parse_exp std_in (* parse input stream into an unattributed expression *) 
val ae = make_attributes_exp ue topenv (* translate unattributed expression into att. exp. *)
val ke1 = display_exp ae
val t = unify_type_exp ae (* unify types according to local equational type rules *)
val C = get_constraint_exp ae (* collect all constraints *)
in
val attexp = ke1
val canannot = display_exp ae
val constraints = show_constraints [C]
val _ = induced_unify C (* unify types according to requirements of simple VFG *)
val svfg = display_exp ae
val _ = pred_succs C (* set predecessor and successor attributes in each type *)
val _ = make_acyclic C (* collapse all strong components in SVFG *)
val asvfg = display_exp ae
val _ = (reset_pred_succs C; pred_succs C); (* set preds/succs for asvfg *)
val _ = tvar_usage_exp (ae, t) (* mark types that may come 
    							  from/escape to context *)
val _ = type_int C (* interpret types according to nondirected 
        					  SVFG information *)
val final = display_exp ae
end

