(* Do the whole process *)
local
open Constraint Type Environment General Exp
val ue = parse_exp std_in (* parse input stream into an unattributed expression *) 
val ae = make_attributes_exp ue topenv (* translate unattributed expression into att. exp. *)
val ke1 = display ae
val t = unify_type_exp ae (* unify types according to local equational type rules *)
val C = get_constraint_exp ae (* collect all constraints *)
in
val attexp = ke1
val canannot = display ae
val constraints = show_constraints [C]
val _ = induced_unify C (* unify types according to requirements of simple VFG *)
val svfg = display ae
val _ = pred_succs C (* set predecessors and successors in svfg *)
val _ = make_acyclic C (* collapse all strong components in SVFG *)
val asvfg = display ae
val _ = (reset_pred_succs C; pred_succs C) (* set preds/succs of asvfg *)
val N = get_types C (* get types in C *)
val _ = set_sources N (* set source types for all types *)
val _ = set_sinks N (* set sink types for all types *)
val _ = set_types N (* set types according to sources and sinks *)
val final = display ae
end






