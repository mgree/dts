(* Do the whole process for definitions *)
local
open Constraint Type PolyType Environment General Exp
val _ = pd := 5
val ud = parse_def std_in (* parse input stream into a preattributed definition *) 
val ad = make_attributes_def ud topenv 
	     (* translate preattributed expression into attributed definition *)
val ae = case ad of DEFINE (_, ae) => ae
         (* extract expression part from definition *)	     
val kd1 = display_def ad
val t = unify_type_def ad (* unify types according to local equational type rules *)
val C = get_constraint_def ad (* collect all constraints *)
in
val _ = pd := 40
val att_def = kd1
val can_annot = display_def ad
val constraints = show_constraints C
val _ = induced_unify C (* unify types according to requirements of simple VFG *)
val svfg = display_def ad
val _ = pred_succs C (* set predecessors and successors in svfg *)
val _ = make_acyclic C (* collapse all strong components in SVFG *)
val asvfg = display_def ad
val _ = (reset_pred_succs C; pred_succs C) (* set preds/succs of asvfg *)
local
val N = get_types C (* get types in C *)
in
val _ = set_sources N (* set source types for all types *)
val _ = set_sinks N (* set sink types for all types *)
val _ = set_types N (* set types according to sources and sinks *)
end
val final = display_def ad
val constraints_final = show_constraints C
local
val coercion_params = coercion_parameters (aflatten [C])
in
val coercion_pars = map show_constraint coercion_params
val poly_type = show_polytype (close (coercion_params, t))
end
end







