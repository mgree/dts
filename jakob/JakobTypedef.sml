(* Process one definition completely *)

local
open Constraint Type PolyType Environment SchemeGeneral Exp
in

val topty = ref (None : atype Option)


fun type_def ud env =
let
(* open Constraint Type PolyType Environment SchemeGeneral Exp *)

val ad = make_attributes_def ud env 
		(* translate preattributed definition 
                   into attributed definition *)
val t = unify_type_def ad (* unify types according to local 
                             equational type rules *)
val _ = topty := Some t

val C = get_constraint_def ad (* collect all constraints *)
val _ = induced_unify C (* unify types according to requirements 
                           of simple VFG *)
val _ = pred_succs C (* set predecessors and successors in svfg *)
val _ = make_acyclic C (* collapse all strong components in SVFG *)
val _ = (reset_pred_succs C; pred_succs C) (* set preds/succs of asvfg *)
val N = get_types C (* get all nodes/types occurring in C *)
val _ = set_sources N (* set source types for all types *)
val _ = set_sinks N (* set sink types for all types *)
val _ = set_types N (* set types according to sources and sinks *)
val _ = mark_rec_types N
(*
val _ = find_list_types N
*)
in 
    ad

   (*
   case ud of
     DEFINE ((s,_), _) =>
       let val coercion_list = aflatten [C]
           val coercion_params = coercion_parameters coercion_list
       in (s, show_polytype (close (coercion_params, t)))
       end       
   *)
end


end (* local open *)
