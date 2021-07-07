(* 
Eliminate invisible type variables and coercion variables in three stages:

1. Set preds and succs fields in each type variable
2. Set pos and neg fields in each type variable
3. Interpret nonneg type variables:
   a) If no preds: bottom
   b) If exactly one pred: same type as pred, connection coercion is identity
   c) If more than one pred: sum type, interpret pred coercions as tag operations and 
	succ coercions as untag operations

*)

local
in

(* set_preds_succs c = ():
   c: coercion (t1,t2), where t1,t2 shallow
   effect: sets the preds and succs fields of each type variable occurring in (t1,t2) *)

set_preds_succs c =
   let val dom_type_c = dom_type c
       val rng_type_c = rng_type c
   in case (type_tag dom_type_c, type_tag rng_type_c) of
        (TVAR, TVAR) => error ("set_preds_succs", "Illegal tvar-tvar coercion")
      | (TVAR, _) => let val succs_dom_type_c = succs dom_type_c
                     in succs_dom_type_c := c :: (!succs_dom_type_c)
		     end
      | (_, TVAR) => let val preds_rng_type_c = preds rng_type_c
		     in preds_rng_type_c := c :: (!preds_rng_type_c)
      | (_,_) => error ("set_preds_succs", "Illegal nontvar-nontvar coercion")
   end
