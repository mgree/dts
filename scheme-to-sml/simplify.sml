(* (V,E,t) are simplified to (V',E',t').

Assume: all coercions in E are of the form (t,t') where t, t' are either a type 
variable a or a shallow type f(a1,...,an), with f a type constructor and a1,...,an 
type variables.

Stages:

1. Unify all type variable-to-type variable coercions
2. Unify all types with equal type constructor that are related by the 
	symmetric, transitive closure of the coercion relation.
3. Eliminate duplicate type variables, non-type variable types from V, and
	duplicate coercions in E.

*)

local open General UnionFind Type
in


		(* STAGE 1: UNIFY TVAR-TO-TVAR CONSTRAINTS *)


(* contract E = E' where
   E = list of constraints (t1,t2) such t1,t2 are shallow tnodes
   E' = sublist of E with all those constraints (t1,t2) removed and unioned where 
	both t1 and t2 are tvars *)

fun contract nil = nil
  | contract (c::r) =
      let val dom_type_c = dom_type c
          val rng_type_c = rng_type c
      in if type_tag dom_type_c = TVAR andalso type_tag rng_type_c = TVAR
	   then (union (dom_type_c, rng_type_c); link (c, idc(dom_type_c)); contract r)
        else c :: contract r
      end

		(* STAGE 2: UNIFY FLOW-RELATED TNODE WITH EQUAL TYPE CONSTRUCTORS *)

(* lookup {dom_type, rng_type} tc l = {None, Some c}
   tc: type tag, 
   l: list of coercions,
   c: first coercion in l whose {dom_type, rng_type} has type tag equal to tc *)

fun lookup f tc nil = None
  | lookup f tc (c::r) =
      if tc = type_tag (f c) then Some c else lookup f tc r

(* set_preds_succs c = ():
   c = (t1, t2): shallow tnodes such that at least one is a non-tvar
   effect: unify t1, t2 if both non-tvars, else add to reachable of t1 if t2 non-tvar;
	   if reachable of t1 already contains reachable tnode tn with same type tag then
	   unify tn and t2 *)

fun set_preds_succs c =
    let val t1 = dom_type c
        val t2 = rng_type c
        val tc1 = type_tag t1
        val tc2 = type_tag t2
    in
	case (tc1, tc2) of 
	  (TVAR, TVAR) => error ("set_preds_succs", "Illegal tvar-tvar coercion")
        | (_, TVAR) => let val succs2 = succs t2
		           val preds2 = preds t2
		           val s2 = lookup rng_type tc1 (!succs2)
			   val p2 = lookup dom_type tc1 (!preds2)                      
		       in ((case s2 of
			      None => ()
			    | Some(c') => unifyS (t1, rng_type c'));
                           (case p2 of
			      None => (preds2 := c :: (!preds2))
			      Some(c') => let val t1' = dom_type c'
		 		         in (union (c,c'); unifyS(t1,t1'))
		                         end))
                       end
        | (TVAR, _) => let val succs1 = succs t1
		           val preds1 = preds t1
		           val s1 = lookup rng_type tc2 (!succs1)
			   val p1 = lookup dom_type tc1 (!preds2)                      
		       in ((case p1 of
			      None => ()
			    | Some(c') => unifyS (t2, dom_type c'));
                           (case s1 of
			      None => (succs1 := c :: (!succs1))
			      Some(c') => let val t2' = rng_type c'
		 		         in (union (c,c'); unifyS(t2,t2'))
		                         end))
                       end

        | (_, _) => if tc1=tc2 
	            then (link(c,idc(t1)); unifyS(t1,t2))
                    else link(c,error_coercion)
    end

(* unifyS (t1, t2) = ():
   t1, t2: non-tvar shallow tnodes with equal type constructor
   effect: t1 and t2 are unified and all induced unifications are executed *)

and unifyS (t1, t2) =
    let val children1 = children t1
        val children2 = children t2
    in (union (t1,t2); unifyL (children1, children2))
    end

(* unifyL (ts1, ts2) = ():
   ts1, ts2: lists of tvars
   effect: ts1 and ts2 are componentwise unified and all induced unifications are executed *)

and unifyL (nil, nil) = ()
  | unifyL (t1::ts1, t2::ts2) = (unifyV (t1,t2); unifyL (ts1, ts2))
  | unifyL _ = error ("unifyL", "Unequal number of children")

(* unifyV (t1, t2) = ():
   t1, t2: tvars
   effect: t1 are t2 are unified and all induced unifications are executed *)

and unifyV (t1, t2) =
    let val succs1 = succs t1
	val preds1 = preds t1
	val succs2 = succs t2
	val preds2 = preds t2
    in (link (t1,t2); 
	succs2 := merge rng_type (!succs1, !succs2)
        preds2 := merge dom_type (!preds1, !preds2))
    end

(* merge {dom_type,rng_type} (l1, l2) = l3:
   l1, l2: lists of coercions such that no type tag occurs twice in l1, resp. l2
   l3: result of merging l1 and l2 by appending l1 and l2, though such that if
	c in m1 and c' in m2 then only one pair is included and tn1 and tn2
	are unified *)

and merge f (nil, l2) = l2
  | merge f (c::r, l2) =
	let val c_type = f c
            val tc = type_tag c_type
	    val c' = lookup f tc l2
        in case c' of 
	     None => c :: merge f (r, l2)
           | Some c' => let val c'_type = f c'
			in (union (c,c'); unifyS(c_type,c'_type); merge f (r,l2))
			end
        end

(* set_preds_succsL E = () where
   E = list of shallow constraints (t1,t2) such that at least one of t1, t2 is a non-tvar
   effect: apply set_preds_succs to all elements of E *)

val set_preds_succsL = app set_preds_succs


		(* STAGE 3: NORMALIZE VARIABLE AND CONSTRAINT LISTS *)

(* normalizeV V = V':
   V: list of shallow tnodes
   V': sublist of V with all duplicate tvars and non-tvar tnodes eliminated *)

local
fun normV nil = nil
  | normV (tv :: tr) = 
	if type_tag tv = TVAR andalso not (!(pos tv))
	   then (pos tv := true; tv :: normV tr)
        else normV tr
in
fun normalizeV V = (app init_tnode V; normV V)
end


(* normalizeE E = E'
   E: list of constraints (t1,t2) such that t1,t2 are shallow tnodes exactly one of which
	is non-tvar and such that whenever (t1, t2) and ((t1',t2) or (t2, t1')) in E
	and t1, t1' have same type tag then t1 and t1' are equal
   E': sublist of E with all duplicate constraints removed *)

local 
fun normE nil = nil
  | normE (c::r) =
	if !(seen c)
	then normE r
	else (seen c := true;
	      case !!c of
		COERCE(_) => c :: normE r
	      | _ => normE r
in
fun normalizeE E =
    (app init_coercion E; normE E)
end


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

fun set_pos tn =
    if !(pos tn) then ()
    else (pos tn := true;
	  set_posCL (!(preds tn)))
and set_posCL nil = ()
  | set_posCL (c::r) = (set_posC c; set_posCL r)
and set_posC c =
      let val t = dom_type c
      in if !(pos t) then ()
	 else (pos t := true;
	 case type_tag t of
	   TVAR => error ("set_posC", "Illegal predecessor (nonshallow type)")
	 | FUNC => let val [t1,t2] = children t
	           in (set_neg t1; set_pos t2)
		   end
         | _ => app set_pos (children t))
      end
and set_neg tn =
    if !(neg tn) then ()
    else (neg tn := true;
	  set_negCL (!(succs tn)))
and set_negCL nil = ()
  | set_negCL (c::r) = (set_negC c; set_negCL r)
and set_negC c =
      let val t = rng_type c
      in if !(neg t) then ()
	 else (neg t := true;
	       case type_tag t of
		 TVAR => error ("set_negC", "Illegal successor (nonshallow type)")
	       | FUNC => let val [t1,t2] = children t
	                 in (set_pos t1; set_neg t2)
	                 end
	       | _ => app set_neg (children t))
      end

(* elim_nonneg tn = ():
   tn: tvar, with pos/neg fields determined
   effect: eliminate tvars that are _not_ negative *)

fun elim_nonneg tn =
         case !(preds tn) of
	   nil => (link(tn, undef_type);
		   app (fn c => link(c, nonexec_coercion)) (!(succs tn)))
	 | [c] => if !(neg tn) then ()
	          else let val t = dom_type c
		       in (link(c, idc(t)); link(tn, t); set_coercions (!succs tn))
		       end
         | cs => (link(tn, dynamic);
		  app (fn c => link(c, tag (type_tag (dom_type c)))) (!(preds tn));
		  app (fn c => link(c, untag (type_tag (rng_type c)))) (!(succs tn)))
and set_coercions nil = ()
  | set_coercions (c::r) = 
	let val dom_type_c = dom_type c
	    val rng_type_c = rng_type c
	in (if type_tag dom_type_c = type_tag rng_type c
	      then link (c, id(dom_type_c))
	    else link (c, error_coercion);
	    set_coercions r)
        end

fun simplify (V,E) =
  let 
    val E' = contract E (* stage 1 *)
    val _ = set_preds_succsL E' (* stage 2 *)
    val V' = normalizeV V (* stage 3a *)
    val E'' = normalizeE E' (* stage 3b *)
  in (V',E'')
  end

end




