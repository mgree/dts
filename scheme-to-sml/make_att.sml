(* Define canonical completion:

   A: tscheme env
   e: (unit, unit, unit) exp
   e': (tnode * tnode, tnode, tgraph) exp
   (V, E): tgraph = tnode list * (tnode * tnode) list
   r: tnode
   
   complete e A = (e', V, E, r) => A |- e => e': forall V: E. r
   
*)
 
local open Datum Exp Type Env General UnionFind
in

  fun att_datum (BOOLDAT b, _) = 
	let val t = tvar()
            val c = mk_coercion (bool(), t)
        in ((BOOLDAT b, c), [t], [c], t)
        end
    | att_datum (CHARDAT s, _) =
	let val t = tvar()
	    val c = mk_coercion (char(), t)
	in ((CHARDAT s, c), [t], [c], t)
	end
    | att_datum (STRIDAT s, _) =
	let val t = tvar()
	    val c = mk_coercion (string(), t)
	in ((STRIDAT s, c), [t], [c], t)
	end
    | att_datum (SYMBDAT s, _) =
	let val t = tvar()
	    val c = mk_coercion (symbol(), t)
	in ((SYMBDAT s, c), [t], [c], t)
	end
    | att_datum (NUMBDAT s, _) =
	let val t = tvar()
	    val c = mk_coercion (number(), t)
	in ((NUMBDAT s, c), [t], [c], t)
	end
    | att_datum (VECTDAT l, _) =
	let val t = tvar()
            val (es,Vs,Es,ts) = 
		List.foldr (fn ((e,V,E,t), (es,Vs,Es,ts)) => (e::es, V@Vs, E@Es, t::ts))
		     (map att_datum l)
		     (nil,nil,nil,nil)
            val c = case ts of 
		      nil => mk_coercion (vector (tvar()), t)
		    | t'::tr => (app (fn t'' => union (t',t'')) tr;
				 mk_coercion (vector t', t))
        in ((VECTDAT es, c), [t] @ Vs, [c] @ Es, t)
        end
    | att_datum (PAIRDAT (d1,d2), _) =
	let val t = tvar()
	    val (d1',V1,E1,t1) = att_datum d1
	    val (d2',V2,E2,t2) = att_datum d2
            val c = mk_coercion (pair(t1,t2), t)
        in ((PAIRDAT (d1',d2'), c), [t]@V1@V2, [c]@E1@E2, t)
	end
    | att_datum (NILDAT, _) =
	let val t = tvar()
	    val c = mk_coercion (unit(), t)
        in ((NILDAT, c), [t], [c], t)
        end

  fun att_exp (NOEXP, _) A = 
  	    let val t = tvar () 
  	    	val c = mk_coercion (unspec(), t)
  	    in ((NOEXP, c), [t], [c], t) 
  	    end
    | att_exp ((LITERAL d), _) A =
  		let val (d', V, E, t) = att_datum d 
  		in ((LITERAL d', idc(t)), V, E, t)
  		end
    | att_exp (VARIABLE (s,_), _) A =
  		let val (V, E, t) = instantiate (lookup s A)
  		    val t' = tvar()
  		    val c = mk_coercion (t,t')
  		in ((VARIABLE (s, (V,E)), c), [t'] @ V, [c] @ E, t')
  		end
    | att_exp (CALL (e, a), _) A =
    	let val (e', V', E', t') = att_exp e A
    	    val (a', V'', E'', t'') = att_args a A
    	    val t = tvar()
    	    val t''' = tvar()
    	    val c = mk_coercion (t,t''')
    	in (link(t', func(t'',t)); 
    		((CALL (e', a'), c), [t, t'''] @ V' @ V'', [c] @ E' @ E'', t'''))
    	end
    | att_exp (LAMBDA (f, e), _) A =
    	let val (f', A_f, Vf, Ef, tf) = att_formals f
    	    val (e', V', E', t') = att_exp e (add (A, A_f))
    	    val t = tvar()
    	    val c = mk_coercion (func(tf,t'), t)
    	in ((LAMBDA (f', e'), c), [t] @ Vf @ V', [c] @ Ef @ E', t')
    	end
    | att_exp (IF (e1, e2, e3), _) A =
    	let val (e1', V1, E1, t1) = att_exp e1 A
    	    val (e2', V2, E2, t2) = att_exp e2 A
    	    val (e3', V3, E3, t3) = att_exp e3 A
    	    val t = tvar()
    	    val c = mk_coercion (t2,t)
    	in (link(t1, bool());
    	    union(t2, t3);
    	    ((IF (e1', e2', e3'), c), [t] @ V1 @ V2 @ V3, [c] @ E1 @ E2 @ E3, t))
    	end
    | att_exp (ASSIGN ((s,_), e), _) A =
    	let val (e', V', E', t') = att_exp e A
    	    val t'' = tvar()
    	    val c = mk_coercion (unspec(), t'')
    	in case lookup s A of
    	     SIMPLE t => 
    	     	(link(t',t); 
    	     	 ((ASSIGN ((s, (nil,nil)), e'), c), [t''] @ V', [c] @ E', t''))
    	   | POLY _ => error ("att_exp", "Polymorphic values not assignable")
    	end

  and att_args (PAIRARG(e,a), _) A =
  	    let val (e', V', E', t') = att_exp e A
  	        val (a', V'', E'', t'') = att_args a A
  	    	val t = tvar()
  	    	val c = mk_coercion (pair(t',t''), t)
  	    in ((PAIRARG (e',a'), c), [t] @ V' @ V'', [c] @ E' @ E'', t)
  	    end
    | att_args (NULLARG, _) A =
  	    let val t = tvar () 
  	        val c = mk_coercion (unit(), t)
  	    in ((NULLARG, c), [t], [c], t)
  	    end

  and att_formals (AVARPAR (s,_)) =
  		let val t = tvar ()
  		in (AVARPAR (s, t), make_env (s, SIMPLE t), [t], nil, t)
  		end
    | att_formals (APAIRPAR ((s,_),f)) =
  		let val (f', A_f, Vf, Ef, tf) = att_formals f
  		    val t = tvar()
  		    val ts = tvar()
  		    val c = mk_coercion (t, pair(ts,tf))
  		in (APAIRPAR ((s,ts), f'), extend(A_f,s,SIMPLE ts), [t,ts] @ Vf, [c] @ Ef, t)
  		end
    | att_formals ANULLPAR =
  		let val t = tvar()
  		    val c = mk_coercion (t, unit())
  		in (ANULLPAR, empty_env, [t], [c], t)
  		end

end
