(* Perform unifications induced by closure condition of
   simple value flow graphs *)

  local
  open General Type UnionFind
  fun equiv (t1, t2): unit =
      let val t1' = ecr t1
          val t2' = ecr t2
      in if equal(t1', t2')
            then ()
         else case (utype t1', utype t2') of
                (DYN f, DYN f') => (elink (t1', t2'); 
                                    apply equiv (zip (summands f, summands f')))
              | (DYN f, SIMPLE (tt, _)) => 
              				(elink (t2', t1');
              				 equiv (f tt, t2'))
              | (DYN _, TVAR _) => elink (t2', t1')
              | (SIMPLE (tt, _), DYN f) => 
              				(elink (t1', t2');
              				 equiv (t1', f tt))
              | (SIMPLE (tt, tlist), SIMPLE (tt', tlist')) => 
                   if tt = tt' then
		              (elink (t1', t2');
                       apply aliasvar (zip (tlist, tlist')))
                   else let val rd = make_dyn_type [t1', t2']
                        in (elink (t1', rd);
                            elink (t2', rd))
                        end
              | (SIMPLE _, TVAR _) => elink (t2', t1')
              | (TVAR _, DYN _) => elink (t1', t2')
              | (TVAR _, SIMPLE _) => elink (t1', t2')
              | (TVAR _, TVAR _) => elink (t1', t2')
      end
  and aliasvar (t1, t2): unit =
  	  if equal (t1, t2)
  	  	 then ()
  	  else case (utype t1, utype t2) of
  	  	     (TVAR _, TVAR _) => let val t1' = ecr t1
  	  	     						 val t2' = ecr t2
  	  	     				     in (union (t1, t2); equiv (t1', t2'))
  	  	     				     end
  	  	    	(* NOT equiv (t1, t2); union (t1, t2) or
  	  	    		   union (t1, t2); equiv (t1, t2) !!! *)
  	  	   | (_, _) => error ("aliasvar", "Arguments must be type variables!")
  in  	  	   
  val induced_unify = foreach equiv
  end
  