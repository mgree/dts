  infix ::= 

  fun equiv (t1, t2): unit =
      let val t1' = ecr t1
          val t2' = ecr t2
      in if equal(t1', t2')
            then ()
         else case (utype t1', utype t2') of
                (DYN f, DYN f') => (union (t1', t2'); 
                                    apply aliassimple (zip (summands f, summands f')))
              | (DYN f, SIMPLE (tt, _)) => 
              				(get #equivptr t2' := Some t1';
              				 aliassimple (f tt, t2))
              | (DYN _, TVAR _) => get #equivptr t2' := Some t1'
              | (SIMPLE (tt, _), DYN f) => 
              				(get #equivptr t1' := Some t2';
              				 aliassimple (t1, f tt))
              | (SIMPLE (tt, _), SIMPLE (tt', _)) => 
                   if tt = tt' then
                      aliassimple (t1, t2)
                   else let val rd = make_dyn_type [t1', t2']
                        in (get #equivptr t1' := Some rd;
                            get #equivptr t2' := Some rd)
                        end
              | (SIMPLE _, TVAR _) => get #equivptr t2' := Some t1'
              | (TVAR _, DYN _) => get #equivptr t1' := Some t2'
              | (TVAR _, SIMPLE _) => get #equivptr t1' := Some t2'
              | (TVAR _, TVAR _) => get #equivptr t1' := Some t2'
      end
  and aliassimple (t1, t2): unit =
      if equal (t1, t2) 
         then ()
      else case (utype t1, utype t2) of
             (SIMPLE (_, tlist), SIMPLE (_, tlist')) =>
				(union (t1, t2);
                 apply aliasvar (zip (tlist, tlist')))
           | (TVAR _, SIMPLE _) => link (t1, t2)
           | (SIMPLE _, TVAR _) => link (t2, t1)
           | (TVAR _, TVAR _) => union (t1, t2)
           | (_,_) => error ("aliassimple", "Illegal type aliasing attempted")
  and aliasvar (t1, t2): unit =
  	  if equal (t1, t2)
  	  	 then ()
  	  else case (utype t1, utype t2) of
  	  	     (TVAR _, TVAR _) => (union (t1, t2);
  	  	     					  equiv (t1, t2))
  	  	   | (_, _) => error ("aliasvar", "Arguments must be type variables!")
  	  	   
  fun unify (t1, t2) =
      if equal (t1, t2) 
         then ()
      else case (utype t1, utype t2) of
              (DYN f, DYN f') => (union (t1, t2); 
                                  apply unify (zip (summands f, summands f')))
            | (SIMPLE (tt, tlist), SIMPLE (tt', tlist')) =>
                 if tt = tt' then
                    (union (t1, t2); apply unify (zip (tlist, tlist')))
                 else error ("Type constructor clash in unify", "")
            | (TVAR _, TVAR _) => union (t1, t2)
            | (TVAR _, _) => link (t1, t2)
            | (_, TVAR _) => link (t2, t1)
            | (_, _) => error ("Dyn and simple types cannot be unified", "")

  fun pred_succ (l,h) =
      if equal (l, h) 
         then ()
      else let val pred_h = get #preds h
               val succ_l = get #succs l
           in (pred_h := l :: (!pred_h);
               succ_l := h :: (!succ_l))
           end

