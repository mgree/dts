structure KernelConstraints (*: KERNELCONSTRAINTS *) =
  struct
  local open SchemeGeneral KernelTypes SchemeDatum KernelExp UnionFind
  in

  type atype = atype
  type ('a, 'b) annexp = ('a, 'b) annexp
  type 'a variable = 'a variable

  datatype 'a applist =   LIST of 'a list 
                        | APPEND of 'a applist list

  fun leaf a = LIST [a]

  fun foreach f (LIST l) = apply f l
    | foreach f (APPEND l) = apply (foreach f) l

  (* set of free variables = 'a variable aotree *) 
  datatype 'a aotree =   EMPTYAO
                       | SINGLE of 'a
                       | OR of 'a aotree * 'a aotree
                       | AND of 'a aotree * 'a aotree

  fun flatten T =
      let fun f l EMPTYAO = l
            | f l (SINGLE x) = x :: l
            | f l (OR (T1, T2)) = f (f l T1) T2
            | f l (AND (T1, T2)) = f (f l T1) T2
      in f [] T
      end

  (* constraint extraction over annotated datum *)
  val Cdatum = 
      let fun booldat (c as (l,h)) b = 
              let val boolty = make_type (BOOL, [])
              in
                  (unify (l, boolty);
                   (leaf(l,h), h))
              end 

          fun nildat (c as (l,h)) = 
              let val nilty = make_type (NIL, [])
              in
                  (unify (l, nilty);
                   (leaf c, h))
              end

          fun pairdat (c as (l,h)) ((C1, h1), (C2, h2)) = 
              let val consty = make_type (PAIR, [h1, h2])
              in
                  (unify (l, consty);
                   (APPEND [LIST [c], C1, C2], h))
              end 

          fun notimp _ _ = raise Unimplemented "Cdatum"

          val Cdatum_hom =  
                 DHOM {booldat = booldat, chardat = notimp, stridat = notimp, 
                       symbdat = notimp, numbdat = notimp, vectdat = notimp, 
                       pairdat = pairdat, nildat = nildat}
          in
              apply_dhom Cdatum_hom
          end     

  exception StaticSemanticsError of string

  fun split v EMPTYAO = (false, EMPTYAO)
    | split (VAR (s,t)) (T as (SINGLE (VAR (s',t')))) = 
        if s = s' 
           then (unify (t, t');
                 (true, EMPTYAO))
        else (false, T)
    | split v (OR (T1, T2)) =
        let val (b1, T1') = split v T1
            val (b2, T2') = split v T2
        in (b1 andalso b2, 
            if T1' = EMPTYAO andalso T2' = EMPTYAO
               then EMPTYAO
            else OR (T1', T2'))
        end
    | split v (AND (T1, T2)) =
        let val (b1, T1') = split v T1
            val (b2, T2') = split v T2
        in (b1 orelse b2,
            if T1' = EMPTYAO 
               then T2'
            else if T2' = EMPTYAO
                    then T1'
                 else AND (T1', T2'))
        end

  fun lookup s [] = new_typevar()
    | lookup s (VAR (s', t') :: r) =
        if s = s' then t' else lookup s r

  fun zipit [] [] = []
    | zipit (a::r) (a'::r') = (a,a'):: zipit r r'
    | zipit _ _ = raise IllegalInput ("zipit", "list of unequal lengths") 
     
  fun constraints (c as (l,h)) =
  	  (* this might not terminate due to recursive types! *)
      (case (utype l, utype h) of
  	  		 (TVAR _, TVAR _) => LIST [c]
  	  	   | (TVAR _, SIMPLE (FUNC, [td, tr])) =>
  	  	        let val t1 = new_typevar() 
  	  	        	val t2 = new_typevar()
  	  	        in APPEND [leaf (l, make_type (FUNC, [t1,t2])),
  	  	        	       constraints (td, t1), constraints (t2, tr)]
  	  	        end
  	  	   | (TVAR _, SIMPLE (tt, tlist)) =>
  	  	   	    let val tlist' = map (fn _ => new_typevar()) tlist
  	  	   	    in APPEND (leaf (l, make_type (tt, tlist')) ::
  	  	   	    	       map constraints (zipit tlist' tlist))
  	  	   	    end
  	  	   | (SIMPLE (FUNC, [td, tr]), TVAR _) => 
  	  	   	    let val t1 = new_typevar() 
  	  	        	val t2 = new_typevar()
  	  	        in APPEND [leaf (make_type (FUNC, [t1,t2]), h),
  	  	        	       constraints (t1, td), constraints (tr, t2)]
  	  	        end
   	  	   | (SIMPLE (tt, tlist), TVAR _) =>
  	  	   	    let val tlist' = map (fn _ => new_typevar()) tlist
  	  	   	    in APPEND (leaf (make_type (tt, tlist'), h) ::
  	  	   	    	       map constraints (zipit tlist tlist'))
  	  	   	    end
 	  	   | (_,_) => raise IllegalInput ("constraints", "darn"))	

  infix ::=
                
  (* constraint extraction over annotated expression *)
  fun Cexp e Fvars =

      let fun noexp (c as (l,h)) Vars = 
              let val unspecty = make_type (UNSPEC, [])
              in (unify (l, unspecty);
                  (leaf c, EMPTYAO, h))
              end
       
          fun literal (l,h) ad Vars =
              let val (Cd, hd) = Cdatum ad
              in
                  (unify (hd, l);
                   unify (l, h); 
                   (Cd, EMPTYAO, h))
              end

          fun variable (c as (l,h)) (v as (VAR(s, b))) Vars =
               let val b' = lookup s Vars 
               in case utype b' of
                    POLY (C, t) => 
                       let val (C', t') = instantiate (C, t)
                       in (b ::= !!(make_poly (C', t'));
                           unify (l, t');
                           (APPEND [constraints c, LIST C'], EMPTYAO, h))
                       end |
                    _ => 
                       (unify (b, b');
                        unify (b, l);
                        (leaf c, SINGLE v, h)) 
               end

          fun call (c as (l,h)) (frator, frand) Vars =
              let val (Crator, Arator, hrator) = frator Vars 
                  val (Crandl, Arandl, hrandl) = frand Vars
                  val arrowty = make_type (FUNC, [hrandl, l])
              in
                  (unify (hrator, arrowty);
                   (APPEND [leaf c, Crator, Crandl], AND(Arator, Arandl), h))
              end

          val PosTVars: atype list ref = ref []

          fun lambda (c as (l,h)) ((Cfmls, fmls, hfmls), fbody) Vars = 
              let val (Cbody, Abody, hbody) = fbody (fmls @ Vars)
                  val arrowty = make_type (FUNC, [hfmls, hbody])
              in
                  (unify (l, arrowty);
                   (APPEND [leaf c, Cfmls, Cbody],
                    fold (fn (v as (VAR(_, t)), T) => 
                             let val (isused, T') = split v T 
                             in if isused
                                   then T'
                                else (PosTVars := t :: !PosTVars;
                                      T')
                             end) fmls Abody,
                    h))
              end

          fun ifexp (c as (l,h)) (f1, f2, f3) Vars =
          	  let val (C1, A1, h1) = f1 Vars
                  val (C2, A2, h2) = f2 Vars
                  val (C3, A3, h3) = f3 Vars
                  val boolty = make_type (BOOL, [])
              in
                  (unify (h1, boolty);
                   unify (h2, l);
                   unify (h3, l);
                   (APPEND [leaf c, C1, C2, C3], AND(A1, OR(A2, A3)), h))
              end

          fun assign (c as (l, h)) (v as VAR(s,t), f) Vars =
          	  let val (Crhs, Arhs, hrhs) = f Vars
                  val unspecty = make_type (UNSPEC, [])
              in (unify (l, unspecty);
                  unify (t, hrhs);
                  (* here we need an action that notes that t must be updatable (a ref type) *)
                  (Crhs, AND(SINGLE v, Arhs) (* this is not really correct *), h))
              end

          fun pairarg (c as (l,h)) (ffst, frst) Vars =
              let val (Cfst, Afst, hfst) = ffst Vars 
                  val (Crst, Arst, hrst) = frst Vars
                  val consty = make_type (PAIR, [hfst, hrst])
              in
                  (unify (l, consty);
                   (APPEND [leaf c, Cfst, Crst], AND(Afst, Arst), h))
              end

          fun nullarg (c as (l,h)) Vars =
              let val nilty = make_type (NIL, [])
              in
                  (unify (l, nilty);
                   (leaf c, EMPTYAO, h))
              end

          fun avarpar (c as (l,h)) (v as (VAR(_, b))) =
               (unify (b, l);
                unify (l, h);
               (LIST [], [v], h))

          fun apairpar (c as (l,h)) (v as (VAR(_, b)), 
                                    (Crst, frst, hrst)) =
              let val consty = make_type (PAIR, [b, hrst])
              in
                  (unify (l, consty);
                   (APPEND [leaf (h,l), Crst], 
                    if member v frst 
                       then raise StaticSemanticsError "Duplicate parameter"
                    else v :: frst, h))
              end

          fun anullpar (c as (l,h)) =
              let val nilty = make_type (NIL, [])
              in
                  (unify (l, nilty);
                   (leaf (h,l), [], h))
              end

         val Cexp_hom = 
                EHOM {noexp = noexp, literal = literal, variable = variable,
                      call = call, lambda = lambda, ifexp = ifexp,
                      assign = assign, pairarg = pairarg, nullarg = nullarg,
                      avarpar = avarpar, apairpar = apairpar, 
                      anullpar = anullpar}

         val (C, FV, h) = apply_ehom Cexp_hom e Fvars

      in (C, map (fn (VAR (_, t)) => t) (flatten FV), h, !PosTVars)
      end          

  end
end

