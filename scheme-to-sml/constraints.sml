structure KernelConstraints: KERNELCONSTRAINTS =
  struct
  local open SchemeGeneral KernelTypes SchemeDatum KernelExp
  in

  type atype = atype
  type ('a, 'b) annexp = ('a, 'b) annexp
  type 'a variable = 'a variable

  (* constraint set = (atype * atype) bintree *)    
  datatype 'a bintree =   EMPTYBIN 
                        | NODE of 'a * 'a bintree list

  fun leaf a = NODE(a, [])

  fun foreach f EMPTYBIN = ()
    | foreach f (NODE (x, btl)) = (map (foreach f) btl; f x)

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
                   (NODE(c,[C1,C2]), h))
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
                 
  (* constraint extraction over annotated expression *)
  fun Cexp e =

      let fun noexp (c as (l,h)) = 
              let val unspecty = make_type (UNSPEC, [])
              in (unify (l, unspecty);
                  (leaf c, EMPTYAO, h))
              end
       
          fun literal (l,h) ad =
              let val (Cd, hd) = Cdatum ad
              in
                  (unify (hd, l);
                   unify (l, h); 
                   (Cd, EMPTYAO, h))
              end

          fun variable (c as (l,h)) (v as (VAR(_, b))) =
               (unify (b, l);
               (leaf c, SINGLE v, h))

          fun call (c as (l,h)) ((Crator, Arator, hrator), 
                                 (Crandl, Arandl, hrandl)) = 
              let val arrowty = make_type (FUNC, [hrandl, l])
              in
                  (unify (hrator, arrowty);
                   (NODE(c,[Crator,Crandl]), AND(Arator, Arandl), h))
              end

          val PosTVars: atype list ref = ref []

          fun lambda (c as (l,h)) ((Cfmls, fmls, hfmls), (Cbody, Abody, hbody)) = 
              let val arrowty = make_type (FUNC, [hfmls, hbody])
              in
                  (unify (l, arrowty);
                   (NODE(c, [Cfmls, Cbody]),
                    fold (fn (v as (VAR(_, t)), T) => 
                             let val (isused, T') = split v T 
                             in if isused
                                   then T'
                                else (PosTVars := t :: !PosTVars;
                                      T')
                             end) fmls Abody,
                    h))
             end

          fun ifexp (c as (l,h)) ((C1, A1, h1), 
                    (C2, A2, h2), (C3, A3, h3)) =
              let val boolty = make_type (BOOL, [])
              in
                  (unify (h1, boolty);
                   unify (h2, l);
                   unify (h3, l);
                   (NODE(c,[C1, C2, C3]), AND(A1, OR(A2, A3)), h))
              end

          fun assign (c as (l, h)) (v as VAR(s,t), (Crhs, Arhs, hrhs)) =
              let val unspecty = make_type (UNSPEC, [])
              in (unify (l, unspecty);
                  unify (t, hrhs);
                  (* here we need an action that notes that t must be updatable (a ref type) *)
                  (Crhs, AND(SINGLE v, Arhs) (* this is not really correct *), h))
              end

          fun pairarg (c as (l,h)) ((Cfst, Afst, hfst),
                                   (Crst, Arst, hrst)) = 
              let val consty = make_type (PAIR, [hfst, hrst])
              in
                  (unify (l, consty);
                   (NODE(c, [Cfst, Crst]), AND(Afst, Arst), h))
              end

          fun nullarg (c as (l,h)) =
              let val nilty = make_type (NIL, [])
              in
                  (unify (l, nilty);
                   (leaf c, EMPTYAO, h))
              end

          fun avarpar (c as (l,h)) (v as (VAR(_, b))) =
               (unify (b, l);
                unify (l, h);
               (EMPTYBIN, [v], h))

          fun apairpar (c as (l,h)) (v as (VAR(_, b)), 
                                    (Crst, frst, hrst)) =
              let val consty = make_type (PAIR, [b, hrst])
              in
                  (unify (l, consty);
                   (NODE((h,l), [Crst]), 
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

         val (C, FV, h) = apply_ehom Cexp_hom e

      in (C, map (fn (VAR (_, t)) => t) (flatten FV), h, !PosTVars)
      end          
 
  end
end

