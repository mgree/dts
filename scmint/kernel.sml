(*
signature KERNELCONSTRAINTS =
sig



end
*)



structure KernelConstraints =
struct

local open KernelTypes SchemeDatum KernelExp

in
  exception NotImplementedCdatum;
  exception NotImplementedCexp;

  (* constraint set = (atype * atype) bintree *)    
  datatype 'a bintree =   EMPTYBIN 
                        | NODE of 'a * 'a bintree list

  (* set of free variables = 'a variable aotree *) 
  datatype 'a aotree =   EMPTYAO
                       | SINGLE of 'a
                       | OR of 'a aotree * 'a aotree
                       | AND of 'a aotree * 'a aotree
                       | DIF of 'a aotree * 'a aotree

  fun init () = (new_typevar(), new_typevar()) : (atype * atype)

  fun Annast () = read_exp (init, init) std_in

  fun leaf a = NODE(a, [])

  (* constraint extraction over annotated datum *)
  fun Cdatum ad = 
      let fun booldat (c as (l,h)) b = 
              let val boolty = make_type (BOOL, [])
              in
                  (unify (l, boolty);
                   (leaf(l,h), EMPTYAO, h))
              end 

          fun nildat (c as (l,h)) = 
              let val nilty = make_type (NIL, [])
              in
                  (unify (l, nilty);
                   (leaf c, EMPTYAO, h))
              end

          fun pairdat (c as (l,h)) (fst, snd) =
              let val (C1, A1, h1) = fst
                  val (C2, A2, h2) = snd
                  val consty = make_type (PAIR, [h1, h2])
              in
                  (unify (l, consty);
                   (NODE(c,[C1,C2]), EMPTYAO, h))
              end 
          fun notimp _ _ = raise NotImplementedCdatum
          val Cdatum_hom =  
                 DHOM {booldat = booldat, chardat = notimp, stridat = notimp, 
                       symbdat = notimp, numbdat = notimp, vectdat = notimp, 
                       pairdat = pairdat, nildat = nildat}
          in
              apply_dhom Cdatum_hom ad
          end     

  (* constraint extraction over annotated expression *)
  fun Cexp e =

      let fun noexp (c as (_,h)) = (leaf c, EMPTYAO, h)
       
          fun literal (l,h) (ad as (DATUM (_, (l',h')))) =
              let val (Cd, Ad, _) = Cdatum ad
              in
                  (unify (l', h');
                   unify (l, h); 
                   (Cd, Ad, h))
              end

          fun variable (c as (_,h)) (v as (VAR(_, (l',h')))) =
              (unify (l', h');
               (leaf c, SINGLE v, h))

          fun call (c as (l,h)) (rator, randl) =
              let val (Crator, Arator, hrator) = rator
                  val (Crandl, Arandl, hrandl) = randl
                  val arrowty = make_type (FUNC, [hrator, hrandl])
              in
                  (unify (hrator, arrowty);
                   (NODE(c,[Crator,Crandl]), AND(Arator, Arandl), h))
              end

          fun lambda (c as (l,h)) (fmls, body) =
              let val (Cfmls, Afmls, hfmls) = fmls (* Afmls contains binding occ's *)
                  val (Cbody, Abody, hbody) = body
                  val arrowty = make_type (FUNC, [hfmls, hbody])
              in
                  (unify (l, arrowty);
                   (NODE(c, [Cfmls, Cbody]), DIF(Abody, Afmls), h))
              end

          fun ifexp (c as (l,h)) (e1, e2, e3) =
              let val (C1, A1, h1) = e1
                  val (C2, A2, h2) = e2
                  val (C3, A3, h3) = e3
                  val boolty = make_type (BOOL, [])
              in
                  (unify (h1, boolty);
                   unify (h2, h3);
                   unify (h2, l);
                   (NODE(c,[C1, C2, C3]), AND(A1, OR(A2, A3)), h))
              end

          fun pairarg (c as (l,h)) (fst, rst) =
              let val (Cfst, Afst, hfst) = fst
                  val (Crst, Arst, hrst) = rst
                  val consty = make_type (PAIR, [hfst, hrst])
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

          fun avarpar (c as (l,h)) (v as (VAR(_, (l',h')))) =
              (unify (l', h');
               unify (l, h);
               unify (l, h');
               (EMPTYBIN, SINGLE v, h))

          fun apairpar (c as (l,h)) (v as (VAR(_,(l',h'))), rst) =
              let val (Crst, Arst, hrst) = rst
                  val consty = make_type (PAIR, [h', hrst])
              in
                  (unify (l', h');
                   unify (l, consty);
                   (NODE(c, [Crst]), AND(SINGLE v, Arst), h))
              end

          fun anullpar (c as (l,h)) =
              let val nilty = make_type (NIL, [])
              in
                  (unify (l, nilty);
                   (leaf c, EMPTYAO, h))
              end

          fun notimp _ _ = raise NotImplementedCexp
     
          val Cexp_hom = 
                EHOM {noexp = noexp, literal = literal, variable = variable,
                      call = call, lambda = lambda, ifexp = ifexp,
                      assign = notimp, pairarg = pairarg, nullarg = nullarg,
                      avarpar = avarpar, apairpar = apairpar, 
                      anullpar = anullpar}

      in
           apply_ehom Cexp_hom e
      end          
          
                    









end
end

