


structure KernelConstraints  : KERNELCONSTRAINTS   =
struct

local open SchemeGeneral KernelTypes SchemeDatum KernelExp

in

  type atype = atype
  type 'a anndatum = 'a anndatum
  type ('a, 'b) annexp = ('a, 'b) annexp
  type 'a variable = 'a variable

  exception NotImplementedCdatum;
  exception NotImplementedCexp;

  (* constraint set = (atype * atype) bintree *)    
  datatype 'a bintree =   EMPTYBIN 
                        | NODE of 'a * 'a bintree list

  fun foreach f EMPTYBIN = ()
    | foreach f (NODE (x, btl)) = (map (foreach f) btl; f x)

  (* set of free variables = 'a variable aotree *) 
  datatype 'a aotree =   EMPTYAO
                       | SINGLE of 'a
                       | OR of 'a aotree * 'a aotree
                       | AND of 'a aotree * 'a aotree
                       | DIF of 'a aotree * 'a aotree
  exception FreeVars of string

  fun formal_vars T =
      let fun sfvo l EMPTYAO = l
            | sfvo l (SINGLE v) = v::l
            | sfvo l (AND (t1, t2)) = sfvo (sfvo l t1) t2
            | sfvo l _ = raise FreeVars "Something is wrong" 
      in sfvo [] T
      end

  fun free_var_occs T =
      let fun fvo fv EMPTYAO = fv
            | fvo fv (SINGLE v) = v :: fv
            | fvo fv (OR (t1, t2)) = fvo (fvo fv t1) t2
            | fvo fv (AND (t1, t2)) = fvo (fvo fv t1) t2
            | fvo fv (DIF (t1, t2)) = 
                let val vs = formal_vars t2
                    val fv1 = fvo fv t1
                    fun fvarfilter (v as VAR (s, b)) 
                                   ((v' as (VAR (s', b')))::r) =
                          if s = s' 
                             then (unify (b, b'); 
                                   fvarfilter v r)
                          else v':: fvarfilter v r
                      | fvarfilter v [] = []
                    fun fvsfilter (bv::r) l =
                          fvsfilter r (fvarfilter bv l)
                      | fvsfilter [] l = l
                in fvsfilter vs fv1
                end
        in fvo [] T
        end

  fun unused_formals T =
      let fun isunused (VAR (s, _)) EMPTYAO = true
            | isunused (VAR (s, _)) (SINGLE (VAR (s', _))) = s <> s'
            | isunused v (AND (t1, t2)) = 
                (isunused v t1 andalso isunused v t2)
            | isunused v (OR (t1, t2)) = (isunused v t1 orelse isunused v t2)
            | isunused (v as (VAR (s, _))) (DIF (t1, t2)) = 
                (member s (map (fn (VAR (s,_)) => s) (formal_vars t2))
                 orelse isunused v t1)
          fun unfo uf EMPTYAO = uf
            | unfo uf (SINGLE _) = uf
            | unfo uf (AND (T1, T2)) = unfo (unfo uf T1) T2
            | unfo uf (OR (T1, T2)) = unfo (unfo uf T1) T2
            | unfo uf (DIF (T1, T2)) = 
                let val fvars = formal_vars T2 
                    fun new_formals uf (v::r) = 
                          if isunused v T1 
                             then new_formals (v :: uf) r
                          else new_formals uf r
                      | new_formals uf [] = unfo uf T1
                in new_formals uf fvars
                end
       in unfo [] T
       end

  fun init () = (new_typevar(), new_typevar()) : (atype * atype)

  fun Annast () = read_exp (init, new_typevar) std_in

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

          fun variable (c as (l,h)) (v as (VAR(_, b))) =
               (unify (b, l);
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

          fun avarpar (c as (l,h)) (v as (VAR(_, b))) =
               (unify (b, l);
                unify (l, h);
               (EMPTYBIN, SINGLE v, h))

          fun apairpar (c as (l,h)) (v as (VAR(_, b)), rst) =
              let val (Crst, Arst, hrst) = rst
                  val consty = make_type (PAIR, [b, hrst])
              in
                  (unify (b, l);
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

