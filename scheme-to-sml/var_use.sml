 (* 8th stage: classify bound variables as either definitely used 
               or possibly unused *)

  (* 9th stage: propagate positivity all throughout the constraints *)

  local 
  open General Exp Type Coercion

  fun split v EMPTYAO = (false, EMPTYAO)
    | split v (T as (SINGLE v')) = 
        if v = v' 
           then (true, EMPTYAO)
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

  fun noexp a = (EMPTYAO, anil)
  fun literal d = (EMPTYAO, anil)
  fun variable (a, (s, _)) = (SINGLE s, anil)
  fun call (a,(e,ev),(l,lv)) = (AND (e,l), APPEND [ev,lv])
  fun lambda (a,f,e) = 
        fold (fn ((s,t),(T,ts)) => 
                 let val (b, T') = split s T
                 in (T', if b then ts else APPEND [leaf t, ts])
                 end) f e
  fun ifexp (a,(e,ev),(e',ev'),(e'',ev'')) = 
  		(AND (e, OR(e',e'')), APPEND [ev, ev', ev'']) 
  fun assign (a, _ ,e) = e
  fun pairarg (a,(e,ev),(l,lv)) = (AND (e,l), APPEND [ev, lv])
  fun nullarg a = (EMPTYAO, anil)
  fun avarpar p = [p]
  fun apairpar (p,f) = p::f
  fun anullpar () = []
  val use_hom = EHOM { noexp = noexp,
                   literal = literal,
                   variable = variable,
                   call = call,
                   lambda = lambda,
                   ifexp = ifexp,
                   assign = assign,
	           avarpar = avarpar,
                   apairpar = apairpar,
                   anullpar = anullpar,
		   pairarg = pairarg,
                   nullarg = nullarg }

  val free_vars_unused_tvars = apply_ehom use_hom

  (* propgate pos/neg attributes through SVFG *)
  fun propagate ([], []) = ()
    | propagate (tp::rp, ln) =
        let val tatts = attributes tp
            val posptr = #pos tatts
        in if !posptr then 
              propagate (rp, ln)
           else (posptr := true ; 
                (case utype tp of
                   TVAR _ =>
                     propagate (!(#preds tatts) @ rp, ln)
                 | SIMPLE (FUNC, [td,tr]) =>
                     propagate (tr::rp, td::ln)
                 | SIMPLE (_, tlist) =>
                     propagate (tlist @  rp, ln)
                 | _ => error 
                   ("Not a variable or simple type in propagate", "pos")))
        end
    | propagate ([], tn::rn) = 
       let val tatts = attributes tn
            val negptr = #neg tatts
        in if !negptr then 
              propagate ([], rn)
           else (negptr := true ; 
                (case utype tn of
                   TVAR _ => 
                     propagate ([], !(#succs tatts) @ rn)
                  | SIMPLE (FUNC, [td,tr]) =>
                     propagate ([td], tr::rn)
                 | SIMPLE (_, tlist) =>
                     propagate ([], tlist @ rn)
                 |  _ => error 
                   ("Not a variable or simple type in propagate", "neg")))
        end
  in
  fun tvar_usage (ae, t) = 
  	propagate (t :: aflatten [#2 (free_vars_unused_tvars ae)], [])

  end
