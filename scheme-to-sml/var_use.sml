structure KernelVarUsage =
  struct

 (* 8th stage: classify bound variables as either definitely used 
               or possibly unused *)

  local open KernelExp SchemeVariables SchemeTypes
  in

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

  local
  fun noexp a = EMPTYAO
  fun literal (a,d) = EMPTYAO
  fun variable (a,(p,_)) = SINGLE p
  fun call (a,e,l) = AND (e,l)
  type parameter = (string * atype, string * atype, string * atype) var
  val unusedvars = ref ([]: parameter list)
  fun lambda (a,f,e) = 
        fold (fn (v,T) => let val (b, T') = split v T
                          in if b 
                                then T'
                             else T' before unusedvars := v :: !unusedvars
                          end) f e
  fun ifexp (a,e,e',e'') = AND (e, OR(e',e'')) 
  fun assign (a,(p,_),e) = AND (SINGLE p, e)
  fun pairarg (a,e,l) = AND (e,l)
  fun nullarg a = EMPTYAO
  fun avarpar (a,p) = [p]
  fun apairpar (a,p,f) = p :: f
  fun anullpar a = []
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
  in
  fun free_pos_vars e = 
	(unusedvars := [];
	 (apply_ehom use_hom e, !unusedvars))
  end
  end

end

