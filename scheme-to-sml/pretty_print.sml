structure KernelPrettyPrint =
  struct
 
  (* 11th stage: interpret coercions *)
  
  local
  open KernelExp SchemeTypes SchemeCoercions
  in

(*
  local   
  fun booldat (a,b) = cinterp a
  fun chardat (a,c) = cinterp a
  fun stridat (a,c) = cinterp a
  fun symbdat (a,s) = cinterp a
  fun vectdat (a,l) = cinterp a
  fun pairdat (a,d1,d2) = cinterp a
  fun nildat a = cinterp a
  val dint_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val dint = apply_dhom dint_hom

  fun noexp a = cinterp a
  fun literal (a,d) = (dint d; cinterp a)
  fun variable (a,v) = cinterp a
  fun call (a,e,l) = cinterp a
  fun lambda (a,f,e) = cinterp a
  fun ifexp (a,e,e',e'') = cinterp a
  fun assign (a,v,e) = cinterp a
  fun pairarg (a,e,l) = cinterp a
  fun nullarg a = cinterp a
  fun avarpar (a,p) = ()
  fun apairpar (a,p,f) = ()
  fun anullpar a = ()
  val int_hom = EHOM { noexp = noexp,
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
  val interpret_coercions = apply_ehom int_hom 
  end
*)

  datatype kexp =
    BOOLCONST of bool |
    CHARCONST of string |
    STRICONST of string |
    SYMBCONST of string |
    NUMBCONST of string |
    VECTCONST of kexp list |
    || of kexp * kexp |
    NIL |
    NOTHING |
    IDENT of string |
    FCALL of kexp * kexp |
    LAM of kexp * kexp |
    IFEXP of kexp * kexp * kexp |
    ASSIGNMENT of string * kexp |
    CAPP of coercion * kexp
  
  nonfix ||
  local
  fun f3 F (c,x1,x2,x3) = 
              let val c' = interpret_coercion c
              in if isid c'
                    then F (x1,x2,x3)
                 else CAPP (c, F (x1,x2,x3))
              end

  fun f2 F (c,x1,x2) = 
              let val c' = interpret_coercion c
              in if isid c'
                    then F (x1,x2)
                 else CAPP (c, F (x1,x2))
              end

  fun f1 F (c,x) = 
              let val c' = interpret_coercion c
              in if isid c'
                    then F x
                 else CAPP (c, F x)
              end
  fun f0 F c = 
              let val c' = interpret_coercion c
              in if isid c'
                    then F
                 else CAPP (c, F)
              end
  fun get_string (LAMBOUND (s,_))  = s
    | get_string (FREE (s,_)) = s
    | get_string (LETBOUND (s,_)) = s 

  fun fst (x,_) = x
  fun snd (_,y) = y
	
  val Ddisplay =
         DHOM { booldat = f1 BOOLCONST,
                chardat = f1 CHARCONST,
                stridat = f1 STRICONST,
                symbdat = f1 SYMBCONST,
                numbdat = f1 NUMBCONST,
                vectdat = f1 VECTCONST,
                pairdat = f2 ||,
                nildat = f0 NIL
               }

  val Edisplay =
         EHOM { noexp = f0 NOTHING,
                literal = f1 (apply_dhom Ddisplay),
                variable = f1 (IDENT o get_string o fst),
                call = f2 FCALL,
                lambda = f2 LAM,
                ifexp = f3 IFEXP,
                assign = f2 (fn (v,e) => ASSIGNMENT (get_string(fst v), e)),
                pairarg = f2 ||,
                nullarg = f0 NIL,
                avarpar = fn ((),p) => IDENT(get_string p),
                apairpar = fn ((),p,f) => || (IDENT (get_string p), f),
                anullpar = fn () => NIL 
              }
  in
  val display = apply_ehom Edisplay
  end 

  end
end
