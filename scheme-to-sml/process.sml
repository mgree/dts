structure ProcessKernel =
  struct
  local open SchemeDatum KernelExp KernelTypes KernelConstraints
  in

  fun parse ip = read_exp (fn () => (new_typevar(), 
                        new_typevar()), new_typevar) ip
 
  fun process e =   
      let val (Cset, FVars, hi_type, PosTVars) = Cexp e
      in (foreach equiv Cset;
          foreach pred_succ Cset;
          propagate (hi_type :: PosTVars, FVars);
          foreach (fn (l,h) => (interpret l; interpret h)) Cset;  
          e)
      end

  datatype coercion =
    TAG of type_tag |
    CHECK of type_tag |
    CVAR |
    ID

  fun coercion (l,h) =
      case (utype l, utype h) of
        (SIMPLE (tt, _), DYN _) => TAG tt |
        (DYN _, SIMPLE (tt, _)) => CHECK tt |
        (SIMPLE _, SIMPLE _) => ID |
        (DYN _, DYN _) => ID |
        (TVAR _, TVAR _) => ID |
        (_, _) => CVAR
 
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

  local fun f F (l,h) x = 
              let val c = coercion (l,h)
              in if c = ID 
                    then F x
                 else CAPP (c, F x)
              end
        fun f0 F (l,h) = 
              let val c = coercion (l,h)
              in if c = ID 
                    then F
                 else CAPP (c, F)
              end
         fun get_string (VAR (s,_)) = s
    in
    val Ddisplay =
         DHOM { booldat = f BOOLCONST,
                chardat = f CHARCONST,
                stridat = f STRICONST,
                symbdat = f SYMBCONST,
                numbdat = f NUMBCONST,
                vectdat = f VECTCONST,
                pairdat = f ||,
                nildat = f0 NIL
               }

    val Edisplay =
         EHOM { noexp = f0 NOTHING,
                literal = f (apply_dhom Ddisplay),
                variable = f (IDENT o get_string),
                call = f FCALL,
                lambda = f LAM,
                ifexp = f IFEXP,
                assign = f (fn (VAR(s,_),e) => ASSIGNMENT (s, e)),
                pairarg = f ||,
                nullarg = f0 NIL,
                avarpar = f (IDENT o get_string),
                apairpar = f (fn (VAR(s,_), e) => || (IDENT s, e)),
                anullpar = f0 NIL 
              }

    val display = apply_ehom Edisplay
    end 
                 
          
  end
end

