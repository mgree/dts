  (* 11th stage: interpret coercions *)
  
  local
  open Exp Type Coercion Datum General Constraint
  in

  datatype kexp =
    PARAM of string * PPtype |
    BOOLCONST of bool |
    CHARCONST of string |
    STRICONST of string |
    SYMBCONST of string |
    NUMBCONST of string |
    VECTCONST of kexp list |
    CONS of kexp * kexp |
    NIL |
    NOTHING |
    IDENT of string |
    FCALL of kexp * kexp |
    LAM of kexp * kexp |
    IFEXP of kexp * kexp * kexp |
    ASSIGNMENT of string * kexp |
    CAPP of (PPtype * PPtype) * kexp
  
  local
  fun f3 F (c,x1,x2,x3) = CAPP (show_constraint c, F (x1,x2,x3))
  fun f2 F (c,x1,x2) = CAPP (show_constraint c, F (x1,x2))
  fun f1 F (c,x) = CAPP (show_constraint c, F x)
  fun f0 F c = CAPP (show_constraint c, F)

 val Ddisplay =
         DHOM { booldat = f1 BOOLCONST,
                chardat = f1 CHARCONST,
                stridat = f1 STRICONST,
                symbdat = f1 SYMBCONST,
                numbdat = f1 NUMBCONST,
                vectdat = f1 VECTCONST,
                pairdat = f2 CONS,
                nildat = f0 NIL
               }

  val Edisplay =
         EHOM { noexp = f0 NOTHING,
                literal = apply_dhom Ddisplay,
                variable = f1 (IDENT o #1),
                call = f2 FCALL,
                lambda = f2 LAM,
                ifexp = f3 IFEXP,
                assign = f2 (fn (v,e) => ASSIGNMENT (#1 v, e)),
                pairarg = f2 CONS,
                nullarg = f0 NIL,
                avarpar = fn (s, t) => PARAM (s, show_type t),
                apairpar = fn ((s, t),f) => CONS (PARAM (s, show_type t), f),
                anullpar = fn () => NIL 
              }
  in
  val display = apply_ehom Edisplay
  end 

  end
