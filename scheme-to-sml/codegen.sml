  (* 11th stage: interpret coercions *)
  
  local
  open Exp Type PolyType Coercion Datum SchemeGeneral Constraint
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
    IDENT of string * PPtype |
    FCALL of kexp * kexp |
    LAM of kexp * kexp |
    IFEXP of kexp * kexp * kexp |
    ASSIGNMENT of string * kexp |
    CAPP of (PPtype * PPtype) * kexp
  
  datatype kdef =
    DEF of kexp * kexp
  
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
                variable = f1 (fn (s, TYPE t) => IDENT (s, show_type t)
                                | (s, TSCHEME (C, t)) => IDENT (s, show_type t)),
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
  val display_exp = apply_ehom Edisplay
  fun display_def (DEFINE ((s,t), ae)) = 
  		DEF (PARAM (s, show_type t), display_exp ae)
  end 

  end
