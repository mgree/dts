structure Exp (*: KERNELEXP *) =
  struct 
  local open Datum General
  in

  type 'v var = string * 'v
  
  datatype ('a, 'd, 'e, 'l, 'f, 't, 'v) exp_hom =
    EHOM of { noexp: 'a -> 'e,
              literal: 'd -> 'e,
              variable: 'a * 'v var -> 'e,
              call: 'a * 'e * 'l -> 'e,
              lambda: 'a * 'f * 'e -> 'e,
              ifexp: 'a * 'e * 'e * 'e -> 'e,
              assign: 'a * 'v var * 'e -> 'e,
              pairarg: 'a * 'e * 'l -> 'l,
              nullarg: 'a -> 'l,
              avarpar: 't var -> 'f,
              apairpar: 't var * 'f -> 'f,
              anullpar: unit -> 'f }

  datatype ('a, 't, 'v) exp =
    NOEXP of 'a |
    LITERAL of 'a datum |
    VARIABLE of 'a * 'v var |
    CALL of 'a * ('a, 't, 'v) exp * ('a, 't, 'v) args |
    LAMBDA of 'a * 't formals * ('a, 't, 'v) exp |
    IF of 'a * ('a, 't, 'v) exp * ('a, 't, 'v) exp * ('a, 't, 'v) exp |  
    ASSIGN of 'a * 'v var * ('a, 't, 'v) exp

  and ('a, 't, 'v) args =
    PAIRARG of 'a * ('a, 't, 'v) exp * ('a, 't, 'v) args |
    NULLARG of 'a

  and 't formals =
    AVARPAR of 't var |
    APAIRPAR of 't var * 't formals |
    ANULLPAR

  fun eattrib (NOEXP a) = a
    | eattrib (LITERAL d) = dattrib d
    | eattrib (VARIABLE (a,v)) = a
    | eattrib (CALL (a,_,_)) = a
    | eattrib (LAMBDA (a,_,_)) = a
    | eattrib (IF(a,_,_,_)) = a
    | eattrib (ASSIGN(a,_,_)) = a 

  fun apply_ehom (EHOM E) =
      let fun eapply (NOEXP a) = #noexp E a
            | eapply (LITERAL d) = #literal E d
            | eapply (VARIABLE (a,v)) = #variable E (a,v) 
            | eapply (CALL (a, e, l)) = #call E (a, eapply e, lapply l)
            | eapply (LAMBDA (a, f, e)) = #lambda E (a, fapply f, eapply e)
            | eapply (IF (a, e1, e2, e3)) = #ifexp E (a, eapply e1, eapply e2, eapply e3)
            | eapply (ASSIGN (a, v, e)) = #assign E (a, v, eapply e)
          and fapply (AVARPAR v) = #avarpar E v
            | fapply (APAIRPAR (v, f')) = #apairpar E (v, fapply f')
            | fapply ANULLPAR = #anullpar E () 
          and lapply (PAIRARG (a, e, l')) = #pairarg E (a, eapply e, lapply l')
            | lapply (NULLARG a) = #nullarg E a 
      in eapply
      end

  exception ParseError of string
  
  val keywords = ["lambda", "if", "set!", "quote"]
        
  fun dat2exp (parinit, varinit) = 
      let fun dat2e (PAIRDAT (a,d1,d2)) =
              (case d1 of
                 SYMBDAT (_, "lambda") => dat2lambda (a,d2)
               | SYMBDAT (_, "if") => dat2if (a,d2)
               | SYMBDAT (_, "set!") => dat2assign (a,d2)
               | SYMBDAT (_, "quote") => dat2quote (a,d2)
               | _ => CALL (a, dat2e d1, dat2args d2))
            | dat2e (SYMBDAT (a,s)) = VARIABLE (a, (s, varinit (a,s)))
            | dat2e d = LITERAL d
          and dat2lambda (a, (PAIRDAT (_, f, (PAIRDAT (_, e, NILDAT _))))) =
                 LAMBDA (a, dat2formals f, dat2e e)
            | dat2lambda _ = raise ParseError ("Illegal lambda expression")
          and dat2formals (SYMBDAT (a,s)) = 
          	     if member s keywords
          	     	then raise ParseError ("Illegal parameter (keyword)")
          	     else AVARPAR (s, parinit (a,s))
            | dat2formals (PAIRDAT (a,d1,d2)) = 
                 APAIRPAR (dat2par d1, dat2formals d2)
            | dat2formals (NILDAT a) = ANULLPAR
            | dat2formals _ = raise ParseError ("Illegal formals")
          and dat2par (SYMBDAT (a,s)) =
                if member s keywords
                    then raise ParseError ("Illegal parameter (keyword)")
                 else (s, parinit (a,s))
            | dat2par _ = 
                raise ParseError ("Illegal parameter (not an identifier)")
          and dat2if (a, (PAIRDAT (_, d1, PAIRDAT (_, d2, d3)))) =
                IF (a, dat2e d1, dat2e d2, dat2else d3)
            | dat2if _ = raise ParseError ("Illegal if expression")
          and dat2else (PAIRDAT (_, d, NILDAT _)) =
                 dat2e d
            | dat2else (NILDAT a) = NOEXP a
            | dat2else _ = raise ParseError ("Illegal if expression (else)")
          and dat2assign (a, (PAIRDAT (_,d1, PAIRDAT (_,d2, NILDAT _)))) =
                 ASSIGN (a, dat2var d1, dat2e d2)
            | dat2assign _ = raise ParseError ("Illegal assignment")
          and dat2quote (a, PAIRDAT (_,d1, NILDAT _)) = 
                 LITERAL d1 
            | dat2quote _ = raise ParseError ("Illegal quotation")
          and dat2var (SYMBDAT (a,s)) = 
                 if member s keywords 
                    then raise ParseError ("Illegal variable (keyword)")
                 else (s, varinit (a,s))
            | dat2var _ = raise ParseError ("Illegal variable (not an identifier)")
          and dat2args (PAIRDAT (a, d1, d2)) =
                PAIRARG (a, dat2e d1, dat2args d2)
            | dat2args (NILDAT a) = NULLARG a
            | dat2args _ = raise ParseError ("Illegal argument list")
      in
      dat2e
      end

  datatype ('a, 't, 'v) attinit =
      INIT of { parameter: 'a * string -> 't,
                variable: 'a * string -> 'v,
                exp: unit -> 'a}
                                    
  fun read_exp (INIT J) ip = 
  	dat2exp (#parameter J, #variable J) (read_datum (#exp J) ip)

  end
end

