structure Exp (*: KERNELEXP *) =
  struct 
  open Datum General

  type 'v var = string * 'v

  datatype 't formals =
    AVARPAR of 't var |
    APAIRPAR of 't var * 't formals |
    ANULLPAR

  datatype ('a, 't, 'v) uexp =
    NOEXP |
    LITERAL of 'a datum |
    VARIABLE of 'v var |
    CALL of ('a, 't, 'v) exp * ('a, 't, 'v) args |
    LAMBDA of 't formals * ('a, 't, 'v) exp |
    IF of ('a, 't, 'v) exp * ('a, 't, 'v) exp * ('a, 't, 'v) exp |  
    ASSIGN of 'v var * ('a, 't, 'v) exp
  and ('a, 't, 'v) uargs =
    PAIRARG of ('a, 't, 'v) exp * ('a, 't, 'v) args |
    NULLARG
  withtype ('a, 't, 'v) exp = ('a, 't, 'v) uexp * 'a
  and      ('a, 't, 'v) args = ('a, 't, 'v) uargs * 'a

  exception ParseError of string
  
  val keywords = ["lambda", "if", "set!", "quote"]
        
  fun dat2exp (parinit, varinit) = 
      let fun dat2e (PAIRDAT (d1,d2), a) =
              (case d1 of
                 (SYMBDAT "lambda", _) => dat2lambda (a,d2)
               | (SYMBDAT "if", _) => dat2if (a,d2)
               | (SYMBDAT "set!", _) => dat2assign (a,d2)
               | (SYMBDAT "quote", _) => dat2quote (a,d2)
               | _ => (CALL (dat2e d1, dat2args d2), a))
            | dat2e (SYMBDAT s, a) = (VARIABLE (s, varinit (a,s)), a)
            | dat2e (d as (ud,a)) = (LITERAL d, a)
          and dat2lambda (a, (PAIRDAT (f, (PAIRDAT (e, (NILDAT, _)), _)), _)) =
                 (LAMBDA (dat2formals nil f, dat2e e), a)
            | dat2lambda _ = raise ParseError ("Illegal lambda expression")
          and dat2formals seen (SYMBDAT s, a) = 
          	     if member s keywords
          	     	then raise ParseError ("Illegal single/dot parameter (keyword)")
                     else if member s seen
                        then raise ParseError ("Duplicate single/dot parameter")
          	     else AVARPAR (s, parinit (a,s))
            | dat2formals seen (PAIRDAT (d1,d2), _) = 
		 let val par as (s, _) = dat2par seen d1
                 in APAIRPAR (par, dat2formals (s::seen) d2)
                 end
            | dat2formals seen (NILDAT, a) = ANULLPAR
            | dat2formals _ _ = raise ParseError ("Illegal formals")
          and dat2par seen (SYMBDAT s, a) =
                if member s keywords
                    then raise ParseError ("Illegal parameter (keyword)")
                else if member s seen
                    then raise ParseError ("Duplicate parameter")
                else (s, parinit (a,s))
            | dat2par _ _ = 
                raise ParseError ("Illegal parameter (not an identifier)")
          and dat2if (a, (PAIRDAT (d1, (PAIRDAT (d2, d3), _)), _)) =
                (IF (dat2e d1, dat2e d2, dat2else d3), a)
            | dat2if _ = raise ParseError ("Illegal if expression")
          and dat2else (PAIRDAT (d, (NILDAT, _)), _) =
                 dat2e d
            | dat2else (NILDAT, a) = (NOEXP, a)
            | dat2else _ = raise ParseError ("Illegal if expression (else)")
          and dat2assign (a, (PAIRDAT (d1, (PAIRDAT (d2, (NILDAT, _)), _)), _)) =
                 (ASSIGN (dat2var d1, dat2e d2), a)
            | dat2assign _ = raise ParseError ("Illegal assignment")
          and dat2quote (a, (PAIRDAT (d1, (NILDAT, _)), _)) = 
                 (LITERAL d1, dattrib d1)
            | dat2quote _ = raise ParseError ("Illegal quotation")
          and dat2var (SYMBDAT s, a) = 
                 if member s keywords 
                    then raise ParseError ("Illegal variable (keyword)")
                 else (s, varinit (a,s))
            | dat2var _ = raise ParseError ("Illegal variable (not an identifier)")
          and dat2args (PAIRDAT (d1, d2), a) =
                (PAIRARG (dat2e d1, dat2args d2), a)
            | dat2args (NILDAT, a) = (NULLARG, a)
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

