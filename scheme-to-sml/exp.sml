structure Exp (*: KERNELEXP *) =
  struct 
  local open Datum General
  in

  type 'v var = string * 'v

  datatype ('a, 'b, 'c) il =
    CONS of 'b * ('a, 'b, 'c) ilist |
    NIL |
    OTHER of 'c
  withtype ('a, 'b, 'c) ilist = ('a, 'b, 'c) il * 'a

  datatype 't coercion =
     CVAR of int
   | IDC 
   | TAG of 't
   | CHECK of 't
   | MAP of 't * 't coercion list
   | COMP of 't coercion * 't coercion

  datatype ('a, 'v, 't) exp =
    LITERAL of 'a datum |
    VARIABLE of 'v var |
    CALL of ('a, 'v, 't) expression * ('a, ('a, 'v, 't) expression, unit) ilist |
    LAMBDA of ('a, 'v var, 'v var) ilist * ('a, 'v, 't) body |
    IF of ('a, 'v, 't) expression * ('a, 'v, 't) expression * ('a, 'v, 't) expression |  
    ASSIGN of 'v var * ('a, 'v, 't) expression |
    LET of ('a, 'v, 't) body |
    APPLY of 't coercion * ('a, 'v, 't) expression |
    NOEXP
  withtype ('a, 'v, 't) expression = ('a, 'v, 't) exp * 'a
  and ('a, 'v, 't) definitions = ('v var * ('a, 'v, 't) expression) list
  and ('a, 'v, 't) body = ('a, 'v, 't) definitions * ('a, 'v, 't) expression

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

