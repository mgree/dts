  datatype ('d, 'p, 'v, 'e, 'ea, 'l, 'la, 'f, 'fa) exp_hom =
    EHOM of { noexp: 'ea -> 'e,
              literal: 'ea * 'd -> 'e,
              variable: 'ea * 'v -> 'e,
              call: 'ea * 'e * 'l -> 'e,
              lambda: 'ea * 'f * 'e -> 'e,
              ifexp: 'ea * 'e * 'e * 'e -> 'e,
              assign: 'ea * 'v * 'e -> 'e,
              letexp: 'ea * 'f * 'l * 'e -> 'e,
              letrec: 'ea * 'f * 'l * 'e -> 'e,
              pairarg: 'la * 'e * 'l -> 'l,
              nullarg: 'la -> 'l,
              avarpar: 'fa * 'p -> 'f,
              apairpar: 'fa * 'p * 'f -> 'f,
              anullpar: 'fa -> 'f }

  datatype ('d, 'p, 'v, 'e, 'l, 'f) exp =
    NOEXP of 'e |
    LITERAL of 'e * 'd |
    VARIABLE of 'e * 'v |
    CALL of 'e * ('d, 'p, 'v, 'e, 'l, 'f) exp * ('d, 'p, 'v, 'e, 'l, 'f) args |
    LAMBDA of 'e * ('p, 'f) formals * ('d, 'p, 'v, 'e, 'l, 'f) exp |
    IF of 'e * ('d, 'p, 'v, 'e, 'l, 'f) exp * ('d, 'p, 'v, 'e, 'l, 'f) exp * 
          ('d, 'p, 'v, 'e, 'l, 'f) exp |  
    ASSIGN of 'e * 'v * ('d, 'p, 'v, 'e, 'l, 'f) exp |
    LET of 'e * ('p, 'f) formals * ('d, 'p, 'v, 'e, 'l, 'f) args *
        ('d, 'p, 'v, 'e, 'l, 'f) exp |
    LETREC of 'e * ('p, 'f) formals * ('d, 'p, 'v, 'e, 'l, 'f) args *
        ('d, 'p, 'v, 'e, 'l, 'f) exp 

  and ('d, 'p, 'v, 'e, 'l, 'f) args =
    PAIRARG of 'l * ('d, 'p, 'v, 'e, 'l, 'f) exp * 
                    ('d, 'p, 'v, 'e, 'l, 'f) args |
    NULLARG of 'l

  and ('p, 'f) formals =
    AVARPAR of 'f * 'p |
    APAIRPAR of 'f * 'p * ('p, 'f) formals |
    ANULLPAR of 'f

  fun eattrib (NOEXP a) = a
    | eattrib (LITERAL (a,_)) = a
    | eattrib (VARIABLE (a,_)) = a
    | eattrib (CALL (a,_,_)) = a
    | eattrib (LAMBDA (a,_,_)) = a
    | eattrib (IF(a,_,_,_)) = a
    | eattrib (ASSIGN(a,_,_)) = a 
    | eattrib (LET(a,_,_,_)) = a
    | eattrib (LETREC(a,_,_,_)) = a

  fun apply_ehom (EHOM E) =
      let fun eapply (NOEXP a) = #noexp E a
            | eapply (LITERAL (a,d)) = #literal E (a, d)
            | eapply (VARIABLE v) = #variable E v 
            | eapply (CALL (a, e, l)) = #call E (a, eapply e, lapply l)
            | eapply (LAMBDA (a, f, e)) = #lambda E (a, fapply f, eapply e)
            | eapply (IF (a, e1, e2, e3)) = #ifexp E (a, eapply e1, eapply e2, eapply e3)
            | eapply (ASSIGN (a, v, e)) = #assign E (a, v, eapply e)
            | eapply (LET (a, f, l, e)) = #letexp E (a, fapply f, lapply l, eapply e)
            | eapply (LETREC (a, f, l, e)) = #letrec E (a, fapply f, lapply l, eapply e)
          and fapply (AVARPAR a) = #avarpar E a
            | fapply (APAIRPAR (a, v, f')) = #apairpar E (a, v, fapply f')
            | fapply (ANULLPAR a) = #anullpar E a 
          and lapply (PAIRARG (a, e, l')) = #pairarg E (a, eapply e, lapply l')
            | lapply (NULLARG a) = #nullarg E a 
      in eapply
      end

  datatype ('a, 'ea, 'la, 'fa, 'pa, 'va) attinit =
      INIT of { parameter: string -> 'pa,
                    variable: string -> 'va,
                    exp: 'a -> 'ea,
                    args: 'a -> 'la,
                    formals: 'a -> 'fa }
                    
  exception ParseError of string
  
  val keywords = ["lambda", "if", "set!", "quote", "let", "letrec"]
        
  fun dat2exp (INIT I) = 
      let fun dat2e (PAIRDAT (a,d1,d2)) =
              (case d1 of
                 SYMBDAT (_, "lambda") => dat2lambda (a,d2)
               | SYMBDAT (_, "if") => dat2if (a,d2)
               | SYMBDAT (_, "set!") => dat2assign (a,d2)
               | SYMBDAT (_, "quote") => dat2quote (a,d2)
               | SYMBDAT (_, "let") => dat2let (a,d2)
               | SYMBDAT (_, "letrec") => dat2letrec (a,d2)
               | _ => CALL (#exp I a, dat2e d1, dat2args d2))
            | dat2e (SYMBDAT (a,s)) = VARIABLE (#exp I a, #variable I s)
            | dat2e d = LITERAL (#exp I (dattrib d), d)
          and dat2lambda (a, (PAIRDAT (_, f, (PAIRDAT (_, e, NILDAT _))))) =
                 LAMBDA (#exp I a, dat2formals f, dat2e e)
            | dat2lambda _ = raise ParseError ("Illegal lambda expression")
          and dat2formals (SYMBDAT (a,s)) = 
                 AVARPAR (#formals I a, #parameter I s)
            | dat2formals (PAIRDAT (a,d1,d2)) = 
                 APAIRPAR (#formals I a, dat2par d1, dat2formals d2)
            | dat2formals (NILDAT a) = ANULLPAR (#formals I a)
            | dat2formals _ = raise ParseError ("Illegal formals")
          and dat2par (SYMBDAT (a,s)) =
                if member s keywords
                    then raise ParseError ("Illegal parameter (keyword)")
                 else #parameter I s
            | dat2par _ = 
                raise ParseError ("Illegal parameter (not an identifier)")
          and dat2if (a, (PAIRDAT (_, d1, PAIRDAT (_, d2, d3)))) =
                IF (#exp I a, dat2e d1, dat2e d2, dat2else d3)
            | dat2if _ = raise ParseError ("Illegal if expression")
          and dat2else (PAIRDAT (_, d, NILDAT _)) =
                 dat2e d
            | dat2else (NILDAT a) = NOEXP (#exp I a)
            | dat2else _ = raise ParseError ("Illegal if expression (else)")
          and dat2assign (a, (PAIRDAT (_,d1, PAIRDAT (_,d2, NILDAT _)))) =
                 ASSIGN (#exp I a, dat2var d1, dat2e d2)
            | dat2assign _ = raise ParseError ("Illegal assignment")
          and dat2quote (a, PAIRDAT (_,d1, NILDAT _)) = 
                 LITERAL (#exp I a, d1) 
            | dat2quote _ = raise ParseError ("Illegal quotation")
          and dat2var (SYMBDAT (a,s)) = 
                 if member s keywords 
                    then raise ParseError ("Illegal variable (keyword)")
                 else #variable I s
            | dat2var _ = raise ParseError ("Illegal variable (not an identifier)")
          and dat2let (a, PAIRDAT(_,d1,PAIRDAT(_,d2,NILDAT _))) =
                let val (f, l) = dat2bindings d1
                in LET (#exp I a, f, l, dat2e d2)
                end
            | dat2let _ = raise ParseError ("Illegal let-expression")
          and dat2letrec (a, PAIRDAT(_,d1,PAIRDAT(_,d2,NILDAT _))) =
                let val (f, l) = dat2bindings d1
                in LETREC (#exp I a, f, l, dat2e d2)
                end
            | dat2letrec _ = raise ParseError ("Illegal letrec-expression")
          and dat2bindings (NILDAT a) = (ANULLPAR (#formals I a), NULLARG (#args I a))
            | dat2bindings (PAIRDAT(a,d1,d2)) =
                 let val (p,e) = dat2binding d1
                     val (f,l) = dat2bindings d2
                 in (APAIRPAR (#formals I a, p, f), PAIRARG (#args I a, e, l))
                 end
            | dat2bindings _ = 
                 raise ParseError ("Illegal list of bindings in let/letrec-expression")
          and dat2binding (PAIRDAT(_,d1,PAIRDAT(_,d2,NILDAT _))) =
                 (dat2par d1, dat2e d2)
            | dat2binding _ = raise ParseError ("Illegal binding in let/letrec-expression")
          and dat2args (PAIRDAT (a, d1, d2)) =
                PAIRARG (#args I a, dat2e d1, dat2args d2)
            | dat2args (NILDAT a) = NULLARG (#args I a)
            | dat2args _ = raise ParseError ("Illegal argument list")
      in
      dat2e
      end

  fun read_exp dinit Ie ip = dat2exp Ie (read_datum dinit ip)

