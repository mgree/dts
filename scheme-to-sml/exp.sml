structure KernelExp (*: KERNELEXP *) =
  struct 
  local open SchemeDatum 
  in

  datatype ('e, 'ea, 'd, 'v, 'l, 'f) exp_hom =
    EHOM of { noexp: 'ea -> 'e,
              literal: 'ea * 'd -> 'e,
              variable: 'ea * 'v -> 'e,
              call: 'ea * 'e * 'l -> 'e,
              lambda: 'ea * 'f * 'e -> 'e,
              ifexp: 'ea * 'e * 'e * 'e -> 'e,
              assign: 'ea * 'v * 'e -> 'e }

  datatype ('l, 'la, 'e) arg_hom =
    LHOM of { pairarg: 'la * 'e * 'l -> 'l,
              nullarg: 'la -> 'l }

  datatype ('f, 'fa, 'p) form_hom =
    FHOM of { avarpar: 'fa * 'p -> 'f,
              apairpar: 'fa * 'p * 'f -> 'f,
              anullpar: 'fa -> 'f }

  datatype ('d, 'v, 'p, 'e, 'l, 'f) exp =
    NOEXP of 'e |
    LITERAL of 'e * 'd |
    VARIABLE of 'e * 'v |
    CALL of 'e * ('d, 'v, 'p, 'e, 'l, 'f) exp * ('d, 'v, 'p, 'e, 'l, 'f) args |
    LAMBDA of 'e * ('f, 'p) formals * ('d, 'v, 'p, 'e, 'l, 'f) exp |
    IF of 'e * ('d, 'v, 'p, 'e, 'l, 'f) exp * ('d, 'v, 'p, 'e, 'l, 'f) exp * 
          ('d, 'v, 'p, 'e, 'l, 'f) exp |  
    ASSIGN of 'e * 'v * ('d, 'v, 'p, 'e, 'l, 'f) exp
  and ('d, 'v, 'p, 'e, 'l, 'f) args =
    PAIRARG of 'l * ('d, 'v, 'p, 'e, 'l, 'f) exp * ('d, 'v, 'p, 'e, 'l, 'f) args |
    NULLARG of 'l

  and ('f, 'p) formals =
    AVARPAR of 'f * 'p |
    APAIRPAR of 'f * 'p * ('f, 'p) formals |
    ANULLPAR of 'f

  fun apply_fhom (FHOM F) = 
    let fun fapply (AVARPAR a) = #avarpar F a
              | fapply (APAIRPAR (a, v, f')) = #apairpar F (a, v, fapply f')
              | fapply (ANULLPAR a) = #anullpar F a 
        in fapply
        end
          
  fun apply_ehom (D,  EHOM E, LHOM L, F) =
      let fun eapply (NOEXP a) = #noexp E a
                | eapply (LITERAL (a,d)) = #literal E (a, apply_dhom D d)
                | eapply (VARIABLE v) = #variable E v 
            | eapply (CALL (a, e, l)) = #call E (a, eapply e, lapply l)
                | eapply (LAMBDA (a, f, e)) = #lambda E (a, apply_fhom F f, eapply e)
                | eapply (IF (a, e1, e2, e3)) = #ifexp E (a, eapply e1, eapply e2, eapply e3)
                | eapply (ASSIGN (a, v, e)) = #assign E (a, v, eapply e)
          and lapply (PAIRARG (a, e, l')) = #pairarg L (a, eapply e, lapply l')
                | lapply (NULLARG a) = #nullarg L a 
      in eapply
      end

  datatype ('e, 'l, 'f, 'p, 'v) attinit =
      INIT of { parameter: string -> 'p,
                    variable: string -> 'v,
                    exp: unit -> 'e,
                    args: unit -> 'l,
                    formals: unit -> 'f }
                    
  exception ParseError of string
  
  val keywords = ["lambda", "if", "set!", "quote"]
        
  fun dat2exp (INIT I) =
      let fun dat2e (PAIRDAT (a,d1,d2)) =
                             (case d1 of
                           SYMBDAT (a', "lambda") => dat2lambda (a,d2)
                             | SYMBDAT (a', "if") => dat2if (a,d2)
                             | SYMBDAT (a', "set!") => dat2assign (a,d2)
                                 | SYMBDAT (a', "quote") => dat2quote (a,d2)
                                 | _ => CALL (#exp I a, dat2e d1, dat2args d2))
                | dat2e (SYMBDAT (a,s)) = VARIABLE (#exp I a, #variable I s)
                | dat2e d = LITERAL (#exp I (dattrib d), d)
              and dat2lambda (al, (PAIRDAT (a, f, (PAIRDAT (a', e, NILDAT a''))))) =
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
                               then raise ParseError ("Illegal parameter (keyword) ")
                            else #parameter I s
                    | dat2par _ = raise ParseError ("Illegal parameter (not an identifier)")
          and dat2if (ai, (PAIRDAT (a, d1, PAIRDAT (a', d2, d3)))) =
                IF (#exp I ai, dat2e d1, dat2e d2, dat2else d3)
                | dat2if _ = raise ParseError ("Illegal if expression")
                  and dat2else (PAIRDAT (_, d, NILDAT _)) = dat2e d
                | dat2else (NILDAT a) = NOEXP (#exp I a)
                    | dat2else _ = raise ParseError ("Illegal if expression (else)")
                  and dat2assign (aa, (PAIRDAT (a,d1, PAIRDAT (a',d2, NILDAT a'')))) =
                         ASSIGN (#exp I aa, dat2var d1, dat2e d2)
            | dat2assign _ = raise ParseError ("Illegal assignment")
          and dat2quote (aq, PAIRDAT (a1,d1, NILDAT _)) =
                     LITERAL (#exp I aq, d1)
                | dat2quote _ = raise ParseError ("Illegal quotation")
                  and dat2var (SYMBDAT (a,s)) = 
                if member s keywords 
                   then ParseError ("Illegal variable (keyword)")
                else #variable I s
                    | dat2var _ = raise ParseError ("Illegal variable (not an identifier)")
                  and dat2args (PAIRDAT (a, d1, d2)) =
                PAIRARG (#args I a, dat2e d1, dat2args d2)
            | dat2args (NILDAT a) = NULLARG (#args I a)
                | dat2args _ = raise ParseError ("Illegal argument list")
      in
      dat2e
      end

  fun dinit (INIT {exp = x, ...}) = x
  
  fun read_exp I ip = dat2exp I (read_datum (dinit I) ip)

  end
end

