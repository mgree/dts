structure KernelExp: KERNELEXP =
  struct 
  local open SchemeDatum 
  in

  datatype ('a, 'd, 'e, 'f, 'l, 'v) exp_hom =
    EHOM of { noexp: 'a -> 'e,
              literal: 'a -> 'd -> 'e,
              variable: 'a -> 'v -> 'e,
              call: 'a -> 'e * 'l -> 'e,
              lambda: 'a -> 'f * 'e -> 'e,
              ifexp: 'a -> 'e * 'e * 'e -> 'e,
              assign: 'a -> 'v * 'e -> 'e,
              pairarg: 'a -> 'e * 'l -> 'l,
              nullarg: 'a -> 'l,
              avarpar: 'a -> 'v -> 'f,
              apairpar: 'a -> 'v * 'f -> 'f,
              anullpar: 'a -> 'f
            }

  type 'a anndatum = 'a anndatum

  datatype 'b variable =
    VAR of string * 'b

  datatype ('a, 'b) exp =
    NOEXP |
    LITERAL of 'a anndatum |
    VARIABLE of 'b variable |
    CALL of ('a, 'b) annexp * ('a, 'b) annargs |
    LAMBDA of ('a, 'b) annformals * ('a, 'b) annexp |
    IF of ('a, 'b) annexp * ('a, 'b) annexp * ('a, 'b) annexp |  
    ASSIGN of 'b variable * ('a, 'b) annexp

  and ('a, 'b) annexp =
    EXP of ('a, 'b) exp * 'a

  and ('a, 'b) args =
    PAIRARG of ('a, 'b) annexp * ('a, 'b) annargs |
    NULLARG 

  and ('a, 'b) annargs =
    ARGS of ('a, 'b) args * 'a

  and ('a, 'b) formals =
    AVARPAR of 'b variable |
    APAIRPAR of 'b variable * ('a, 'b) annformals |
    ANULLPAR

  and ('a, 'b) annformals =
    FORMALS of ('a, 'b) formals * 'a

  fun apply_ehom (EHOM H: ('a, 'a anndatum, 'e, 'f, 'l, 'b variable) exp_hom) 
  		 (e: ('a, 'b) annexp): 'e =
      let fun eapply (EXP (e, a)) =
              case e of
	        NOEXP => #noexp H a
              | LITERAL ad => #literal H a ad
              | VARIABLE v => #variable H a v
              | CALL (e, l) => #call H a (eapply e, aapply l)
              | LAMBDA (f, e) => #lambda H a (fapply f, eapply e)
              | IF (e1, e2, e3) => 
	      	  #ifexp H a (eapply e1, eapply e2, eapply e3)
              | ASSIGN (v, e) => #assign H a (v, eapply e)
          and aapply (ARGS (l, a)) =
	      case l of
	        PAIRARG (e, l') => #pairarg H a (eapply e, aapply l')
              | NULLARG => #nullarg H a
          and fapply (FORMALS (f, a)) =
	      case f of
	        AVARPAR v => #avarpar H a v
	      | APAIRPAR (v, f') => #apairpar H a (v, fapply f')
	      | ANULLPAR => #anullpar H a
       in eapply e
       end

  exception ParseError of string

  fun dat2exp (ainit: unit -> 'a, vinit: unit -> 'b) (d: 'a anndatum): 
  	('a, 'b) annexp =
      let fun make_exp EC x = EXP (EC x, ainit())
          fun make_noexp () = EXP (NOEXP, ainit())
          fun make_pairarg x = ARGS (PAIRARG x, ainit())
	  fun make_nullarg () = ARGS (NULLARG, ainit())
	  fun make_formals FC x = FORMALS (FC x, ainit())
	  fun make_nullpar () = FORMALS (ANULLPAR, ainit())
	  fun make_var s = VAR (s, vinit ())
          fun dat2e (ad as DATUM (d, a)) =
	      case d of
	        PAIRDAT (d1 as DATUM (d', a'), d2) =>
		  (case d' of
		    SYMBDAT "lambda" => dat2lambda d2
		  | SYMBDAT "if" => dat2if d2
		  | SYMBDAT "set!" => dat2assign d2
		  | SYMBDAT "quote" => dat2quote d2
		  | _ => make_exp CALL (dat2e d1, dat2args d2))
	      | SYMBDAT s => make_exp VARIABLE (make_var s)
	      | _ => make_exp LITERAL ad
	  and dat2lambda (DATUM (PAIRDAT (f, DATUM (PAIRDAT (e, 
	  		  DATUM (NILDAT, _)), _)), _)) =
		make_exp LAMBDA (dat2formals f, dat2e e)
	    | dat2lambda _ = raise ParseError ("Illegal lambda expression")
	  and dat2formals (DATUM (SYMBDAT s, _)) =
		make_formals AVARPAR (make_var s)
	    | dat2formals (DATUM (PAIRDAT (d1, d2), _)) = 
	        make_formals APAIRPAR (dat2var d1, dat2formals d2)
	    | dat2formals (DATUM (NILDAT, _)) = make_nullpar ()
	    | dat2formals _ = raise ParseError ("Illegal formals")
	  and dat2var (DATUM (SYMBDAT s, _)) = 
	  	(case s of
		  "lambda" => raise ParseError ("Illegal variable (lambda)")
		| "if" => raise ParseError ("Illegal variable (if)")
		| "set!" => raise ParseError ("Illegal variable (set!)")
		| _ => make_var s)
	    | dat2var _ = raise ParseError 
	    			("Illegal variable (not an identifier)")
          and dat2if (DATUM (PAIRDAT (d1, DATUM (PAIRDAT (d2, d3), _)), _)) =
                make_exp IF (dat2e d1, dat2e d2, dat2else d3)
	    | dat2if _ = raise ParseError ("Illegal if expression")
	  and dat2else (DATUM (PAIRDAT (d, DATUM (NILDAT, _)), _)) = dat2e d
	    | dat2else (DATUM (NILDAT, _)) = make_noexp ()
	    | dat2else _ = raise ParseError ("Illegal if expression (else)")
	  and dat2assign (DATUM (PAIRDAT (d1, DATUM (PAIRDAT (d2,
	  		  DATUM (NILDAT, _)), _)), _)) =
	        make_exp ASSIGN (dat2var d1, dat2e d2)
            | dat2assign _ = raise ParseError ("Illegal assignment")
          and dat2quote (DATUM (PAIRDAT (d1, DATUM (NILDAT, _)), _)) =
	       make_exp LITERAL d1
	    | dat2quote _ = raise ParseError ("Illegal quotation")
	  and dat2args (DATUM (PAIRDAT (d1, d2), _)) =
                make_pairarg (dat2e d1, dat2args d2)
            | dat2args (DATUM (NILDAT, _)) = make_nullarg ()
	    | dat2args _ = raise ParseError ("Illegal argument list")
      in
      dat2e d
      end

  fun read_exp (ainit: unit -> 'a, vinit: unit -> 'b) (ip: instream):
   	          ('a, 'b) annexp =
      dat2exp (ainit, vinit) (read_datum ainit ip)

  end
end


