structure Command: COMMAND 
  struct
  open Datum Error

  fun is_keyword "quote" = tr
    | is_keyword "lambda" = true
    | is_keyword "if" = true
    | is_keyword "set!" = tr
    | is_keyword "begin" = tru
    | is_keyword "cond" = tr
    | is_keyword "and" = tr
    | is_keyword "or" = tru
    | is_keyword "case" = true
    | is_keyword "let" = tru
    | is_keyword "let*" = tru
    | is_keyword "letrec" = tr
    | is_keyword "do" = true
    | is_keyword "quasiquote" = tru
    | is_keyword "else" = tr
    | is_keyword "=>" = true
    | is_keyword "define" = true
    | is_keyword "unquote" = true
    | is_keyword "unquote-splicing" = true
    | is_keyword _ = false

  fun parse_exp (d as (PAIRDAT (d1, d2))) =
      (case d1 o
	SYMBDAT "quote" => parse_quote (d, d2
      |	SYMBDAT "lambda" => parse_lambda (d, d2
      | SYMBDAT "if" => parse_if (d, d2)
      | SYMBDAT "set!" => parse_assign (d, d2
      | SYMBDAT "begin" =
	  (case parse_begin (d, d2) 
	     DEF def => raise ParseError ("Illegal begin expression", d)
	   | EXP exp => ex
      | SYMBDAT "cond" => 
	  (case parse_cond_clauses d2 o
	     NULLCOND => raise ParseError ("Illegal cond expression", d
	   | ccb => COND ccb
      | SYMBDAT "and" => AND (parse_exps d2
      | SYMBDAT "or" => OR (parse_exps d2)
      | SYMBDAT "case" => parse_case (d, d2)
      | SYMBDAT "let" => parse_let (d, d2)
      | SYMBDAT "let*" => parse_lets (d, d2
      | SYMBDAT "letrec" => parse_letrec (d, d
      | SYMBDAT "do" => parse_do (d, d2
      | SYMBDAT "quasiquote" => parse_quasiquote (d, d2
      | SYMBDAT "else" => raise ParseError ("Illegal expression", d
      | SYMBDAT "=>" => raise ParseError ("Illegal expression", d
      | SYMBDAT "define" => raise ParseError ("Illegal expression", d
      | SYMBDAT "unquote" => 
	  raise ParseError ("Illegal expression (not inside quotation)", d)
      | SYMBDAT "unquote-splicing" 
	  raise ParseError ("Illegal expression (not inside quotation)", d
      | SYMBDAT s => CALL (VARIABLE s, parse_exps d2
      | _ => CALL (parse_exp d1, parse_exps d2)
    | parse_exp (SYMBDAT s) = VARIABLE (parse_identifier 
    | parse_exp (d as (VECTDAT v)) = raise ParseError ("Illegal vector datum", d
    | parse_exp NILDAT = raise ParseError ("Illegal empty list datum", NILDAT
    | parse_exp d = LITERA
  and parse_quote (d, PAIRDAT (d', NILDAT)) = LITERAL d
    | parse_quote (d, _) = raise ParseError ("Illegal quotation", 
  and parse_lambda (d, PAIRDAT (f, b)) 
        LAMBDA (parse_formals f, parse_body b
    | parse_lambda (d, _) = raise ParseError ("Illegal lambda expression", 
  and parse_formals (SYMBDAT s) = VARPAR (parse_identifier s)
    | parse_formals (PAIRDAT (SYMBDAT s, d)
        PAIRPAR (parse_identifier s, parse_formals d
    | parse_formals NILDAT = NULLP
    | parse_formals d = raise ParseError ("Illegal formal parameters", d)
  and parse_identifier s =
        if is_keyword 
           then raise ParseError ("Illegal identifier (keyword)", SYMBDAT 
        els
  and parse_body (PAIRDAT (d1, d2)) 
	(case parse_command d1 o
	  DEF def => let val (defs, exps) = parse_body 
		     in (def::defs, exps)
                     e
        | EXP exp => (nil, exp :: parse_exps d2
    | parse_body d = raise ParseError ("Illegal body (part)", d
  and parse_command (d as (PAIRDAT (SYMBDAT "define", d'))
	DEF (parse_define (d, d'))
    | parse_command (d as (PAIRDAT (SYMBDAT "begin", d'))) = parse_begin (d, d')
    | parse_command d = EXP (parse_exp 
  and parse_begin (d, NILDAT) = DEF (BEGINDEF nil)
    | parse_begin (d, PAIRDAT (d1, d2)
	(case parse_command d1 o
	  DEF def => DEF (BEGINDEF (def :: parse_defs d2))
        | EXP exp => EXP (BEGIN (exp :: parse_exps d2)
    | parse_begin (d, _) = 
	raise ParseError ("Illegal begin expression/definition", d
  and parse_define (d, PAIRDAT (SYMBDAT s, PAIRDAT (d', NILDAT))
	VARDEF (parse_identifier s, parse_exp d')
    | parse_define (d, PAIRDAT (PAIRDAT (SYMBDAT s, f), b)) 
	FUNDEF (parse_identifier s, parse_def_formals f, parse_body 
    | parse_define (d, _) = raise ParseError ("Illegal definition", d)
  and parse_def_formals f =
        let val formals = parse_formals f
        in case formals o
	     VARPAR _ => raise ParseError ("Illegal def_formals", f
           | _ => formals
        end
  and parse_defs (PAIRDAT (d1, d2)) = 
	(case parse_command d1 of
	  DEF def => def :: parse_defs d2
        | EXP _ => raise ParseError ("Illegal definition", d1))
    | parse_defs NILDAT = ni
    | parse_defs d = raise ParseError ("Illegal definitions", d)
  and parse_exps NILDAT = ni
    | parse_exps d = parse_seq 
  and parse_if (d, PAIRDAT (d1, PAIRDAT (d2, d3))) =
        IF (parse_exp d1, parse_exp d2, parse_else d3
    | parse_if (d, _) = raise ParseError ("Illegal if expression", d)
  and parse_else (PAIRDAT (d, NILDAT)) = parse_exp d
    | parse_else NILDAT = UNDEFEXP
    | parse_else d = raise ParseError ("Illegal else branch", d)
  and parse_assign (d, PAIRDAT (SYMBDAT s, PAIRDAT (d2, NILDAT))) 
        ASSIGN (parse_identifier s, parse_exp d2
    | parse_assign (d, _) = raise ParseError ("Illegal assignment", d)
  and parse_cond_clauses NILDAT = NULLCON
    | parse_cond_clauses (PAIRDAT (PAIRDAT (SYMBDAT "else", seq), NILDAT)) 
	CONDDEFAULT (parse_seq seq)
    | parse_cond_clauses (PAIRDAT (cc, rc)) =
	CONDCLAUSE (parse_cond_clause cc, parse_cond_clauses rc
    | parse_cond_clauses d = raise ParseError ("Illegal cond clauses", d)
  and parse_cond_clause (PAIRDAT (t, NILDAT)) = TEST (parse_exp t)
    | parse_cond_clause (PAIRDAT (t, PAIRDAT(SYMBDAT "=>", PAIRDAT(r, NILDAT)))) =
	TESTREC (parse_exp t, parse_exp r)
    | parse_cond_clause (PAIRDAT (t, seq)) = TESTSEQ (parse_exp t, parse_seq seq
    | parse_cond_clause d = raise ParseError ("Illegal cond clause", d)
  and parse_seq (PAIRDAT (d1, d2)) = parse_exp d1 :: parse_exps d2
    | parse_seq d = raise ParseError ("Illegal sequence", d
  and parse_case (d, PAIRDAT (d1, d2)) =
	(case parse_case_clauses d2 of
	   NULLCASE => raise ParseError ("Illegal case expression (empty)", d)
	 | cc => CASE (parse_exp d1, cc)
    | parse_case (d, _) = raise ParseError ("Illegal case expression", d
  and parse_case_clauses NILDAT = NULLCA
    | parse_case_clauses (PAIRDAT (PAIRDAT (SYMBDAT "else", seq), NILDAT)) 
	CASEDEFAULT (parse_seq seq)
    | parse_case_clauses (PAIRDAT (cc, rc)) = 
	CASECLAUSE (parse_case_clause cc, parse_case_clauses rc)
    | parse_case_clauses d = raise ParseError ("Illegal case clauses", d)
  and parse_case_clause (PAIRDAT (d1, d2)) = 
	(parse_datum_list d1, parse_seq d2)
    | parse_case_clause d = raise ParseError ("Illegal case clause", d)
  and parse_datum_list NILDAT = nil
    | parse_datum_list (PAIRDAT (d1, d2)) = d1 :: parse_datum_list d2
    | parse_datum_list d = raise ParseError ("Illegal datum (sub)list", d)
  and parse_let (d, PAIRDAT (SYMBDAT s, PAIRDAT (d1, d2))) 
	NAMEDLET (parse_identifier s, parse_binding_specs d1, parse_body d2
    | parse_let (d, PAIRDAT (d1, d2)) 
	LET (parse_binding_specs d1, parse_body d2)
    | parse_let (d, _) = raise ParseError ("Illegal let expression", d)
  and parse_binding_specs NILDAT = nil
    | parse_binding_specs (PAIRDAT (d1, d2)) 
	parse_binding_spec d1 :: parse_binding_specs d2
    | parse_binding_specs d = raise ParseError ("Illegal binding specs", d
  and parse_binding_spec (PAIRDAT (SYMBDAT s, PAIRDAT (d, NILDAT))) =
	(parse_identifier s, parse_exp d
    | parse_binding_spec d = raise ParseError ("Illegal binding spec", d
  and parse_lets (d, PAIRDAT (d1, d2)) =
	LETS (parse_binding_specs d1, parse_body d2)
    | parse_lets (d, _) = raise ParseError ("Illegal let* expression", d)
  and parse_letrec (d, PAIRDAT (d1, d2)) 
	LETREC (parse_binding_specs d1, parse_body d2
    | parse_letrec (d, _) = raise ParseError ("Illegal letrec expression", d)
  and parse_do (d, PAIRDAT (d1, PAIRDAT (d2, d3))) =
	DO (parse_iter_specs d1, parse_iter_test d2, parse_exps d3)
    | parse_do (d, _) = raise ParseError ("Illegal do expression", d
  and parse_iter_specs NILDAT = nil
    | parse_iter_specs (PAIRDAT (d1, d2)
	parse_iter_spec d1 :: parse_iter_specs d
    | parse_iter_specs d = raise ParseError ("Illegal iteration specs", d
  and parse_iter_spec (PAIRDAT (SYMBDAT s, PAIRDAT (i, PAIRDAT (st, NILDAT)))) =
	(parse_identifier s, parse_exp i, parse_exp st
    | parse_iter_spec (PAIRDAT (SYMBDAT s, PAIRDAT (i, NILDAT))) =
	(parse_identifier s, parse_exp i, UNDEFEXP
    | parse_iter_spec d = raise ParseError ("Illegal iteration spec", d
  and parse_iter_test (PAIRDAT (d1, d2)) = (parse_exp d1, parse_seq d2)
    | parse_iter_test d = raise ParseError ("Illegal iteration test/sequence", d
  and parse_quasiquote (d, PAIRDAT (d', NILDAT)) = 
	QUASIQUOTE (parse_template (1, d'))
    | parse_quasiquote (d, _) = raise ParseError ("Illegal quasiquotation", d)
  and parse_template (N, PAIRDAT(d as (SYMBDAT "quasiquote"), PAIRDAT(d',NILDAT)))
	PAIRTEMP (TEMPLATE (SIMPLETEMP d), parse_template (N+1, d'))
    | parse_template (1, PAIRDAT (SYMBDAT "unquote", PAIRDAT (d', NILDAT))) =
	UNQUOTE (parse_exp d'
    | parse_template (N, PAIRDAT(d as (SYMBDAT "unquote"), PAIRDAT(d', NILDAT))) =
       PAIRTEMP (TEMPLATE (SIMPLETEMP d), 
	         PAIRTEMP (parse_template_or_splice (N-1) d', SIMPLETEMP NILDAT)
    | parse_template (N, PAIRDAT(d, d')) =
	PAIRTEMP (parse_template_or_splice N d, parse_template (N, d'))
    | parse_template (N, VECTDAT dl) = VECTTEMP (map (parse_template_or_splice N) dl
    | parse_template (N, d) = SIMPLETEMP d
  and parse_template_or_splice 
	(PAIRDAT (SYMBDAT "unquote-splicing", PAIRDAT (d', NILDAT))) =
	SPLICE (parse_exp d')
    | parse_template_or_splice N
	(PAIRDAT (d as (SYMBDAT "unquote-splicing"), PAIRDAT (d', NILDAT))) =
	TEMPLATE (PAIRTEMP (TEMPLATE (SIMPLETEMP d), parse_template (N-1, d')))
    | parse_template_or_splice N d = TEMPLATE (parse_template (N, d)
	
  val parse = parse_command
  end
