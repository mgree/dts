structure Eval: EVAL =
  struct
  open Conversion Error Object Pair Symbol Number Char String Vector IO 
       Datum Command Environment

  fun eval_datum (BOOLDAT b) = BOOL_TAG b
    | eval_datum (CHARDAT c) = CHAR_TAG (str2char c)
    | eval_datum (STRIDAT s) = STRING_TAG (FIXED s)
    | eval_datum (SYMBDAT s) = SYMBOL_TAG (string2symbol s)
    | eval_datum (NUMBDAT n) = NUMBER_TAG (str2number n)
    | eval_datum (VECTDAT dl) = VECTOR_TAG (vector (map eval_datum dl))
    | eval_datum (PAIRDAT (d1, d2)) = PAIR_TAG (cons (eval_datum d1, eval_datum d2))
    | eval_datum NILDAT = LIST_TAG nil

  val top_env = ref empty_env
  fun update_top_env b = (top_env := add (!top_env) b)

  fun get_value (v, env) = 
	lookup env v handle Lookup _ => lookup (!top_env) v 

  local val cstack = ref (nil: string list)
  in 
  fun init_cstack () = (cstack := nil)
  fun push_call v = (cstack := v :: !cstack)
  fun pop_call v =
	let val fst = hd (!cstack)
	in (cstack := tl (!cstack);
	    if fst = v then () else pop_call v)
        end
  fun write_cstack oport = 
	(output(oport, "\nCall stack:\n");
	 app (fn s => output(oport, s ^ "\n")) (!cstack))
  end

  fun eval_exp env (LITERAL d) = eval_datum d
    | eval_exp env (VARIABLE v) = get_value (v, env)
    | eval_exp env (CALL (VARIABLE v, a)) =
	(push_call v; 
	 let val dv = PROCEDURE_UNTAG (get_value (v, env)) (map (eval_exp env) a)
	 in (pop_call v; dv)
	 end)
    | eval_exp env (CALL (f, a)) = 
	PROCEDURE_UNTAG (eval_exp env f) (map (eval_exp env) a)
    | eval_exp env (LAMBDA (f, b)) = 
        PROCEDURE_TAG (fn arglist => 
	 let fun flength (PAIRPAR(_,fr)) = 1 + flength fr
	       | flength _ = 0
	     fun zip (NULLPAR, nil) = nil |
		 zip (PAIRPAR(fv,fr), av::ar) = (fv, av) :: zip (fr, ar) |
		 zip (VARPAR v, l) = [(v, LIST_TAG l)] |
                 zip _ = raise InputError ("Wrong number of arguments", 
					LIST_TAG arglist) 
         in eval_body (push env (zip (f, arglist))) b 
         end) 
    | eval_exp env (IF (e0, e1, e2)) = 
         if object2boolean (eval_exp env e0) 
	    then eval_exp env e1 
	 else eval_exp env e2
    | eval_exp env (ASSIGN (v, e)) = 
	 UNSPECIFIED_TAG((get_value (v, env) handle 
	   Lookup _ => let val unspec_value = UNSPECIFIED_TAG ()
		       in (update_top_env (v, unspec_value);
			   unspec_value)
		       end) := !(eval_exp env e))
    | eval_exp env (COND b) = 
         let fun eval_cond NULLCOND = UNSPECIFIED_TAG()
	       | eval_cond (CONDDEFAULT s) = eval_sequence env s
  	       | eval_cond (CONDCLAUSE (TESTSEQ (e1, e2), r)) =
		     if object2boolean (eval_exp env e1) 
			 then eval_sequence env e2
		     else eval_cond r
	       | eval_cond (CONDCLAUSE (TEST e, r)) = 
		     let val testvalue = eval_exp env e 
		     in if object2boolean testvalue 
			    then testvalue
			else eval_cond r
		     end
  	       | eval_cond (CONDCLAUSE (TESTREC (e0, e1), r)) =
  	             let val testvalue = eval_exp env e0 
		     in if object2boolean testvalue 
			    then PROCEDURE_UNTAG (eval_exp env e1) [testvalue]
			else eval_cond r
		     end
	 in eval_cond b
	 end
    | eval_exp env (CASE (e, cb)) =
          let val v = eval_exp env e 
	      fun eval_case NULLCASE = UNSPECIFIED_TAG()
		| eval_case (CASEDEFAULT s) = eval_sequence env s
		| eval_case (CASECLAUSE ((dl, es), r)) = 
		     let fun ev_c nil = eval_case r 
                           | ev_c (d::r) = if is_eqv (v, eval_datum d) 
					       then eval_sequence env es 
					   else ev_c r
		     in ev_c dl
		     end
	  in eval_case cb
	  end
     | eval_exp env (AND l) =
          let fun eval_and nil = BOOL_TAG true |
  	        eval_and [a] = eval_exp env a |
  		eval_and (a::r) = 
  		  let val v = eval_exp env a 
  		  in if object2boolean v 
  		     then eval_and r
  		     else v 
  		  end
          in eval_and l 
  	  end
     | eval_exp env (OR l) = 
          let fun eval_or nil = BOOL_TAG false |
  	        eval_or [a] = eval_exp env a |
  	        eval_or (a::r) = 
  		  let val x = eval_exp env a 
  		  in if object2boolean x 
  		     then x
  		     else eval_or r 
  		  end
          in eval_or l
  	  end 
     | eval_exp env (LET (l, b)) 
  	let val binds = map (fn (v,e) => (v, eval_exp env e)) l
  	in eval_body (push env binds) b 
          e
     | eval_exp env (NAMEDLET (itvar, bl, b)) =	eval_exp env 
	(LETREC ([(itvar, LAMBDA (fold (fn ((v,e),y) => PAIRPAR (v,y)) bl NULLPAR, b))], 
	         (nil, [CALL (VARIABLE itvar, map #2 bl)])))
     | eval_exp env (LETS (l, b)) =
          let fun eval_lets env nil = eval_body env b 
  	        | eval_lets env ((v,e)::r) 
			eval_lets (add env (v, eval_exp env e)) r 
          in eval_lets en
          end
     | eval_exp env (LETREC (l, b)) =
          let val newenv = push env (map (fn (v, e) => (v, UNSPECIFIED_TAG())) l)
          in (app (fn (v, e) => (get_value(v,newenv) := !(eval_exp newenv e))) l;
  	    eval_body newenv b
          e
     | eval_exp env (BEGIN s) = eval_sequence env s
     | eval_exp env (DO (l, (e, s), el)) 
          let val newenv = push env (map (fn (x,y,z) => (x, eval_exp env y)) l)
  	      fun iterate() = 
		if object2boolean (eval_exp newenv e
  		   then eval_sequence newenv s
  		else (app (eval_exp newenv) el; 
  		      app (fn (x,y,UNDEFEXP) => (
	                    | (x,y,z) => (get_value (x,newenv) :
					 !(eval_exp newenv z))) l;
	              iterate())
  	  in iterate() 
  	  end
     | eval_exp env (DELAY e) = PROCEDURE_TAG (fn _ => eval_exp env e) 
     | eval_exp env (QUASIQUOTE t) = 
        let fun append (nil, r) = 
              | append (l1::lr, r) = PAIR_TAG (l1, append (lr, r)
            fun eval_vector_list nil = n
	      | eval_vector_list (TEMPLATE t :: lr) 
		  eval_template t :: eval_vector_list lr
              | eval_vector_list (SPLICE e :: lr) 
		  LIST_UNTAG (eval_exp env e) @ eval_vector_list lr
	    and eval_template (SIMPLETEMP d) = eval_datum d
	      | eval_template (PAIRTEMP (TEMPLATE t1, t2)) = 
		  PAIR_TAG (eval_template t1, eval_template t2)
	      | eval_template (PAIRTEMP (SPLICE e, t)) =
		  append (LIST_UNTAG (eval_exp env e), eval_template t)
	      | eval_template (VECTTEMP tl) 
		  VECTOR_TAG (vector (eval_vector_list tl))
	      | eval_template (UNQUOTE e) = eval_exp env e
        in eval_template t
        end 
    | eval_exp env UNDEFEXP = UNSPECIFIED_TAG()
  and eval_sequence env nil = UNSPECIFIED_TAG()
    | eval_sequence env [e] = eval_exp env e
    | eval_sequence env (e1::er) = (eval_exp env e1; eval_sequence env er) 
  and eval_body env (nil, s) = eval_sequence env 
    | eval_body env (dl, s) =
          let fun get_vars (VARDEF (v,_):: rd) = v:: get_vars rd 
  	        get_vars (FUNDEF (v, _, _):: rd) = v:: get_vars rd 
  	        get_vars (BEGINDEF dl :: dl') = get_vars (dl @ dl') |
  		get_vars nil = nil
  	    val newenv = push env (map (fn v => (v, UNSPECIFIED_TAG()))
			           (get_vars dl
  	    fun eval_definition_par (VARDEF (v,e)) = 
                  (get_value(v,newenv) := !(eval_exp newenv e)
  	      | eval_definition_par (FUNDEF (v, f, b)) 
                  (get_value(v,newenv) := !(eval_exp newenv (LAMBDA (f,b)))
              | eval_definition_par (BEGINDEF dl) = app eval_definition_par d
          in (app eval_definition_par dl;
  	    eval_sequence newenv s)
  	end
  fun eval_definition (VARDEF (v,e)) 
	(top_env := add (!top_env) (v, eval_exp empty_env e)
    | eval_definition (FUNDEF (v, f, b)
           eval_definition (VARDEF (v, LAMBDA (f,b))
    | eval_definition (BEGINDEF dl) = app eval_definition dl
  fun eval (EXP e) = eval_exp empty_env 
    | eval (DEF d) = UNSPECIFIED_TAG (eval_definition d
  fun run (iport, oport) =
      let fun read_eval_loop () 
	     (output (oport, "> ");
	      write (eval (init_cstack(); parse (read_datum iport)), oport
		 hand
        	   ParseError (m, d) =>
	             (output(oport, m ^ ":\n"); write (eval_datum d, oport)
	         | TypeError (m, dv) => 
		     (output(oport, m ^ " expected, found ")
		      write (dv, oport)
	              write_cstack oport)
	         | InputError (m, dv) =
		     (output(oport, m ^ ": "); 
	              write (dv, oport
	              write_cstack oport
	         | IOError (m,s) => 
		     (output(oport, m ^": " ^ s);
		      write_cstack oport
	         | Io s => 
		     (output(oport, "I/O error: " ^s);
		      write_cstack oport
	         | Lookup s => 
		     (output(oport, "Lookup error: " ^ s);
		      write_cstack oport)
	         | Overflow => 
		     (output(oport, "Implementation restriction: Numeric overflow");
		      write_cstack oport
	         | Unimplemented s => output(oport, s ^ "not implemented");
	      output (oport, "\n");
	      read_eval_loop (
          val prev_input_port = current_input_port()
	  val prev_output_port = current_output_port()
      in (set_input_port iport;
	  set_output_port oport
	  read_eval_loop () handle EndOfFile => ()
          set_input_port prev_input_por
          set_output_port prev_output_port)
      end
  fun load s = run (open_in s, open_out "load.log")

  structure Basis: BASIS 
  struct
  open Conversion Error Object Pair SList Symbol Number Char String SVector 
       Control IO Environment

  (* Some coercions for embedding functions in dynamic/tagged types *
  fun wrong_number_arguments 
      raise InputError ("Wrong number of arguments", UNSPECIFIED_TAG())

  fun ID f = 
  fun FUNC0 h f = PROCEDURE_TAG 
	(fn nil => h (f ()
       	  | _ => wrong_number_arguments 0
  fun FUNC0L (g, h) f = PROCEDURE_TAG 
	(fn l => h (f (List.map g l)
  fun FUNC1 (g, h) f = PROCEDURE_T
        (fn [x] => h (f (g x)) |
            _  => wrong_number_arguments 1
  fun FUNC1L (g, h, i) f = PROCEDURE_TA
        (fn (x::l) => i (f (g x, List.map h l))
          | _ => wrong_number_arguments 
  fun FUNC2UT (g, h, i) f 
	(fn [x,y] => i (f (g x, h y))
          | _ => wrong_number_arguments 2)
  fun FUNC2 c f = PROCEDURE_TAG (FUNC2UT c f
  fun FUNC2L (g, h, i, j) f = PROCEDURE_TA
	(fn (x::y::l) => j (f (g x, h y, List.map i l)
          | _ => wrong_number_arguments 2)
  fun FUNC3 (g, h, i, j) f = PROCEDURE_TAG
	(fn [x,y,z] => j (f (g x, h y, i z))
	  | _ => wrong_number_arguments 3)
  fun FUNC1OR2 (g, h, i, d) f  = PROCEDURE_TAG
        (fn [y] => i (f (g d, h y))
	  | [x,y] => i (f (g x, h y))
	  | _ => wrong_number_arguments 2
  fun FUNC2OR1 (g, h, i, d) f = PROCEDURE_TAG  
	(fn [x] => i (f (g x, h d)
          | [x,y] => i (f (g x, h y))
          | _ => wrong_number_arguments 2
  fun FUNC1OR0 (h, i) (g, f) = PROCEDURE_TA
        (fn nil => i (f ()
          | [x] => i (g (h x))
          | _ => wrong_number_arguments 1)
  fun FUNC2ELSE1 (h, i, j) (g, f) = PROCEDURE_TA
	(fn [x] => j (f (h x))
          | [x,y] => j (g (h x, i y)
          | _ => wrong_number_arguments 2
  fun FUNC1UT f = (fn [x] => f x 
                      _ => wrong_number_arguments 1)

  fun CATCH f (Some x) = f 
    | CATCH f None = BOOL_TAG false 

  (* Bindings for all the essential standard procedures required by IEEE Scheme *
  val bindings = 
        ("boolean?", FUNC1 (ID, BOOL_TAG) is_boolean)
	("symbol?", FUNC1 (ID, BOOL_TAG) is_symbol)
	("char?", FUNC1 (ID, BOOL_TAG) is_char),
	("vector?", FUNC1 (ID, BOOL_TAG) is_vector),
	("pair?", FUNC1 (ID, BOOL_TAG) is_pair),
	("number?", FUNC1 (ID, BOOL_TAG) is_number),
	("string?", FUNC1 (ID, BOOL_TAG) is_string)
	("procedure?", FUNC1 (ID, BOOL_TAG) is_procedure)
	("list?", FUNC1 (ID, BOOL_TAG) is_list)
	("null?", FUNC1 (ID, BOOL_TAG) is_null)
	("input-port?", FUNC1 (ID, BOOL_TAG) is_input_port),
	("output-port?", FUNC1 (ID, BOOL_TAG) is_output_port),
	("eof-object?",  FUNC1 (ID, BOOL_TAG) is_eof_object)
	
	("not", FUNC1 (object2boolean, BOOL_TAG) not),

	("eqv?", FUNC2 (ID, ID, BOOL_TAG) is_eqv),
	("eq?",  FUNC2 (ID, ID, BOOL_TAG) is_eq),
	("equal?",  FUNC2 (ID, ID, BOOL_TAG) is_equal),

	("cons", FUNC2 (ID, ID, PAIR_TAG) cons),
        ("car", FUNC1 (PAIR_UNTAG, ID) car),
	("cdr", FUNC1 (PAIR_UNTAG, ID) cdr)
	("set-car!", FUNC2 (PAIR_UNTAG, ID, UNSPECIFIED_TAG) set_car), 
	("set_cdr!", FUNC2 (PAIR_UNTAG, ID, UNSPECIFIED_TAG) set_cdr
	("caar", FUNC1 (PAIR_UNTAG, ID) caar)
	("cadr", FUNC1 (PAIR_UNTAG, ID) cadr),
	("cdar", FUNC1 (PAIR_UNTAG, ID) cdar
	("cddr", FUNC1 (PAIR_UNTAG, ID) cddr)
	("caaar", FUNC1 (PAIR_UNTAG, ID) caaar)
	("caadr", FUNC1 (PAIR_UNTAG, ID) caadr),
	("cadar", FUNC1 (PAIR_UNTAG, ID) cadar),
	("caddr", FUNC1 (PAIR_UNTAG, ID) caddr)
	("cdaar", FUNC1 (PAIR_UNTAG, ID) cdaar),
	("cdadr", FUNC1 (PAIR_UNTAG, ID) cdadr)
	("cddar", FUNC1 (PAIR_UNTAG, ID) cddar),
	("cdddr", FUNC1 (PAIR_UNTAG, ID) cdddr)
	("caaaar", FUNC1 (PAIR_UNTAG, ID) caaaar)
	("caaadr", FUNC1 (PAIR_UNTAG, ID) caaadr),
	("caadar", FUNC1 (PAIR_UNTAG, ID) caadar),
	("caaddr", FUNC1 (PAIR_UNTAG, ID) caaddr),
	("cadaar", FUNC1 (PAIR_UNTAG, ID) cadaar),
	("cadadr", FUNC1 (PAIR_UNTAG, ID) cadadr),
	("caddar", FUNC1 (PAIR_UNTAG, ID) caddar),
	("cadddr", FUNC1 (PAIR_UNTAG, ID) cadddr),
	("cdaaar", FUNC1 (PAIR_UNTAG, ID) cdaaar
	("cdaadr", FUNC1 (PAIR_UNTAG, ID) cdaadr
	("cdadar", FUNC1 (PAIR_UNTAG, ID) cdadar),
	("cdaddr", FUNC1 (PAIR_UNTAG, ID) cdaddr),
	("cddaar", FUNC1 (PAIR_UNTAG, ID) cddaar),
	("cddadr", FUNC1 (PAIR_UNTAG, ID) cddadr)
	("cdddar", FUNC1 (PAIR_UNTAG, ID) cdddar),
	("cddddr", FUNC1 (PAIR_UNTAG, ID) cddddr),
	
	("list", PROCEDURE_TAG list),
	("length", FUNC1 (LIST_UNTAG, NUMBER_TAG) length)
	("append", PROCEDURE_TAG append)
	("reverse", FUNC1 (LIST_UNTAG, LIST_TAG) reverse)
	("list-ref", FUNC2 (LIST_UNTAG, NUMBER_UNTAG, ID) list_ref)
	("memq", FUNC2 (ID, LIST_UNTAG, (fn nil => BOOL_TAG false | 
	         l => LIST_TAG l)) memq)
	("memv", FUNC2 (ID, LIST_UNTAG, (fn nil => BOOL_TAG false | 
	         l => LIST_TAG l)) memv),
	("member", FUNC2 (ID, LIST_UNTAG, (fn nil => BOOL_TAG false 
	         l => LIST_TAG l)) member),
	("assq", FUNC2 (ID, List.map PAIR_UNTAG o LIST_UNTAG, CATCH PAIR_TAG
		 assq)
	("assv", FUNC2 (ID, List.map PAIR_UNTAG o LIST_UNTAG, CATCH PAIR_TAG
		 assv)
	("assoc", FUNC2 (ID, List.map PAIR_UNTAG o LIST_UNTAG, CATCH PAIR_TAG)
		 assoc),
		
	("symbol->string", FUNC1 (SYMBOL_UNTAG, STRING_TAG o FIXED)
		symbol2string)
	("string->symbol", FUNC1 (mstring2string o STRING_UNTAG, SYMBOL_TAG
		string2symbol),
	("complex?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_complex)
	("real?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_real)
	("rational?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_rational),
	("integer?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_integer),
	("exact?", FUNC1 (COMPLEX_UNTAG, BOOL_TAG) is_exact)
	("inexact?", FUNC1 (COMPLEX_UNTAG, BOOL_TAG) is_inexact)
	("=", FUNC2L (COMPLEX_UNTAG, COMPLEX_UNTAG, COMPLEX_UNTAG, BOOL_TAG) eq)
	("<", FUNC2L (REAL_UNTAG, REAL_UNTAG, REAL_UNTAG, BOOL_TAG) lt),
	(">", FUNC2L (REAL_UNTAG, REAL_UNTAG, REAL_UNTAG, BOOL_TAG) gt)
	("<=", FUNC2L (REAL_UNTAG, REAL_UNTAG, REAL_UNTAG, BOOL_TAG) le)
	(">=", FUNC2L (REAL_UNTAG, REAL_UNTAG, REAL_UNTAG, BOOL_TAG) ge)
	("zero?", FUNC1 (COMPLEX_UNTAG, BOOL_TAG) is_zero
	("positive?", FUNC1 (REAL_UNTAG, BOOL_TAG) is_positive)
	("negative?", FUNC1 (REAL_UNTAG, BOOL_TAG) is_negative),
	("odd?", FUNC1 (INTEGER_UNTAG, BOOL_TAG) is_odd),
	("even?", FUNC1 (INTEGER_UNTAG, BOOL_TAG) is_even),
	("max", FUNC1L (REAL_UNTAG, REAL_UNTAG, REAL_TAG) max)
	("min", FUNC1L (REAL_UNTAG, REAL_UNTAG, REAL_TAG) min),
	("+", FUNC0L (COMPLEX_UNTAG, COMPLEX_TAG) plus),
	("*", FUNC0L (COMPLEX_UNTAG, COMPLEX_TAG) mult)
	("-", FUNC1OR2 (COMPLEX_UNTAG, COMPLEX_UNTAG, COMPLEX_TAG, 
	      NUMBER_TAG 0) minus)
	("/", FUNC1OR2 (COMPLEX_UNTAG, COMPLEX_UNTAG, COMPLEX_TAG, 
	      NUMBER_TAG 1) divide),
	("abs", FUNC1 (REAL_UNTAG, REAL_TAG) abs),
	("quotient", FUNC2 (INTEGER_UNTAG, INTEGER_UNTAG, INTEGER_TAG) quotient)
	("remainder", FUNC2 (INTEGER_UNTAG, INTEGER_UNTAG, INTEGER_TAG) remainder),
	("modulo", FUNC2 (INTEGER_UNTAG, INTEGER_UNTAG, INTEGER_TAG) modulo)
	("gcd", FUNC0L (INTEGER_UNTAG, INTEGER_TAG) gcd),
	("lcm", FUNC0L (INTEGER_UNTAG, INTEGER_TAG) lcm),
        ("numerator", FUNC1 (RATIONAL_UNTAG, INTEGER_TAG) numerator)
        ("denominator", FUNC1 (RATIONAL_UNTAG, NATURAL_TAG) denominator),
	("floor", FUNC1 (REAL_UNTAG, INTEGER_TAG) floor),
	("ceiling", FUNC1 (REAL_UNTAG, INTEGER_TAG) ceiling),
	("truncate", FUNC1 (REAL_UNTAG, INTEGER_TAG) truncate),
	("round",  FUNC1 (REAL_UNTAG, INTEGER_TAG) round)
        ("rationalize", FUNC2 (REAL_UNTAG, REAL_UNTAG, RATIONAL_TAG) rationalize)
        ("exp", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) exp),
        ("log", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) log)
        ("sin", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) sin),
        ("cos", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) cos)
        ("tan", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) tan),
        ("asin", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) asin)
        ("acos", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) acos),
        ("atan", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) atan),
        ("atanr", FUNC2 (REAL_UNTAG, REAL_UNTAG, REAL_TAG) atanr),
        ("sqrt", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) sqrt),
        ("expt", FUNC2 (COMPLEX_UNTAG, COMPLEX_UNTAG, COMPLEX_TAG) expt),
        ("make-rectangular", FUNC2 (REAL_UNTAG, REAL_UNTAG, COMPLEX_TAG) 
		make_rectangular),
        ("make-polar", FUNC2 (REAL_UNTAG, REAL_UNTAG, COMPLEX_TAG) make_polar)
        ("real-part", FUNC1 (COMPLEX_UNTAG, REAL_TAG) real_part),
        ("imag-part", FUNC1 (COMPLEX_UNTAG, REAL_TAG) imag_part)
        ("magnitude", FUNC1 (COMPLEX_UNTAG, REAL_TAG) imag_part)
        ("angle", FUNC1 (COMPLEX_UNTAG, REAL_TAG) imag_part),
        ("exact->inexact", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) exact2inexact)
        ("inexact->exact", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) inexact2exact),
	("number->string", FUNC2OR1 (NUMBER_UNTAG, NATURAL2RADIX o 
    	  NATURAL_UNTAG, STRING_TAG o string2mstring, NUMBER_TAG 10) number2string)
	("string->number", FUNC2OR1 (mstring2string o STRING_UNTAG
		NATURAL2RADIX o NATURAL_UNTAG, CATCH NUMBER_TAG, NUMBER_TAG 10
		string2number),
	
	("char=?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_eq),
	("char<?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_lt)
	("char>?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_gt)
	("char<=?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_le),
	("char>=?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_ge),
	("char-ci=?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_ci_eq),
	("char-ci<?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_ci_lt)
	("char-ci>?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_ci_gt),
	("char-ci<=?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_ci_le),
	("char-ci>=?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_ci_ge)
	("char-alphabetic?", FUNC1 (CHAR_UNTAG, BOOL_TAG) is_char_alphabetic)
	("char-numeric?", FUNC1 (CHAR_UNTAG, BOOL_TAG) is_char_numeric),
	("char-whitespace?", FUNC1 (CHAR_UNTAG, BOOL_TAG) is_char_whitespace),
	("char-upper-case?", FUNC1 (CHAR_UNTAG, BOOL_TAG) is_char_upper_case),
	("char-lower-case?", FUNC1 (CHAR_UNTAG, BOOL_TAG) is_char_lower_case),
	("char->integer", FUNC1 (CHAR_UNTAG, INTEGER_TAG) char2integer),
	("integer->char", FUNC1 (INTEGER_UNTAG, CHAR_TAG) integer2char)
	("char-upcase", FUNC1 (CHAR_UNTAG, CHAR_TAG) char_upcase),
	("char-downcase", FUNC1 (CHAR_UNTAG, CHAR_TAG) char_downcase)
	("make-string", FUNC2OR1 (NATURAL_UNTAG, CHAR_UNTAG, STRING_TAG, 
		        CHAR_TAG unspec_char) make_string),
	("string", FUNC0L (CHAR_UNTAG, STRING_TAG) string),
	("string-length", FUNC1 (STRING_UNTAG, NATURAL_TAG) string_length),
	("string-ref", FUNC2 (STRING_UNTAG, NATURAL_UNTAG, CHAR_TAG) string_ref),
	("string-set!", FUNC3 (STRING_UNTAG, NATURAL_UNTAG, CHAR_UNTAG, 
		        UNSPECIFIED_TAG) string_set),
	("string=?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_eq),
	("string-ci=?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_ci_eq)
	("string<?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_lt),
	("string>?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_gt),
	("string<=?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_le),
	("string>=?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_ge),
	("string-ci<?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_ci_lt),
	("string-ci>?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_ci_gt),
	("string-ci<=?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_ci_le),
	("string-ci>=?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_ci_ge),
	("substring", FUNC3 (STRING_UNTAG, NATURAL_UNTAG, NATURAL_UNTAG, 
		      STRING_TAG) substring),
	("string-append", FUNC0L (STRING_UNTAG, STRING_TAG) string_append)
	("make-vector", FUNC2OR1 (NATURAL_UNTAG, ID, VECTOR_TAG, 
		        UNSPECIFIED_TAG()) make_vector),
	("vector", FUNC0L (ID, VECTOR_TAG) vector),
	("vector-length", FUNC1 (VECTOR_UNTAG, NATURAL_TAG) vector_length),
	("vector-ref", FUNC2 (VECTOR_UNTAG, NATURAL_UNTAG, ID) vector_ref)
	("vector-set!", FUNC3 (VECTOR_UNTAG, NATURAL_UNTAG, ID, 
	                UNSPECIFIED_TAG) vector_set),

	("apply", FUNC2L (PROCEDURE_UNTAG, ID, ID, ID) apply)
	("map", FUNC2L (PROCEDURE_UNTAG, LIST_UNTAG, LIST_UNTAG, LIST_TAG) map),
	("for-each", FUNC2L (PROCEDURE_UNTAG, LIST_UNTAG, LIST_UNTAG, 
		     UNSPECIFIED_TAG) for_each),
	("call-with-current-continuation", FUNC1 (PROCEDURE_UNTAG, ID
		     call_with_current_continuation
        ("call-with-input-file", FUNC2 (mstring2string o STRING_UNTAG, 
		     PROCEDURE_UNTAG, ID) call_with_input_file)
	("call-with-output-file", FUNC2 (mstring2string o STRING_UNTAG, 
		     PROCEDURE_UNTAG, ID) call_with_output_file),
	("current-input-port", FUNC0 INPUT_PORT_TAG current_input_port),
	("current-output-port", FUNC0 OUTPUT_PORT_TAG current_output_port),
	("open-input-file", FUNC1 (mstring2string o STRING_UNTAG, 
		     INPUT_PORT_TAG) open_input_file),
	("open-output-file", FUNC1 (mstring2string o STRING_UNTAG
		     OUTPUT_PORT_TAG) open_output_file),
	("close-input-port", FUNC1 (INPUT_PORT_UNTAG, UNSPECIFIED_TAG
		     close_input_port),
	("close-output-port", FUNC1 (OUTPUT_PORT_UNTAG, UNSPECIFIED_TAG)
		     close_output_port),
	("read-char", FUNC1OR0 (INPUT_PORT_UNTAG, CHAR_TAG) 
			(read_char, read_char_current)),
	("peek-char", FUNC1OR0 (INPUT_PORT_UNTAG, CHAR_TAG) 
		        (peek_char, peek_char_current)),
	("read", FUNC1OR0 (INPUT_PORT_UNTAG, ID) (read, read_current)), 
	("newline", FUNC1OR0 (OUTPUT_PORT_UNTAG, UNSPECIFIED_TAG) 
		    (newline, newline_current)),
	("write-char", FUNC2ELSE1 (CHAR_UNTAG, OUTPUT_PORT_UNTAG, 
		       UNSPECIFIED_TAG) (write_char, write_char_current)),
	("write", FUNC2ELSE1 (ID, OUTPUT_PORT_UNTAG, UNSPECIFIED_TAG) 
		  (write, write_current)),
	("display", FUNC2ELSE1 (ID, OUTPUT_PORT_UNTAG, UNSPECIFIED_TAG) 
		  (display, display_current)),
        ("exit", PROCEDURE_TAG (fn _ => raise EndOfFile)),
        ("load", FUNC1 (mstring2string o STRING_UNTAG, UNSPECIFIED_TAG) load)
	]
  end
  
  val base_env = push empty_env Basis.bindings
  val _ = (top_env := base_env)
  fun init_env() = (top_env := base_env)

  en
 