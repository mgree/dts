(*$SCHEMEEVAL *)

signature SCHEMEEVAL =
sig

(* EVAL

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Scheme abstract syntax evaluation

*)

type dynamic
type env
type expression
type formals
type definition
type command_or_definition

exception WrongNumberArguments of int 

val eval_exp: env -> expression -> dynamic
val eval_definition: definition -> unit
val eval_command_or_definition: command_or_definition -> dynamic
val SmlScheme : unit -> unit

end


(*$SchemeEval: SCHEMEEVAL SchemeGeneral SchemeBool SchemeChar SchemeNumber 
               SchemeString SchemeSymbol SchemeList SchemePair 
               SchemeProcedure SchemeVector SchemeInputOutput 
               SchemeEnvironment SchemeAst SchemeDynamic *)

structure SchemeEval: SCHEMEEVAL =
  struct
  local open SchemeGeneral SchemeBool SchemeChar SchemeNumber SchemeString 
      SchemeSymbol SchemeList SchemePair SchemeProcedure 
      SchemeVector SchemeInputOutput SchemeEnvironment SchemeAst SchemeDynamic
      
  in

  type dynamic = dynamic
  type env = env
  type expression = expression
  type formals = formals
  type definition = definition
  type command_or_definition = command_or_definition

  exception WrongNumberArguments of int  
  
  fun eval_exp env (VARIABLE v) = lookup (v, env)
    | eval_exp env (CALL (f, a)) = PROCEDURE_UNTAG (eval_exp env f) 
         (map (eval_exp env) a)
    | eval_exp env (LAMBDA (f, b)) = 
         PROCEDURE_TAG (fn arglist => 
	 let fun flength (PAIRPAR(_,fr)) = 1 + flength fr
	       | flength _ = 0
	     fun zip (NULLPAR, nil) = nil |
		 zip (PAIRPAR(fv,fr), av::ar) = (fv, av) :: zip (fr, ar) |
		 zip (VARPAR v, l) = [(v, LIST_TAG l)] |
                 zip _ = raise WrongNumberArguments (flength f)
         in eval_body (push (zip (f, arglist), env)) b 
         end)
    | eval_exp env (IF (e0, e1, Some e2)) = 
         if dynamic2bool (eval_exp env e0) then eval_exp env e1 
	 else eval_exp env e2
    | eval_exp env (IF (e0, e1, None)) =
         if dynamic2bool (eval_exp env e0) then eval_exp env e1 
	 else unspecified
    | eval_exp env (ASSIGN (v, e)) = 
      	(lookup (v, env) := !(eval_exp env e); unspecified)
    | eval_exp env (COND b) = 
         let fun eval_cond NULLCOND = unspecified
	       | eval_cond (CONDDEFAULT s) = eval_sequence env s
  	       | eval_cond (CONDCLAUSE (TESTSEQ (e1, e2), r)) =
		     if dynamic2bool (eval_exp env e1) 
			 then eval_sequence env e2
		     else eval_cond r
	       | eval_cond (CONDCLAUSE (TEST e, r)) = 
		     let val testvalue = eval_exp env e 
		     in if dynamic2bool testvalue 
			    then testvalue
			else eval_cond r
		     end
  	       | eval_cond (CONDCLAUSE (TESTREC (e0, e1), r)) =
  	             let val testvalue = eval_exp env e0 
		     in if dynamic2bool testvalue 
			    then PROCEDURE_UNTAG (eval_exp env e1) [testvalue]
			else eval_cond r
		     end
	 in eval_cond b
	 end
    | eval_exp env (CASE (e, cb)) =
          let val v = eval_exp env e 
	      fun eval_case NULLCASE = unspecified
		| eval_case (CASEDEFAULT s) = eval_sequence env s
		| eval_case (CASECLAUSE ((dl, es), r)) = 
		     let fun ev_c nil = eval_case r 
                           | ev_c (d::r) = if is_eqv (v,d) 
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
  		  in if dynamic2bool v 
  		     then eval_and r
  		     else v 
  		  end
          in eval_and l 
  	end |
      eval_exp env (OR l) = 
          let fun eval_or nil = BOOL_TAG false |
  	        eval_or [a] = eval_exp env a |
  	        eval_or (a::r) = 
  		  let val x = eval_exp env a 
  		  in if dynamic2bool x 
  		     then x
  		     else eval_or r 
  		  end
          in eval_or l
  	end |
      eval_exp env (LET (l, b)) =
  	let val binds = map (fn (v,e) => (v, eval_exp env e)) l
  	in eval_body (push (binds, env)) b 
          end |
      eval_exp env (NAMEDLET (itvar, bl, b)) =
          let val binds = (itvar, eval_exp env (LAMBDA 
		(fold (fn ((v,e),y) => PAIRPAR (v,y)) bl NULLPAR, b))) ::
  	                 map (fn (v,e) => (v, eval_exp env e)) bl
          in eval_body (push (binds, env)) b
          end |
      eval_exp env (LETS (l, b)) =
          let fun eval_lets env nil = eval_body env b |
  	        eval_lets env ((v,e)::r) = 
  		     eval_lets (add (v, eval_exp env e, env)) r
          in eval_lets env l
          end |
      eval_exp env (LETREC (l, b)) =
          let val newenv = push (map (fn (v, e) => (v, unspecified)) l, env)
          in (app (fn (v, e) => (lookup(v,newenv) := !(eval_exp newenv e))) l;
  	    eval_body newenv b)
          end |
      eval_exp env (BEGIN s) = eval_sequence env s |
      eval_exp env (DO (l, e, s, el)) =
          let val newenv = push (map (fn (x,y,z) => (x, eval_exp env y)) l, env)
  	    fun iterate() = if dynamic2bool (eval_exp newenv e)
  			    then eval_sequence newenv s
  			    else (app (eval_exp newenv) el; 
  			    	  app (fn (x,y,Some z) => (lookup (x,newenv)
  				  		:= !(eval_exp newenv z)) |
  				          (x,y,None) => ()) l;
  				  iterate())
  	in iterate() 
  	end |
      eval_exp env (DELAY e) = PROCEDURE_TAG (fn _ => eval_exp env e) |
      eval_exp env (LITERAL d) = d |
      eval_exp env (QUASIQUOTE t) = 
        let fun eval_template (SIMPLETEMP d) = d
	      | eval_template (LISTTEMP tl) = 
		    LIST_TAG (foldappend (map eval_template_or_splice tl))
	      | eval_template (ILISTTEMP (tl, t)) =
                    let fun etl nil = eval_template t |
  		          etl (TEMPLATE t :: rt) = PAIR_TAG 
  			  	(eval_template t, etl rt) |
  		          etl (UNQSPL t :: rt) =
  			     let val v = LIST_UNTAG (eval_exp env t) 
  				 fun tagtog nil = etl rt |
  				     tagtog (a::b) = PAIR_TAG (a, tagtog b)
  			     in tagtog v
  			     end
                    in etl tl 
		    end
	      | eval_template (VECTEMP l) = VECTOR_TAG 
		    (foldappend (map eval_template_or_splice l))
	      | eval_template (UNQUOTE e) = eval_exp env e 
            and eval_template_or_splice (TEMPLATE t) = [eval_template t]
	      | eval_template_or_splice (UNQSPL t) = 
		    LIST_UNTAG (eval_exp env t)
        in eval_template t
        end
  and eval_sequence env (el, e) =
      (app (eval_exp env) el; eval_exp env e)
  and eval_body env (nil, s) = eval_sequence env s |
      eval_body env (dl, s) =
          let fun get_vars (VARDEF (v,_):: rd) = v:: get_vars rd |
  	        get_vars (FUNDEF (v, _, _):: rd) = v:: get_vars rd |
  	        get_vars (BEGINDEF dl :: dl') = get_vars (dl @ dl') |
  		get_vars nil = nil
  	    val newenv = push (map (fn v => (v, unspecified)) 
			       (get_vars dl), env)
  	    fun eval_definition_par (VARDEF (v,e)) = 
                  (lookup(v,newenv) := !(eval_exp newenv e)) |
  	    eval_definition_par (FUNDEF (v, f, b)) = 
                  (lookup(v,newenv) := !(eval_exp newenv (LAMBDA (f,b)))) |
              eval_definition_par (BEGINDEF dl) = app eval_definition_par dl
          in (app eval_definition_par dl;
  	    eval_sequence newenv s)
  	end

  and eval_definition (VARDEF (v,e)) = add_top (v, eval_exp empty_env e) |
      eval_definition (FUNDEF (v, f, b)) = 
           eval_definition (VARDEF (v, LAMBDA (f,b))) |
      eval_definition (BEGINDEF dl) = app eval_definition dl

  and eval_command_or_definition (COMMAND e) = eval_exp empty_env e |
      eval_command_or_definition (DEFINITION d) = 
          (eval_definition d; unspecified)



  
  and r_e_w(instream,outstream) =
          let val cod = dat2command_or_definition (read_datum instream)
              val result = eval_command_or_definition cod
          in
              write(result,outstream)
          end



  and read_eval_write(instream,outstream) =
          let fun mes(message,arg) = 
              let val em = "ERROR : "^message^"\n"^"ARGUMENT : "^arg 
              in
                   output(outstream,em)
              end
          in
              r_e_w(instream,outstream)
                 handle IllegalInput(message,arg) => 
		   mes("Illegal Input, "^message,arg)
                 |      Unimplemented name => mes("Unimplemented",name)
                 |      WrongNumberArguments _ => 
                           output(outstream,"Wrong number of arguments")
                 |      EOF => output(outstream,"End of input reached ...")
                 |      TypeError(s,d) => 
			  (output(outstream,"ERROR : Type Error, "^s^"\n"^"ARGUMENT : ");
			   write(d,outstream))

          end



  and load filename =
           (output(std_out,"Loading from "^(sstring2str filename)^" ...\n");
           let val instream = open_in (sstring2str filename)
           in 
               while not (end_of_stream instream) do
                  (output(std_out,"\n");
		   read_eval_write(instream,std_out))
           end)
      


  infix ||

  (* Some coercions for embedding functions in dynamic/tagged types *)



  fun ID f = f
  fun FUNC0 h f = PROCEDURE_TAG (fn nil => h f () |
       			             _ => raise WrongNumberArguments 0)
  fun FUNC0L (g, h) f = PROCEDURE_TAG (fn l => h (f (map g l)))
  fun FUNC1 (g, h) f = PROCEDURE_TAG
        (fn [x] => h (f (g x)) |
            _  => raise WrongNumberArguments 1)
  fun FUNC1L (g, h, i) f = PROCEDURE_TAG
        (fn (x::l) => i (f (g x, map h l)) |
            _ => raise WrongNumberArguments 1)
  fun FUNC2 (g, h, i) f = PROCEDURE_TAG (fn [x,y] => i (f (g x, h y)) |
					 _ => raise WrongNumberArguments 2)
  fun FUNC1O (g, h, i) d f  = PROCEDURE_TAG
        (fn [x] => i (f (g x, d)) |
	   [x,y] => i (f (g x, h y)) |
	       _ => raise WrongNumberArguments 2)
  fun FUNC3 (g, h, i, j) f = PROCEDURE_TAG
	(fn [x,y,z] => j (f (g x, h y, i z)) |
		 _ => raise WrongNumberArguments 3)
  fun FUNC1UT f = (fn [x] => f x |
                      _ => raise WrongNumberArguments 1)

  (* Bindings for all the essential standard procedures required by
     R4Scheme and IEEE Scheme *)

  val init_bindings =
      [ ("not", FUNC1 (BOOL_UNTAG, BOOL_TAG) not),
        ("boolean?", FUNC1 (ID, BOOL_TAG) is_boolean),

	("eqv?", FUNC2 (ID, ID, BOOL_TAG) is_eqv),
	("eq?",  FUNC2 (ID, ID, BOOL_TAG) is_eq),
	("equal?",  FUNC2 (ID, ID, BOOL_TAG) is_equal),

	("pair?", FUNC1 (ID, BOOL_TAG) is_pair),
	("cons", FUNC2 (ID, ID, PAIR_TAG) cons),
        ("car", FUNC1 (PAIR_UNTAG, ID) car),
	("cdr", FUNC1 (PAIR_UNTAG, ID) cdr),
	("set-car!", FUNC2 (PAIR_UNTAG, !, UNSPECIFIED_TAG) set_car), 
	("set_cdr!", FUNC2 (PAIR_UNTAG, !, UNSPECIFIED_TAG) set_cdr),
	("caar", FUNC1 ((PAIR_UNTAG || ID) o PAIR_UNTAG, ID) caar),
	("cadr", FUNC1 ((ID || PAIR_UNTAG) o PAIR_UNTAG, ID) cadr),
	("cdar", FUNC1 ((PAIR_UNTAG || ID) o PAIR_UNTAG, ID) cdar),
	("cddr", FUNC1 ((ID || PAIR_UNTAG) o PAIR_UNTAG, ID) cddr),
	("caaar", FUNC1 (((PAIR_UNTAG || ID) o PAIR_UNTAG || ID) o
		PAIR_UNTAG, ID) caaar),  
	("caadr", PROCEDURE_TAG (fn _ => raise Unimplemented "caadr")), 
	("cadar", FUNC1 (((ID || PAIR_UNTAG) o PAIR_UNTAG || ID) o
		PAIR_UNTAG, ID) cadar), 
	("caddr", PROCEDURE_TAG (fn _ => raise Unimplemented "caddr")),
	("cdaar", PROCEDURE_TAG (fn _ => raise Unimplemented "cdaar")),
	("cdadr", PROCEDURE_TAG (fn _ => raise Unimplemented "cdadr")),
	("cddar", PROCEDURE_TAG (fn _ => raise Unimplemented "cddar")),
	("cdddr", PROCEDURE_TAG (fn _ => raise Unimplemented "cdddr")),
	("caaaar", PROCEDURE_TAG (fn _ => raise Unimplemented "caaaar")),
	("caaadr", PROCEDURE_TAG (fn _ => raise Unimplemented "caaadr")),
	("caadar", PROCEDURE_TAG (fn _ => raise Unimplemented "caadar")),
	("caaddr", PROCEDURE_TAG (fn _ => raise Unimplemented "caaddr")),
	("cadaar", PROCEDURE_TAG (fn _ => raise Unimplemented "cadaar")),
	("cadadr", PROCEDURE_TAG (fn _ => raise Unimplemented "cadadr")),
	("caddar", PROCEDURE_TAG (fn _ => raise Unimplemented "caddar")),
	("cadddr", PROCEDURE_TAG (fn _ => raise Unimplemented "cadddr")),
	("cdaaar", PROCEDURE_TAG (fn _ => raise Unimplemented "cdaaar")),
	("cdaadr", PROCEDURE_TAG (fn _ => raise Unimplemented "cdaadr")),
	("cdadar", PROCEDURE_TAG (fn _ => raise Unimplemented "cdadar")),
	("cdaddr", PROCEDURE_TAG (fn _ => raise Unimplemented "cdaddr")),
	("cddaar", PROCEDURE_TAG (fn _ => raise Unimplemented "cddaar")),
	("cddadr", PROCEDURE_TAG (fn _ => raise Unimplemented "cddadr")),
	("cdddar", PROCEDURE_TAG (fn _ => raise Unimplemented "cdddar")),
	("cddddr", PROCEDURE_TAG (fn _ => raise Unimplemented "cddddr")),
	
	("null?", FUNC1 (ID, BOOL_TAG) is_null),
	("list?", FUNC1 (ID, BOOL_TAG) is_list),
	("list", FUNC0L (ID, LIST_TAG) list),
	("length", FUNC1 (LIST_UNTAG, NUMBER_TAG) length),
	("append", FUNC0L (LIST_UNTAG, LIST_TAG) append),
	("reverse", FUNC1 (LIST_UNTAG, LIST_TAG) reverse),
	("list-tail", FUNC2 (LIST_UNTAG, NUMBER_UNTAG, LIST_TAG) list_tail),
	("list-ref", FUNC2 (LIST_UNTAG, NUMBER_UNTAG, ID) list_ref),
	("memq", FUNC2 (ID, LIST_UNTAG, 
			fn nil => BOOL_TAG false | dl => LIST_TAG dl)
	         (mem is_eq)),
	("memv", FUNC2 (ID, LIST_UNTAG, 
			fn nil => BOOL_TAG false | dl => LIST_TAG dl)
	               (mem is_eqv)),
	("member", FUNC2 (ID, LIST_UNTAG, 
			fn nil => BOOL_TAG false | dl => LIST_TAG dl)
	               (mem is_equal)),
	("assq", FUNC2 (ID, map PAIR_UNTAG o LIST_UNTAG, 
			fn Some p => PAIR_TAG p | None => BOOL_TAG false)
	         (ass is_eq)),
	("assv", FUNC2 (ID, map PAIR_UNTAG o LIST_UNTAG, 
			fn Some p => PAIR_TAG p | None => BOOL_TAG false)
	         (ass is_eqv)),
	("assoc", FUNC2 (ID, map PAIR_UNTAG o LIST_UNTAG, 
			fn Some p => PAIR_TAG p | None => BOOL_TAG false)
	         (ass is_equal)),
		
	("symbol?", FUNC1 (ID, BOOL_TAG) is_symbol),
	("symbol->string", FUNC1 (SYMBOL_UNTAG, STRING_TAG)
		symbol2string), 	
	("string->symbol", FUNC1 (STRING_UNTAG, SYMBOL_TAG)
		string2symbol),

	("number?", FUNC1 (ID, BOOL_TAG) is_number),
	("complex?", FUNC1 (ID, BOOL_TAG) 
	    (fn x => if is_number x then is_complex (NUMBER_UNTAG x)
	                           else false)),
	("real?", FUNC1 (ID, BOOL_TAG) 
	    (fn x => if is_number x then is_real (NUMBER_UNTAG x)
	                           else false)),
	("rational?", FUNC1 (ID, BOOL_TAG) 
	    (fn x => if is_number x then is_rational (NUMBER_UNTAG x)
	                           else false)),
	("integer?", FUNC1 (ID, BOOL_TAG) 
	    (fn x => if is_number x then is_integer (NUMBER_UNTAG x)
	                           else false)),
	("exact?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_exact),
	("inexact?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_inexact),
	("=", FUNC0L (NUMBER_UNTAG, BOOL_TAG) number_eq),
	("<", FUNC0L (NUMBER_UNTAG, BOOL_TAG) number_lt),
	(">", FUNC0L (NUMBER_UNTAG, BOOL_TAG) number_gt),
	("<=", FUNC0L (NUMBER_UNTAG, BOOL_TAG) number_le),
	(">=", FUNC0L (NUMBER_UNTAG, BOOL_TAG) number_ge),
	("zero?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_zero),
	("positive?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_positive),
	("negative?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_negative),
	("odd?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_odd),
	("even?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_even),
	("max", FUNC0L (NUMBER_UNTAG, NUMBER_TAG) max),
	("min", FUNC0L (NUMBER_UNTAG, NUMBER_TAG) min),
	("+", FUNC0L (NUMBER_UNTAG, NUMBER_TAG) plus),
	("*", FUNC0L (NUMBER_UNTAG, NUMBER_TAG) mult),
	("-", FUNC1L (NUMBER_UNTAG, NUMBER_UNTAG, NUMBER_TAG) minus),
	("/", FUNC1L (NUMBER_UNTAG, NUMBER_UNTAG, NUMBER_TAG) divide),
	("abs", FUNC1 (NUMBER_UNTAG, NUMBER_TAG) abs),
	("quotient", FUNC2 (NUMBER_UNTAG, NUMBER_UNTAG, NUMBER_TAG) quotient),
	("remainder", FUNC2 (NUMBER_UNTAG, NUMBER_UNTAG, NUMBER_TAG) remainder),
	("modulo", FUNC2 (NUMBER_UNTAG, NUMBER_UNTAG, NUMBER_TAG) modulo),
	("gcd", FUNC0L (NUMBER_UNTAG, NUMBER_TAG) gcd),
	("lcm", FUNC0L (NUMBER_UNTAG, NUMBER_TAG) lcm),
	("floor", FUNC1 (NUMBER_UNTAG, NUMBER_TAG) floor),
	("ceiling", FUNC1 (NUMBER_UNTAG, NUMBER_TAG) ceiling),
	("truncate", FUNC1 (NUMBER_UNTAG, NUMBER_TAG) truncate),
	("round",  FUNC1 (NUMBER_UNTAG, NUMBER_TAG) round),
	("number->string", FUNC1O (NUMBER_UNTAG, NUMBER_UNTAG, STRING_TAG) 10 number2string),
	("string->number", FUNC1O (STRING_UNTAG, NUMBER_UNTAG, NUMBER_TAG) 10 string2number),
	
	("char?", FUNC1 (ID, BOOL_TAG) is_char),
	("char=?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_eq),
	("char<?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_lt),
	("char>?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_gt),
	("char<=?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_le),
	("char>=?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_ge),
	("char-ci=?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_ci_eq),
	("char-ci<?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_ci_lt),
	("char-ci>?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_ci_gt),
	("char-ci<=?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_ci_le),
	("char-ci>=?", FUNC2 (CHAR_UNTAG, CHAR_UNTAG, BOOL_TAG) char_ci_ge),
	("char-alphabetic?", FUNC1 (CHAR_UNTAG, BOOL_TAG) is_char_alphabetic),
	("char-numeric?", FUNC1 (CHAR_UNTAG, BOOL_TAG) is_char_numeric),
	("char-whitespace?", FUNC1 (CHAR_UNTAG, BOOL_TAG) is_char_whitespace),
	("char-upper-case?", FUNC1 (CHAR_UNTAG, BOOL_TAG) is_char_upper_case),
	("char-lower-case?", FUNC1 (CHAR_UNTAG, BOOL_TAG) is_char_lower_case),
	("char->integer", FUNC1 (CHAR_UNTAG, NUMBER_TAG) char2integer),
	("integer->char", FUNC1 (NUMBER_UNTAG, CHAR_TAG) integer2char),
	("char-upcase", FUNC1 (CHAR_UNTAG, CHAR_TAG) char_upcase),
	("char-downcase", FUNC1 (CHAR_UNTAG, CHAR_TAG) char_downcase),

	("string?", FUNC1 (ID, BOOL_TAG) is_string),
	("make-string", FUNC1O (NUMBER_UNTAG, CHAR_UNTAG, STRING_TAG) (str2char " ") make_string),
	("string", FUNC0L (CHAR_UNTAG, STRING_TAG) string),
	("string-length", FUNC1 (STRING_UNTAG, NUMBER_TAG) string_length),
	("string-ref", FUNC2 (STRING_UNTAG, NUMBER_UNTAG, CHAR_TAG) string_ref),
	("string-set!", FUNC3 (STRING_UNTAG, NUMBER_UNTAG, CHAR_UNTAG, UNSPECIFIED_TAG) string_set),
	("string=?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_eq),
	("string-ci=?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_ci_eq),
	("string<?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_lt),
	("string>?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_gt),
	("string<=?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_le),
	("string>=?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_ge),
	("string-ci<?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_ci_lt),
	("string-ci>?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_ci_gt),
	("string-ci<=?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_ci_le),
	("string-ci>=?", FUNC2 (STRING_UNTAG, STRING_UNTAG, BOOL_TAG) string_ci_ge),
	("substring", FUNC3 (STRING_UNTAG, NUMBER_UNTAG, NUMBER_UNTAG, STRING_TAG) substring),
	("string-append", FUNC0L (STRING_UNTAG, STRING_TAG) string_append),
	("string->list", FUNC1 (STRING_UNTAG, LIST_TAG o map CHAR_TAG) string2list),
	("list->string", FUNC1 (map CHAR_UNTAG o LIST_UNTAG, STRING_TAG) list2string),
	("string-copy", FUNC1 (STRING_UNTAG, STRING_TAG) string_copy),
	("string-fill!", FUNC2 (STRING_UNTAG, CHAR_UNTAG, UNSPECIFIED_TAG) string_fill),

	("vector?", FUNC1 (ID, BOOL_TAG) is_vector),
	("make-vector", FUNC1O (NUMBER_UNTAG, ID, VECTOR_TAG) unspecified make_vector),
	("vector", FUNC0L (ID, VECTOR_TAG) vector),
	("vector-length", FUNC1 (VECTOR_UNTAG, NUMBER_TAG) vector_length),
	("vector-ref", FUNC2 (VECTOR_UNTAG, NUMBER_UNTAG, ID) vector_ref),
	("vector-set!", FUNC3 (VECTOR_UNTAG, NUMBER_UNTAG, !, UNSPECIFIED_TAG) vector_set),
	("vector->list", FUNC1 (VECTOR_UNTAG, LIST_TAG) vector2list),
	("list->vector", FUNC1 (LIST_UNTAG, VECTOR_TAG) list2vector),
	("vector-fill!", FUNC2 (VECTOR_UNTAG, !, UNSPECIFIED_TAG) vector_fill),

	("procedure?", FUNC1 (ID, BOOL_TAG) is_procedure),
	("apply", FUNC2 (PROCEDURE_UNTAG, LIST_UNTAG, ID) apply),
	("map", FUNC1L (PROCEDURE_UNTAG, LIST_UNTAG, LIST_TAG) mapn),
	("for-each", FUNC1L (PROCEDURE_UNTAG, LIST_UNTAG, UNSPECIFIED_TAG) for_each),
	("force", FUNC1 (PROCEDURE_UNTAG, ID) (fn p => p nil)),
	("call-with-current-continuation", PROCEDURE_TAG 
	 (fn _ => raise Unimplemented "call-with-current-continuation")), 
(*	("call-with-current-continuation", FUNC1 (PROCEDURE_UNTAG, ID) 
	        (((fn p => [PROCEDURE_TAG (FUNC1UT p)] --> ID) --> ID)) 
			    call_with_current_continuation), *)
	
        ("call-with-input-file", PROCEDURE_TAG 
	 (fn _ => raise Unimplemented "call-with-input-file")),
	("call-with-output-file", PROCEDURE_TAG 
	 (fn _ => raise Unimplemented "call-with-output-file")),
	("input-port?", FUNC1 (ID, BOOL_TAG) is_input_port),
	("output-port?", FUNC1 (ID, BOOL_TAG) is_output_port),
	("current-input-port", PROCEDURE_TAG 
	 (fn _ => raise Unimplemented "current-input-port")),
	("current-output-port", PROCEDURE_TAG 
	 (fn _ => raise Unimplemented "current-output-port")),
	("with-input-from-file", PROCEDURE_TAG 
	 (fn _ => raise Unimplemented "with-input-from-file")),
	("with-output-to-file", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "with-output-to-file")),
	("open-input-file", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "open-input-file")),
	("open-output-file", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "open-output-file")),
	("close-output-file", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "close-output-file")),
	("close-input-port", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "close-input-port")),
	("close-output-port", PROCEDURE_TAG 
	 (fn _ => raise Unimplemented "close-output-port")),
	("read", PROCEDURE_TAG 
	 (fn _ => raise Unimplemented "read")),
	("read-char", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "read-char")),
	("peek-char", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "peek-char")),
	("eof-object?", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "eof-object?")),
	("char-ready?", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "char-ready?")),
	("write", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "write")),
	("display", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "display")),
	("newline", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "newline")),
	("write-char", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "write-char")),
	("load", FUNC1 (STRING_UNTAG, UNSPECIFIED_TAG) load),
	("transcript-on", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "transcript-on")),
	("transcript-off", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "transcript-off")),
        ("exit", PROCEDURE_TAG
          (fn _ => raise EXIT))
	]


  val v = push_top init_bindings





fun SmlScheme ()  = 
    let fun eval_loop() =
        while true do
           let fun prompt () = 
               output(std_out,"\nSmlScheme > ")
           in
               (prompt ();
                read_eval_write(std_in,std_out))
           end    
    in
        eval_loop()
        handle EXIT => output(std_out,"Bye\n")
    end




  end
  end
