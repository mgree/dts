signature INTERPRET =
  sig
 
 type command
 type value
 type env 

 exception RuntimeError of string 
 val top_env: env ref
 val eval: command -> value
 val command_loop: instream * outstream -> unit
 val scheme: unit -> unit

end

functor Interpret (Runtime: RUNTIME) =
  struct

  type command = Command.command
  type value = Dynamic.dynamic
  type env = Environment.env

  exception RuntimeError of string

  local
  open Datum Dynamic
  in
  fun eval_datum (BOOLDAT b) = BOOL_TAG b
    | eval_datum (CHARDAT c) = CHAR_TAG (Character.str2char c)
    | eval_datum (STRIDAT s) = STRING_TAG (String.str2sstring s)
    | eval_datum (SYMBDAT s) = SYMBOL_TAG (Symbol.str2symbol s)
    | eval_datum (NUMBDAT n) = NUMBER_TAG (Number.str2number n)
    | eval_datum (VECTDAT dl) = VECTOR_TAG (Vector.vector (map eval_datum dl))
    | eval_datum (PAIRDAT (d1, d2)) = 
	PAIR_TAG (Pair.cons (eval_datum d1, eval_datum d2))
    | eval_datum NILDAT = LIST_TAG nil
  end

  local 
  open Environment 
  in
  val top_env = ref (push empty_env Basis.bindings)
  fun get_value (v, env) = 
	lookup env v handle Lookup _ => lookup (!top_env) v 
  end

  local
  open Command Dynamic Environment
  in
  fun eval_exp env (LITERAL d) = eval_datum d
    | eval_exp env (VARIABLE v) = get_value (v, env)
    | eval_exp env (CALL (f, a)) = 
	PROCEDURE_UNTAG (eval_exp env f) (map (eval_exp env) a)
    | eval_exp env (LAMBDA (f, b)) = 
        PROCEDURE_TAG (fn arglist => 
	 let fun flength (PAIRPAR(_,fr)) = 1 + flength fr
	       | flength _ = 0
	     fun zip (NULLPAR, nil) = nil |
		 zip (PAIRPAR(fv,fr), av::ar) = (fv, av) :: zip (fr, ar) |
		 zip (VARPAR v, l) = [(v, LIST_TAG l)] |
                 zip _ = raise RuntimeError "Wrong number of arguments"
         in eval_body (push env (zip (f, arglist))) b 
         end) 
    | eval_exp env (IF (e0, e1, e2)) = 
         if dynamic2bool (eval_exp env e0) 
	    then eval_exp env e1 
	 else eval_exp env e2
    | eval_exp env (ASSIGN (v, e)) = 
	((get_value (v, env) handle 
	   Lookup _ => let val unspec_value = Dynamic.UNSPECIFIED_TAG ()
		       in (top_env := add (!top_env) (v, unspec_value); 
			   unspec_value)
		       end) := !(eval_exp env e); 
         unspecified)
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
  		  in if dynamic2bool v 
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
  		  in if dynamic2bool x 
  		     then x
  		     else eval_or r 
  		  end
          in eval_or l
  	  end 
     | eval_exp env (LET (l, b)) =
  	let val binds = map (fn (v,e) => (v, eval_exp env e)) l
  	in eval_body (push env binds) b 
          end
     | eval_exp env (NAMEDLET (itvar, bl, b)) =
          let val binds = (itvar, eval_exp env (LAMBDA 
		(fold (fn ((v,e),y) => PAIRPAR (v,y)) bl NULLPAR, b))) ::
  	                 map (fn (v,e) => (v, eval_exp env e)) bl
          in eval_body (push env binds) b
          end
     | eval_exp env (LETS (l, b)) =
          let fun eval_lets env nil = eval_body env b |
  	        eval_lets env ((v,e)::r) = 
  		     eval_lets (add env (v, eval_exp env e)) r
          in eval_lets env l
          end
     | eval_exp env (LETREC (l, b)) =
          let val newenv = push env (map (fn (v, e) => (v, unspecified)) l)
          in (app (fn (v, e) => (get_value(v,newenv) := !(eval_exp newenv e))) l;
  	    eval_body newenv b)
          end
     | eval_exp env (BEGIN s) = eval_sequence env s
     | eval_exp env (DO (l, (e, s), el)) =
          let val newenv = push env (map (fn (x,y,z) => (x, eval_exp env y)) l)
  	      fun iterate() = 
		if dynamic2bool (eval_exp newenv e)
  		   then eval_sequence newenv s
  		else (app (eval_exp newenv) el; 
  		      app (fn (x,y,UNDEFEXP) => () 
	                 | (x,y,z)=>(get_value (x,newenv):= !(eval_exp newenv z))) l;
	              iterate())
  	  in iterate() 
  	  end
     | eval_exp env (DELAY e) = PROCEDURE_TAG (fn _ => eval_exp env e) 
     | eval_exp env (QUASIQUOTE t) = 
        let fun append (nil, r) = r
              | append (l1::lr, r) = PAIR_TAG (l1, append (lr, r))
            fun eval_vector_list nil = nil
	      | eval_vector_list (TEMPLATE t :: lr) = 
		  eval_template t :: eval_vector_list lr
              | eval_vector_list (SPLICE e :: lr) =
		  LIST_UNTAG (eval_exp env e) @ eval_vector_list lr
	    and eval_template (SIMPLETEMP d) = eval_datum d
	      | eval_template (PAIRTEMP (TEMPLATE t1, t2)) = 
		  PAIR_TAG (eval_template t1, eval_template t2)
	      | eval_template (PAIRTEMP (SPLICE e, t)) =
		  append (LIST_UNTAG (eval_exp env e), eval_template t)
	      | eval_template (VECTTEMP tl) = 
		  VECTOR_TAG (Vector.vector (eval_vector_list tl))
	      | eval_template (UNQUOTE e) = eval_exp env e
        in eval_template t
        end 
    | eval_exp env UNDEFEXP = unspecified
  and eval_sequence env nil = unspecified
    | eval_sequence env [e] = eval_exp env e
    | eval_sequence env (e1::er) = (eval_exp env e1; eval_sequence env er) 
  and eval_body env (nil, s) = eval_sequence env s |
      eval_body env (dl, s) =
          let fun get_vars (VARDEF (v,_):: rd) = v:: get_vars rd |
  	        get_vars (FUNDEF (v, _, _):: rd) = v:: get_vars rd |
  	        get_vars (BEGINDEF dl :: dl') = get_vars (dl @ dl') |
  		get_vars nil = nil
  	    val newenv = push env (map (fn v => (v, unspecified))(get_vars dl))
  	    fun eval_definition_par (VARDEF (v,e)) = 
                  (get_value(v,newenv) := !(eval_exp newenv e)) |
  	    eval_definition_par (FUNDEF (v, f, b)) = 
                  (get_value(v,newenv) := !(eval_exp newenv (LAMBDA (f,b)))) |
              eval_definition_par (BEGINDEF dl) = app eval_definition_par dl
          in (app eval_definition_par dl;
  	    eval_sequence newenv s)
  	end

  fun eval_definition (VARDEF (v,e)) = 
	(top_env := add (!top_env) (v, eval_exp empty_env e)) |
      eval_definition (FUNDEF (v, f, b)) = 
           eval_definition (VARDEF (v, LAMBDA (f,b))) |
      eval_definition (BEGINDEF dl) = app eval_definition dl 

  fun eval (EXP e) = eval_exp empty_env e |
      eval (DEF d) = (eval_definition d; unspecified)

  fun command_loop (is, os, es) =
      ((output (es, "> ");
        Dynamic.write (eval (Command.parse (Datum.read is)), os)) handle
           Datum.ReadError (m, is) => output(es, m ^ ": " ^ is)
         | Command.ParseError (m, d) => 
		(output(es, m ^ ":\n"); Dynamic.write (eval_datum d, es))
         | RuntimeError s => output(es, s)
         | Dynamic.TypeError (m, dv) => 
		(output(es, m ^ ": "); Dynamic.write (dv, es))
	 | Environment.Lookup k => output(es, "Unbound variable: " ^ k)
         | General.Unimplemented s => output(es, s ^ "not implemented\n")
         | General.IllegalInput (m,s) => output(es, m ^": " ^ s);
       output (es, "\n");
       command_loop (is, os, es)) handle Datum.EOF => ()

  fun scheme () = command_loop (std_in, std_out, std_out)

  end
  end
