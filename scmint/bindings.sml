(*$ SchemeBindings: SCHEMEBINDINGS SchemeEnvironment SchemeBool SchemeChar
                  SchemeString SchemeNumber SchemeSymbol SchemePair
		  SchemeList SchemeProcedure SchemeVector SchemeInputOutput
		  SchemeDynamic *)

structure Bindings: BINDINGS =
  struct 
  local open Bool Char Number Symbol String Pair List Procedure Vector
             InputOutput Dynamic Environment Coercion 
  in
  type variable = variable
  type dynamic = dynamic

  val initbindings =
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
	("memq", PROCEDURE_TAG 
	 (fn dval => case FUNC2UT (ID, LIST_UNTAG, ID) (mem is_eq)
		dval of nil => BOOL_TAG false | dl => LIST_TAG dl)),
	("memv", PROCEDURE_TAG
	 (fn dval => case FUNC2UT (ID, LIST_UNTAG, ID) (mem is_eqv)
		dval of nil => BOOL_TAG false | dl => LIST_TAG dl)),
	("member", PROCEDURE_TAG
	 (fn dval => case FUNC2UT (ID, LIST_UNTAG, ID) (mem is_equal)
		dval of nil => BOOL_TAG false | dl => LIST_TAG dl)),
	("assq", PROCEDURE_TAG
	 (fn dval => FUNC2UT (ID, list_map PAIR_UNTAG o LIST_UNTAG, PAIR_TAG)
	  (ass is_eq) dval handle EmptyList "ass" => BOOL_TAG false)),
	("assv", PROCEDURE_TAG
	 (fn dval => FUNC2UT (ID, list_map PAIR_UNTAG o LIST_UNTAG, PAIR_TAG)
	  (ass is_eqv) dval handle EmptyList "ass" => BOOL_TAG false)),
	("assoc", PROCEDURE_TAG
	 (fn dval => FUNC2UT (ID, list_map PAIR_UNTAG o LIST_UNTAG, PAIR_TAG)
	  (ass is_equal) dval handle EmptyList "ass" => BOOL_TAG false)),
		
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
	("string->list", FUNC1 (STRING_UNTAG, LIST_TAG o list_map CHAR_TAG) string2list),
	("list->string", FUNC1 (list_map CHAR_UNTAG o LIST_UNTAG, STRING_TAG) list2string),
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
	("apply", FUNC2 (PROCEDURE_UNTAG, slist2list o LIST_UNTAG, ID) apply),
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
	("load", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "load")),
	("transcript-on", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "transcript-on")),
	("transcript-off", PROCEDURE_TAG
	 (fn _ => raise Unimplemented "transcript-off"))
	]
  end
  end
