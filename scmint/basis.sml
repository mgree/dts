structure Basis: BASIS =
  struct

  open Error Object Pair SList Symbol Number Char String SVector 
       Control IO

  (* Some coercions for embedding functions in dynamic/tagged types *)

  fun wrong_number_arguments n = 
      raise InputError ("Wrong number of arguments", UNSPECIFIED_TAG())

  fun ID f = f
  fun FUNC0 h f = PROCEDURE_TAG 
	(fn nil => h (f ())
       	  | _ => wrong_number_arguments 0)
  fun FUNC0L (g, h) f = PROCEDURE_TAG 
	(fn l => h (f (List.map g l)))
  fun FUNC1 (g, h) f = PROCEDURE_TAG
        (fn [x] => h (f (g x)) |
            _  => wrong_number_arguments 1)
  fun FUNC1L (g, h, i) f = PROCEDURE_TAG
        (fn (x::l) => i (f (g x, List.map h l))
          | _ => wrong_number_arguments 1)
  fun FUNC2UT (g, h, i) f =	
	(fn [x,y] => i (f (g x, h y))
          | _ => wrong_number_arguments 2)
  fun FUNC2 c f = PROCEDURE_TAG (FUNC2UT c f)
  fun FUNC2L (g, h, i, j) f = PROCEDURE_TAG 
	(fn (x::y::l) => j (f (g x, h y, List.map i l))
          | _ => wrong_number_arguments 2)
  fun FUNC3 (g, h, i, j) f = PROCEDURE_TAG
	(fn [x,y,z] => j (f (g x, h y, i z))
	  | _ => wrong_number_arguments 3)

  fun FUNC1OR2 (g, h, i, d) f  = PROCEDURE_TAG
        (fn [y] => i (f (g d, h y))
	  | [x,y] => i (f (g x, h y))
	  | _ => wrong_number_arguments 2)
  fun FUNC2OR1 (g, h, i, d) f = PROCEDURE_TAG  
	(fn [x] => i (f (g x, h d))
          | [x,y] => i (f (g x, h y))
          | _ => wrong_number_arguments 2)
  fun FUNC1OR0 (h, i) (g, f) = PROCEDURE_TAG
        (fn nil => i (f ())
          | [x] => i (g (h x))
          | _ => wrong_number_arguments 1)
  fun FUNC2ELSE1 (h, i, j) (g, f) = PROCEDURE_TAG
	(fn [x] => j (f (h x))
          | [x,y] => j (g (h x, i y))
          | _ => wrong_number_arguments 2)

  fun FUNC1UT f = (fn [x] => f x |
                      _ => wrong_number_arguments 1)

  (* Bindings for all the essential standard procedures required by IEEE Scheme *)

  val bindings = [
        ("boolean?", FUNC1 (ID, BOOL_TAG) is_boolean),
	("symbol?", FUNC1 (ID, BOOL_TAG) is_symbol),
	("char?", FUNC1 (ID, BOOL_TAG) is_char),
	("vector?", FUNC1 (ID, BOOL_TAG) is_vector),
	("pair?", FUNC1 (ID, BOOL_TAG) is_pair),
	("number?", FUNC1 (ID, BOOL_TAG) is_number),
	("string?", FUNC1 (ID, BOOL_TAG) is_string),
	("procedure?", FUNC1 (ID, BOOL_TAG) is_procedure),
	("list?", FUNC1 (ID, BOOL_TAG) is_list),
	("null?", FUNC1 (ID, BOOL_TAG) is_null),
	("input-port?", FUNC1 (ID, BOOL_TAG) is_input_port),
	("output-port?", FUNC1 (ID, BOOL_TAG) is_output_port),
	("eof-object?",  FUNC1 (ID, BOOL_TAG) is_eof_object),
	
	("not", FUNC1 (ID, BOOL_TAG) not),

	("eqv?", FUNC2 (ID, ID, BOOL_TAG) is_eqv),
	("eq?",  FUNC2 (ID, ID, BOOL_TAG) is_eq),
	("equal?",  FUNC2 (ID, ID, BOOL_TAG) is_equal),

	("cons", FUNC2 (ID, ID, PAIR_TAG) cons),
        ("car", FUNC1 (PAIR_UNTAG, ID) car),
	("cdr", FUNC1 (PAIR_UNTAG, ID) cdr),
	("set-car!", FUNC2 (PAIR_UNTAG, ID, UNSPECIFIED_TAG) set_car), 
	("set_cdr!", FUNC2 (PAIR_UNTAG, ID, UNSPECIFIED_TAG) set_cdr),
	("caar", FUNC1 (PAIR_UNTAG, ID) caar),
	("cadr", FUNC1 (PAIR_UNTAG, ID) cadr),
	("cdar", FUNC1 (PAIR_UNTAG, ID) cdar),
	("cddr", FUNC1 (PAIR_UNTAG, ID) cddr),
	("caaar", FUNC1 (PAIR_UNTAG, ID) caaar),  
	("caadr", FUNC1 (PAIR_UNTAG, ID) caadr),
	("cadar", FUNC1 (PAIR_UNTAG, ID) cadar),
	("caddr", FUNC1 (PAIR_UNTAG, ID) caddr),
	("cdaar", FUNC1 (PAIR_UNTAG, ID) cdaar),
	("cdadr", FUNC1 (PAIR_UNTAG, ID) cdadr),
	("cddar", FUNC1 (PAIR_UNTAG, ID) cddar),
	("cdddr", FUNC1 (PAIR_UNTAG, ID) cdddr),
	("caaaar", FUNC1 (PAIR_UNTAG, ID) caaaar),
	("caaadr", FUNC1 (PAIR_UNTAG, ID) caaadr),
	("caadar", FUNC1 (PAIR_UNTAG, ID) caadar),
	("caaddr", FUNC1 (PAIR_UNTAG, ID) caaddr),
	("cadaar", FUNC1 (PAIR_UNTAG, ID) cadaar),
	("cadadr", FUNC1 (PAIR_UNTAG, ID) cadadr),
	("caddar", FUNC1 (PAIR_UNTAG, ID) caddar),
	("cadddr", FUNC1 (PAIR_UNTAG, ID) cadddr),
	("cdaaar", FUNC1 (PAIR_UNTAG, ID) cdaaar),
	("cdaadr", FUNC1 (PAIR_UNTAG, ID) cdaadr),
	("cdadar", FUNC1 (PAIR_UNTAG, ID) cdadar),
	("cdaddr", FUNC1 (PAIR_UNTAG, ID) cdaddr),
	("cddaar", FUNC1 (PAIR_UNTAG, ID) cddaar),
	("cddadr", FUNC1 (PAIR_UNTAG, ID) cddadr),
	("cdddar", FUNC1 (PAIR_UNTAG, ID) cdddar),
	("cddddr", FUNC1 (PAIR_UNTAG, ID) cddddr),
	
	("list", PROCEDURE_TAG list),
	("length", FUNC1 (LIST_UNTAG, NUMBER_TAG) length),
	("append", PROCEDURE_TAG append),
	("reverse", FUNC1 (LIST_UNTAG, LIST_TAG) reverse),
	("list-ref", FUNC2 (LIST_UNTAG, NUMBER_UNTAG, ID) list_ref),
	("memq", FUNC2 (ID, LIST_UNTAG, (fn nil => BOOL_TAG false | 
	         l => LIST_TAG l)) memq),
	("memv", FUNC2 (ID, LIST_UNTAG, (fn nil => BOOL_TAG false | 
	         l => LIST_TAG l)) memv),
	("member", FUNC2 (ID, LIST_UNTAG, (fn nil => BOOL_TAG false | 
	         l => LIST_TAG l)) member),
	("assq", PROCEDURE_TAG (fn l => (FUNC2UT (ID, List.map PAIR_UNTAG o 
	    LIST_UNTAG, PAIR_TAG) assq l handle NotFound => BOOL_TAG false))), 
	("assv", PROCEDURE_TAG (fn l => (FUNC2UT (ID, List.map PAIR_UNTAG o 
	    LIST_UNTAG, PAIR_TAG) assv l handle NotFound => BOOL_TAG false))), 
	("assoc", PROCEDURE_TAG (fn l => (FUNC2UT (ID, List.map PAIR_UNTAG o 
            LIST_UNTAG, PAIR_TAG) assoc l handle NotFound => BOOL_TAG false))), 
		
	("symbol->string", FUNC1 (SYMBOL_UNTAG, STRING_TAG o FIXED)
		symbol2string), 	
	("string->symbol", FUNC1 (mstring2string o STRING_UNTAG, SYMBOL_TAG)
		string2symbol),

	("complex?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_complex),
	("real?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_real),
	("rational?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_rational),
	("integer?", FUNC1 (NUMBER_UNTAG, BOOL_TAG) is_integer),
	("exact?", FUNC1 (COMPLEX_UNTAG, BOOL_TAG) is_exact),
	("inexact?", FUNC1 (COMPLEX_UNTAG, BOOL_TAG) is_inexact),
	("=", FUNC2L (COMPLEX_UNTAG, COMPLEX_UNTAG, COMPLEX_UNTAG, BOOL_TAG) eq),
	("<", FUNC2L (REAL_UNTAG, REAL_UNTAG, REAL_UNTAG, BOOL_TAG) lt),
	(">", FUNC2L (REAL_UNTAG, REAL_UNTAG, REAL_UNTAG, BOOL_TAG) gt),
	("<=", FUNC2L (REAL_UNTAG, REAL_UNTAG, REAL_UNTAG, BOOL_TAG) le),
	(">=", FUNC2L (REAL_UNTAG, REAL_UNTAG, REAL_UNTAG, BOOL_TAG) ge),
	("zero?", FUNC1 (COMPLEX_UNTAG, BOOL_TAG) is_zero),
	("positive?", FUNC1 (REAL_UNTAG, BOOL_TAG) is_positive),
	("negative?", FUNC1 (REAL_UNTAG, BOOL_TAG) is_negative),
	("odd?", FUNC1 (INTEGER_UNTAG, BOOL_TAG) is_odd),
	("even?", FUNC1 (INTEGER_UNTAG, BOOL_TAG) is_even),
	("max", FUNC1L (REAL_UNTAG, REAL_UNTAG, REAL_TAG) max),
	("min", FUNC1L (REAL_UNTAG, REAL_UNTAG, REAL_TAG) min),
	("+", FUNC0L (COMPLEX_UNTAG, COMPLEX_TAG) plus),
	("*", FUNC0L (COMPLEX_UNTAG, COMPLEX_TAG) mult),
	("-", FUNC1OR2 (COMPLEX_UNTAG, COMPLEX_UNTAG, COMPLEX_TAG, 
	      NUMBER_TAG 0) minus),
	("/", FUNC1OR2 (COMPLEX_UNTAG, COMPLEX_UNTAG, COMPLEX_TAG, 
	      NUMBER_TAG 1) divide),
	("abs", FUNC1 (REAL_UNTAG, REAL_TAG) abs),
	("quotient", FUNC2 (INTEGER_UNTAG, INTEGER_UNTAG, INTEGER_TAG) quotient),
	("remainder", FUNC2 (INTEGER_UNTAG, INTEGER_UNTAG, INTEGER_TAG) remainder),
	("modulo", FUNC2 (INTEGER_UNTAG, INTEGER_UNTAG, INTEGER_TAG) modulo),
	("gcd", FUNC0L (INTEGER_UNTAG, INTEGER_TAG) gcd),
	("lcm", FUNC0L (INTEGER_UNTAG, INTEGER_TAG) lcm),
        ("numerator", FUNC1 (RATIONAL_UNTAG, INTEGER_TAG) numerator),
        ("denominator", FUNC1 (RATIONAL_UNTAG, NATURAL_TAG) denominator),
	("floor", FUNC1 (REAL_UNTAG, INTEGER_TAG) floor),
	("ceiling", FUNC1 (REAL_UNTAG, INTEGER_TAG) ceiling),
	("truncate", FUNC1 (REAL_UNTAG, INTEGER_TAG) truncate),
	("round",  FUNC1 (REAL_UNTAG, INTEGER_TAG) round),
        ("rationalize", FUNC2 (REAL_UNTAG, REAL_UNTAG, RATIONAL_TAG) rationalize),
        ("exp", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) exp),
        ("log", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) log),
        ("sin", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) sin),
        ("cos", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) cos),
        ("tan", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) tan),
        ("asin", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) asin),
        ("acos", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) acos),
        ("atan", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) atan),
        ("atanr", FUNC2 (REAL_UNTAG, REAL_UNTAG, REAL_TAG) atanr),
        ("sqrt", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) sqrt),
        ("expt", FUNC2 (COMPLEX_UNTAG, COMPLEX_UNTAG, COMPLEX_TAG) expt),
        ("make-rectangular", FUNC2 (REAL_UNTAG, REAL_UNTAG, COMPLEX_TAG) 
		make_rectangular),
        ("make-polar", FUNC2 (REAL_UNTAG, REAL_UNTAG, COMPLEX_TAG) make_polar),
        ("real-part", FUNC1 (COMPLEX_UNTAG, REAL_TAG) real_part),
        ("imag-part", FUNC1 (COMPLEX_UNTAG, REAL_TAG) imag_part),
        ("magnitude", FUNC1 (COMPLEX_UNTAG, REAL_TAG) imag_part),
        ("angle", FUNC1 (COMPLEX_UNTAG, REAL_TAG) imag_part),
        ("exact->inexact", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) exact2inexact),
        ("inexact->exact", FUNC1 (COMPLEX_UNTAG, COMPLEX_TAG) inexact2exact),
	("number->string", FUNC2OR1 (NUMBER_UNTAG, NATURAL2RADIX o 
		NATURAL_UNTAG, STRING_TAG o FIXED, NUMBER_TAG 10) number2string),
	("string->number", FUNC2OR1 (mstring2string o STRING_UNTAG, 
		NATURAL2RADIX o NATURAL_UNTAG, NUMBER_TAG, NUMBER_TAG 10) 
		string2number),
	
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
	("char->integer", FUNC1 (CHAR_UNTAG, INTEGER_TAG) char2integer),
	("integer->char", FUNC1 (INTEGER_UNTAG, CHAR_TAG) integer2char),
	("char-upcase", FUNC1 (CHAR_UNTAG, CHAR_TAG) char_upcase),
	("char-downcase", FUNC1 (CHAR_UNTAG, CHAR_TAG) char_downcase),

	("make-string", FUNC2OR1 (NATURAL_UNTAG, CHAR_UNTAG, STRING_TAG, 
		        CHAR_TAG unspec_char) make_string),
	("string", FUNC0L (CHAR_UNTAG, STRING_TAG) string),
	("string-length", FUNC1 (STRING_UNTAG, NATURAL_TAG) string_length),
	("string-ref", FUNC2 (STRING_UNTAG, NATURAL_UNTAG, CHAR_TAG) string_ref),
	("string-set!", FUNC3 (STRING_UNTAG, NATURAL_UNTAG, CHAR_UNTAG, 
		        UNSPECIFIED_TAG) string_set),
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
	("substring", FUNC3 (STRING_UNTAG, NATURAL_UNTAG, NATURAL_UNTAG, 
		      STRING_TAG) substring),
	("string-append", FUNC0L (STRING_UNTAG, STRING_TAG) string_append),

	("make-vector", FUNC2OR1 (NATURAL_UNTAG, ID, VECTOR_TAG, 
		        UNSPECIFIED_TAG()) make_vector),
	("vector", FUNC0L (ID, VECTOR_TAG) vector),
	("vector-length", FUNC1 (VECTOR_UNTAG, NATURAL_TAG) vector_length),
	("vector-ref", FUNC2 (VECTOR_UNTAG, NATURAL_UNTAG, ID) vector_ref),
	("vector-set!", FUNC3 (VECTOR_UNTAG, NATURAL_UNTAG, ID, 
	                UNSPECIFIED_TAG) vector_set),

	("apply", FUNC2L (PROCEDURE_UNTAG, ID, ID, ID) apply),
	("map", FUNC2L (PROCEDURE_UNTAG, LIST_UNTAG, LIST_UNTAG, LIST_TAG) map),
	("for-each", FUNC2L (PROCEDURE_UNTAG, LIST_UNTAG, LIST_UNTAG, 
		     UNSPECIFIED_TAG) for_each),
	("call-with-current-continuation", FUNC1 (PROCEDURE_UNTAG, ID)
		     call_with_current_continuation),
	
        ("call-with-input-file", FUNC2 (mstring2string o STRING_UNTAG, 
		     PROCEDURE_UNTAG, ID) call_with_input_file),
	("call-with-output-file", FUNC2 (mstring2string o STRING_UNTAG, 
		     PROCEDURE_UNTAG, ID) call_with_output_file),
	("current-input-port", FUNC0 INPUT_PORT_TAG current_input_port),
	("current-output-port", FUNC0 OUTPUT_PORT_TAG current_output_port),
	("open-input-file", FUNC1 (mstring2string o STRING_UNTAG, 
		     INPUT_PORT_TAG) open_input_file),
	("open-output-file", FUNC1 (mstring2string o STRING_UNTAG, 
		     OUTPUT_PORT_TAG) open_output_file),
	("close-input-port", FUNC1 (INPUT_PORT_UNTAG, UNSPECIFIED_TAG)
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
		  (display, display_current))
	]

  end
