(* open Types

signature CONVERSION =
  sig

  val object2boolean: object -> boolean
  val mstring2string: mstring -> string
  val string2mstring: string -> mstring
  val vector2list: vector -> plist 

  end

signature ERROR =
  sig

  exception Unimplemented of string
  and Impossible of string
  and ParseError of string * datum
  and TypeError of string * object
  and InputError of string * object
  and IOError of string * string
  and EndOfFile

  end

signature OBJECT =
  sig

  val BOOL_TAG: boolean -> object
  val SYMBOL_TAG: symbol -> object
  val CHAR_TAG: char -> object
  val VECTOR_TAG: vector -> object
  val PAIR_TAG: pair -> object
  val NUMBER_TAG: number -> object
  val COMPLEX_TAG: complex -> object
  val REAL_TAG: real -> object
  val RATIONAL_TAG: rational -> object
  val INTEGER_TAG: integer -> object
  val NATURAL_TAG: natural -> object
  val STRING_TAG: mstring -> object
  val PROCEDURE_TAG: procedure -> object
  val LIST_TAG: plist -> object
  val INPUT_PORT_TAG: input_port -> object
  val OUTPUT_PORT_TAG: output_port -> object
  val UNSPECIFIED_TAG: unspec -> object
  
  val BOOL_UNTAG: object -> boolean
  val SYMBOL_UNTAG: object -> symbol
  val CHAR_UNTAG: object -> char
  val VECTOR_UNTAG: object -> vector
  val PAIR_UNTAG: object -> pair
  val NUMBER_UNTAG: object -> number
  val COMPLEX_UNTAG: object -> complex
  val REAL_UNTAG: object -> real
  val RATIONAL_UNTAG: object -> rational
  val INTEGER_UNTAG: object -> integer
  val NATURAL_UNTAG: object -> natural
  val STRING_UNTAG: object -> mstring
  val PROCEDURE_UNTAG: object -> procedure
  val LIST_UNTAG: object -> plist
  val INPUT_PORT_UNTAG: object -> input_port
  val OUTPUT_PORT_UNTAG: object -> output_port

  val is_boolean: object -> boolean
  val is_symbol: object -> boolean
  val is_char: object -> boolean
  val is_vector: object -> boolean
  val is_pair: object -> boolean
  val is_number: object -> boolean
  val is_string: object -> boolean
  val is_procedure: object -> boolean
  val is_list: object -> boolean
  val is_null: object -> boolean
  val is_input_port: object -> boolean
  val is_output_port: object -> boolean
  val is_eof_object: object -> boolean

  val not: boolean -> boolean
  val boolean2str: boolean -> string

  (* EQUIVALENCE PREDICATES *)
  
  val is_eq: object * object -> boolean
  val is_eqv: object * object -> boolean
  val is_equal: object * object -> boolean

  end
  
  
signature PAIR =
  sig

  val cons: object * object -> pair
  val car: pair -> object
  val cdr: pair -> object
  val set_car: pair * object -> unspec
  val set_cdr: pair * object -> unspec
  val caar: pair -> object
  val cadr: pair -> object
  val cdar: pair -> object
  val cddr: pair -> object
  val caaar: pair -> object
  val caadr: pair -> object
  val cadar: pair -> object
  val caddr: pair -> object
  val cdaar: pair -> object
  val cdadr: pair -> object
  val cddar: pair -> object
  val cdddr: pair -> object
  val caaaar: pair -> object
  val caaadr: pair -> object
  val caadar: pair -> object
  val caaddr: pair -> object
  val cadaar: pair -> object
  val cadadr: pair -> object
  val caddar: pair -> object
  val cadddr: pair -> object
  val cdaaar: pair -> object
  val cdaadr: pair -> object
  val cdadar: pair -> object
  val cdaddr: pair -> object
  val cddaar: pair -> object
  val cddadr: pair -> object
  val cdddar: pair -> object
  val cddddr: pair -> object
 
  end

signature LIST =
  sig

  val list: object list -> object
  and length: plist -> natural
  and append: object list -> object
  and reverse: plist -> plist
  and list_ref: plist * natural -> object
  val memq: object * plist -> plist
  and memv: object * plist -> plist
  and member: object * plist -> plist
  val assq: object * alist -> pair Option
  and assv: object * alist -> pair Option
  and assoc: object * alist -> pair Option

  end

signature SYMBOL =
  sig

  val symbol2string: symbol -> string
  val string2symbol: string -> symbol

  end

signature NUMBER =
  sig

  val is_complex: number -> boolean
  val is_real: number -> boolean
  val is_rational: number -> boolean
  val is_integer: number -> boolean
 
  val is_exact: complex -> boolean
  val is_inexact: complex -> boolean
  
  val eq: complex * complex * complex list -> boolean
  val lt: real * real * real list -> boolean
  val gt: real * real * real list -> boolean
  val le: real * real * real list -> boolean
  val ge: real * real * real list -> boolean
  
  val is_zero: complex -> boolean
  val is_positive: real -> boolean
  val is_negative: real -> boolean
  val is_odd: integer -> boolean
  val is_even: integer -> boolean
  
  val max: real * real list -> real
  val min: real * real list -> real
  
  val plus: complex list -> complex
  val mult: complex list -> complex
  val minus: complex * complex -> complex
  val divide: complex * complex -> complex
  val abs: real -> real
  
  val quotient: integer * integer -> integer
  val remainder: integer * integer -> integer
  val modulo: integer * integer -> integer
  
  val gcd: integer list -> integer
  val lcm: integer list -> integer
  
  val numerator: rational -> integer
  val denominator: rational -> natural

  val floor: real -> integer
  val ceiling: real -> integer
  val truncate: real -> integer
  val round: real -> integer
  val rationalize: real * real -> rational

  val exp: complex -> complex
  val log: complex -> complex
  val sin: complex -> complex
  val cos: complex -> complex
  val tan: complex -> complex
  val asin: complex -> complex
  val acos: complex -> complex
  val atan: complex -> complex
  val atanr: real * real -> real

  val sqrt: complex -> complex
  val expt: complex * complex -> complex
  val make_rectangular: real * real -> complex
  val make_polar: real * real -> complex
  val real_part: complex -> real
  val imag_part: complex -> real
  val magnitude: complex -> real
  val angle: complex -> real

  val exact2inexact: complex -> complex
  val inexact2exact: complex -> complex

  val NATURAL2RADIX: natural -> radix

  val number2string: number * radix -> string
  val string2number: string * radix -> number Option
  val number2str: number -> string
  val str2number: string -> number
  end

signature CHAR =
  sig

  val unspec_char: char
  val eof_char: char

  val char_eq: char * char -> boolean
  val char_lt: char * char -> boolean
  val char_gt: char * char -> boolean
  val char_le: char * char -> boolean
  val char_ge: char * char -> boolean
  val char_ci_eq: char * char -> boolean
  val char_ci_lt: char * char -> boolean
  val char_ci_gt: char * char -> boolean
  val char_ci_le: char * char -> boolean
  val char_ci_ge: char * char -> boolean
  val is_char_alphabetic: char -> boolean
  val is_char_numeric: char -> boolean
  val is_char_whitespace: char -> boolean
  val is_char_upper_case: char -> boolean
  val is_char_lower_case: char -> boolean
  val char2integer: char -> integer
  val integer2char: integer -> char
  val char_upcase: char -> char
  val char_downcase: char -> char
  val str2char: string -> char
  val char2str: char -> string
  end

signature STRING =
  sig

  val make_string: natural * char -> mstring
  val string: char list -> mstring
  val string_length: mstring -> natural
  val string_ref: mstring * natural -> char
  val string_set: mstring * natural * char -> unspec
  val string_eq: mstring * mstring -> boolean
  val string_ci_eq: mstring * mstring -> boolean
  val string_lt: mstring * mstring -> boolean
  val string_gt: mstring * mstring -> boolean
  val string_le: mstring * mstring -> boolean
  val string_ge: mstring * mstring -> boolean
  val string_ci_lt: mstring * mstring -> boolean
  val string_ci_gt: mstring * mstring -> boolean
  val string_ci_le: mstring * mstring -> boolean
  val string_ci_ge: mstring * mstring -> boolean
  val substring: mstring * natural * natural -> mstring
  val string_append: mstring list -> mstring

  end

signature VECTOR =
  sig

  val make_vector: natural * object -> vector
  val vector: object list -> vector
  val vector_length: vector -> natural
  val vector_ref: vector * natural -> object
  val vector_set: vector * natural * object -> unspec

  end

signature CONTROL =
  sig

  val apply: procedure * object * object list -> object
  val map: procedure * plist * plist list -> plist
  val for_each: procedure * plist * plist list -> unspec
  val call_with_current_continuation: procedure -> object

  end

signature IO =
  sig 

  val set_input_port: input_port -> unspec
  val set_output_port: output_port -> unspec

  val call_with_input_file: string * procedure -> object
  val call_with_output_file: string * procedure -> object
  val current_input_port: unit -> input_port
  val current_output_port: unit -> output_port
  val open_input_file: string -> input_port
  val open_output_file: string -> output_port
  val close_input_port: input_port -> unspec
  val close_output_port: output_port -> unspec
  val read_char: input_port -> char
  val peek_char: input_port -> char
  val read: input_port -> object
  val read_char_current: unit -> char
  val peek_char_current: unit -> char
  val read_current: unit -> object
  val newline: output_port -> unspec
  val newline_current: unit -> unspec
  val write_char: char * output_port -> unspec
  val write_char_current: char -> unspec
  val write: object * output_port -> unspec
  val write_current: object -> unspec
  val display: object * output_port -> unspec
  val display_current: object -> unspec

  end

signature ENVIRONMENT =
  sig
  type env
 
  val empty_env: env
  val add: env -> string * object -> env
  val push: env -> (string * object) list -> env
  val delete: env -> string -> env

  exception Lookup of string
  val lookup: env -> string -> object

  end

signature BASIS =
  sig
  val bindings: (string * object) list
  end

signature DATUM =
  sig 
  val read_datum: instream -> datum
  end

signature COMMAND =
  sig
  val parse: datum -> command
  end

signature EVAL =
  sig

  val eval: command -> object
  val run: input_port * output_port -> unspec
  val load: string -> unspec

  end
*)






