open Types

signature CONVERSIO
  si
  val object2boolean: object -> boolean
  val mstring2string: mstring -> strin
  val string2mstring: string -> mstring
  val vector2list: vector -> pli
  end

signature ERROR 
  sig
  exception Unimplemented of strin
  and Impossible of string
  and ParseError of string * datum
  and TypeError of string * obje
  and InputError of string * object
  and IOError of string * string
  and EndOfFile

  end

signature OBJECT =
  sig

  val BOOL_TAG: boolean -> object
  val SYMBOL_TAG: symbol -> object
  val CHAR_TAG: char -> object
  val VECTOR_TAG: vector -> objec
  val PAIR_TAG: pair -> objec
  val NUMBER_TAG: number -> object
  val COMPLEX_TAG: complex -> object
  val REAL_TAG: real -> object
  val RATIONAL_TAG: rational -> object
  val INTEGER_TAG: integer -> object
  val NATURAL_TAG: natural -> object
  val STRING_TAG: mstring -> objec
  val PROCEDURE_TAG: procedure -> objec
  val LIST_TAG: plist -> object
  val INPUT_PORT_TAG: input_port -> objec
  val OUTPUT_PORT_TAG: output_port -> obje
  val UNSPECIFIED_TAG: unspec -> objec
  val BOOL_UNTAG: object -> boolean
  val SYMBOL_UNTAG: object -> symbol
  val CHAR_UNTAG: object -> char
  val VECTOR_UNTAG: object -> vecto
  val PAIR_UNTAG: object -> pair
  val NUMBER_UNTAG: object -> numbe
  val COMPLEX_UNTAG: object -> comple
  val REAL_UNTAG: object -> rea
  val RATIONAL_UNTAG: object -> rational
  val INTEGER_UNTAG: object -> integ
  val NATURAL_UNTAG: object -> natural
  val STRING_UNTAG: object -> mstrin
  val PROCEDURE_UNTAG: object -> procedure
  val LIST_UNTAG: object -> plist
  val INPUT_PORT_UNTAG: object -> input_port
  val OUTPUT_PORT_UNTAG: object -> output_por
  val is_boolean: object -> boolean
  val is_symbol: object -> boolean
  val is_char: object -> boolean
  val is_vector: object -> boolea
  val is_pair: object -> boolean
  val is_number: object -> boolean
  val is_string: object -> boolean
  val is_procedure: object -> boolean
  val is_list: object -> boole
  val is_null: object -> boolean
  val is_input_port: object -> boolean
  val is_output_port: object -> boolea
  val is_eof_object: object -> boolean

  val not: boolean -> boole
  val boolean2str: boolean -> string
  (* EQUIVALENCE PREDICATES *)
  val is_eq: object * object -> boolea
  val is_eqv: object * object -> boolea
  val is_equal: object * object -> boolean

  en
  
  
signature PAIR =
  sig

  val cons: object * object -> pair
  val car: pair -> object
  val cdr: pair -> object
  val set_car: pair * object -> unspec
  val set_cdr: pair * object -> unspe
  val caar: pair -> object
  val cadr: pair -> object
  val cdar: pair -> object
  val cddr: pair -> object
  val caaar: pair -> object
  val caadr: pair -> objec
  val cadar: pair -> object
  val caddr: pair -> object
  val cdaar: pair -> object
  val cdadr: pair -> object
  val cddar: pair -> object
  val cdddr: pair -> objec
  val caaaar: pair -> objec
  val caaadr: pair -> objec
  val caadar: pair -> object
  val caaddr: pair -> objec
  val cadaar: pair -> objec
  val cadadr: pair -> objec
  val caddar: pair -> object
  val cadddr: pair -> obje
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
  val list: object list -> objec
  and length: plist -> natural
  and append: object list -> object
  and reverse: plist -> plis
  and list_ref: plist * natural -> obje
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
signature NUMBER 
  si
  val is_complex: number -> boolea
  val is_real: number -> boolea
  val is_rational: number -> boolean
  val is_integer: number -> boolea
  val is_exact: complex -> boolean
  val is_inexact: complex -> boolean
  
  val eq: complex * complex * complex list -> boolean
  val lt: real * real * real list -> boolea
  val gt: real * real * real list -> boolean
  val le: real * real * real list -> boolean
  val ge: real * real * real list -> boole
  val is_zero: complex -> boolean
  val is_positive: real -> boolea
  val is_negative: real -> boolean
  val is_odd: integer -> boolean
  val is_even: integer -> boolean
  
  val max: real * real list -> real
  val min: real * real list -> real
  
  val plus: complex list -> comple
  val mult: complex list -> comple
  val minus: complex * complex -> complex
  val divide: complex * complex -> comple
  val abs: real -> rea
  
  val quotient: integer * integer -> intege
  val remainder: integer * integer -> integer
  val modulo: integer * integer -> integer
  
  val gcd: integer list -> integer
  val lcm: integer list -> integer
  
  val numerator: rational -> integer
  val denominator: rational -> natura
  val floor: real -> intege
  val ceiling: real -> integer
  val truncate: real -> intege
  val round: real -> intege
  val rationalize: real * real -> rationa
  val exp: complex -> complex
  val log: complex -> comple
  val sin: complex -> complex
  val cos: complex -> comple
  val tan: complex -> complex
  val asin: complex -> complex
  val acos: complex -> comple
  val atan: complex -> complex
  val atanr: real * real -> real
  val sqrt: complex -> comple
  val expt: complex * complex -> complex
  val make_rectangular: real * real -> complex
  val make_polar: real * real -> comple
  val real_part: complex -> real
  val imag_part: complex -> rea
  val magnitude: complex -> real
  val angle: complex -> rea
  val exact2inexact: complex -> complex
  val inexact2exact: complex -> complex

  val NATURAL2RADIX: natural -> radi
  val number2string: number * radix -> string
  val string2number: string * radix -> number Optio
  val number2str: number -> string
  val str2number: string -> number
  end

signature CHAR 
  sig

  val unspec_char: cha
  val eof_char: cha
  val char_eq: char * char -> boolean
  val char_lt: char * char -> boolean
  val char_gt: char * char -> boolea
  val char_le: char * char -> boole
  val char_ge: char * char -> boolean
  val char_ci_eq: char * char -> boolean
  val char_ci_lt: char * char -> boole
  val char_ci_gt: char * char -> boolea
  val char_ci_le: char * char -> boolean
  val char_ci_ge: char * char -> boolean
  val is_char_alphabetic: char -> boolea
  val is_char_numeric: char -> boolea
  val is_char_whitespace: char -> boolean
  val is_char_upper_case: char -> boolean
  val is_char_lower_case: char -> boole
  val char2integer: char -> integer
  val integer2char: integer -> cha
  val char_upcase: char -> char
  val char_downcase: char -> char
  val str2char: string -> cha
  val char2str: char -> string
  end

signature STRING 
  si
  val make_string: natural * char -> mstring
  val string: char list -> mstring
  val string_length: mstring -> natura
  val string_ref: mstring * natural -> char
  val string_set: mstring * natural * char -> unspe
  val string_eq: mstring * mstring -> boolea
  val string_ci_eq: mstring * mstring -> boolean
  val string_lt: mstring * mstring -> boolean
  val string_gt: mstring * mstring -> boolean
  val string_le: mstring * mstring -> boolea
  val string_ge: mstring * mstring -> boolean
  val string_ci_lt: mstring * mstring -> boolean
  val string_ci_gt: mstring * mstring -> boolea
  val string_ci_le: mstring * mstring -> boolean
  val string_ci_ge: mstring * mstring -> boolean
  val substring: mstring * natural * natural -> mstring
  val string_append: mstring list -> mstrin
  en
signature VECTOR 
  sig
  val make_vector: natural * object -> vecto
  val vector: object list -> vector
  val vector_length: vector -> natura
  val vector_ref: vector * natural -> object
  val vector_set: vector * natural * object -> unspe
  en
signature CONTROL =
  sig

  val apply: procedure * object * object list -> objec
  val map: procedure * plist * plist list -> plist
  val for_each: procedure * plist * plist list -> unspe
  val call_with_current_continuation: procedure -> objec
  en
signature IO 
  sig 

  val set_input_port: input_port -> unspec
  val set_output_port: output_port -> unspec

  val call_with_input_file: string * procedure -> objec
  val call_with_output_file: string * procedure -> object
  val current_input_port: unit -> input_por
  val current_output_port: unit -> output_port
  val open_input_file: string -> input_po
  val open_output_file: string -> output_port
  val close_input_port: input_port -> unspec
  val close_output_port: output_port -> unspec
  val read_char: input_port -> char
  val peek_char: input_port -> char
  val read: input_port -> objec
  val read_char_current: unit -> cha
  val peek_char_current: unit -> cha
  val read_current: unit -> object
  val newline: output_port -> unspec
  val newline_current: unit -> unspec
  val write_char: char * output_port -> unspe
  val write_char_current: char -> unspec
  val write: object * output_port -> unsp
  val write_current: object -> unspec
  val display: object * output_port -> unspec
  val display_current: object -> unspec

  end

signature ENVIRONMENT =
  sig
  type env
 
  val empty_env: env
  val add: env -> string * object -> en
  val push: env -> (string * object) list -> env
  val delete: env -> string -> en
  exception Lookup of strin
  val lookup: env -> string -> objec
  end
signature BASIS =
  sig
  val bindings: (string * object) list
  end
signature DATUM 
  si
  val read_datum: instream -> datum
  en
signature COMMAN
  sig
  val parse: datum -> comman
  en
signature EVAL 
  sig

  val eval: command -> object
  val run: input_port * output_port -> unspec
  val load: string -> unspec

  end

