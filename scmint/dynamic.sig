(*$ SCHEMEDYNAMIC *)

signature SCHEMEDYNAMIC =
sig

(* DYNAMIC

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

General Scheme (dynamic) objects and generic routines

*)

(* TYPES *)

type dynamic

type bool
type symbol
type char
type 'a vector
type ('a, 'b) pair
type number
type sstring
type ('a, 'b) procedure

type 'a slist
type input_port
type output_port

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

exception InputError of string
exception EOF

val read: input_port -> dynamic
val write: dynamic * output_port -> unit
val display: dynamic * output_port -> unit

(* CONSTRUCTORS *)

val BOOL_TAG: bool -> dynamic
val SYMBOL_TAG: symbol -> dynamic
val CHAR_TAG: char -> dynamic
val VECTOR_TAG: dynamic vector -> dynamic
val PAIR_TAG: (dynamic, dynamic) pair -> dynamic
val NUMBER_TAG: number -> dynamic
val STRING_TAG: sstring -> dynamic
val PROCEDURE_TAG: (dynamic list, dynamic) procedure -> dynamic

val LIST_TAG: dynamic slist -> dynamic
val INPUT_PORT_TAG: input_port -> dynamic
val OUTPUT_PORT_TAG: output_port -> dynamic
val UNSPECIFIED_TAG: unit -> dynamic

val unspecified: dynamic

(* DESTRUCTORS *)

exception TypeError of string * dynamic

val BOOL_UNTAG: dynamic -> bool
val SYMBOL_UNTAG: dynamic -> symbol
val CHAR_UNTAG: dynamic -> char
val VECTOR_UNTAG: dynamic -> dynamic vector
val PAIR_UNTAG: dynamic -> (dynamic, dynamic) pair
val NUMBER_UNTAG: dynamic -> number
val STRING_UNTAG: dynamic -> sstring
val PROCEDURE_UNTAG: dynamic -> (dynamic list, dynamic) procedure

val LIST_UNTAG: dynamic -> dynamic slist
val INPUT_PORT_UNTAG: dynamic -> input_port
val OUTPUT_PORT_UNTAG: dynamic -> output_port

val typecase:
	{bool: (bool -> 'a), 
	 symbol: (symbol -> 'a),
	 char: (char -> 'a), 
	 vector: (dynamic vector -> 'a), 
	 pair: ((dynamic, dynamic) pair -> 'a), 
	 number: (number -> 'a), 
	 string: (sstring -> 'a),
	 procedure: (dynamic list, dynamic) procedure -> 'a, 
	 list: dynamic list -> 'a, 
	 input_port: input_port -> 'a, 
	 output_port: output_port -> 'a, 
	 otherwise: dynamic -> 'a} 
	-> dynamic -> 'a

(* TYPE TESTING PROCEDURES *)

val is_boolean: dynamic -> bool
val is_symbol: dynamic -> bool
val is_char: dynamic -> bool
val is_vector: dynamic -> bool
val is_pair: dynamic -> bool
val is_number: dynamic -> bool
val is_string: dynamic -> bool
val is_procedure: dynamic -> bool

val is_list: dynamic -> bool
val is_null: dynamic -> bool
val is_input_port: dynamic -> bool
val is_output_port: dynamic -> bool

(* CONVERSIONS *)

val dynamic2bool: dynamic -> bool

(* EQUALITY PREDICATES *)

val is_eq: dynamic * dynamic -> bool
val is_eqv: dynamic * dynamic -> bool
val is_equal: dynamic * dynamic -> bool

end
