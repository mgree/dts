(*$ SCHEMEINPUTOUTPUT *)

signature SCHEMEINPUTOUTPUT =
sig

(* INPUT/OUTPUT

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Input/output routines for Scheme

*)

(* TYPES *)

type input_port
type output_port

type char
type sstring
type ('a, 'b) procedure

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

(* CONSTRUCTORS *)

val open_input_file: sstring -> input_port
val open_output_file: sstring -> output_port

(* STANDARD PROCEDURES *)

val call_with_input_file: sstring * (input_port, 'b) procedure -> 'b
val call_with_output_file: sstring * (output_port, 'b) procedure -> 'b

val current_input_port: unit -> input_port
val current_output_port: unit -> output_port

val with_input_from_file: sstring * (unit, 'b) procedure -> 'b
val with_output_to_file: sstring * (unit, 'b) procedure -> 'b

val close_input_port: input_port -> unit
val close_output_port: output_port -> unit

val read_char: input_port -> char
val peek_char: input_port -> char

val is_eof_object: char -> bool
val is_char_ready: input_port -> bool

val newline: output_port -> unit
val write_char: char * output_port -> unit

val load: sstring -> unit

val transcript_on: sstring -> unit
val transcript_off: unit -> unit

end


