(*$SCHEMEINPUTOUTPUT *)

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
sharing type input_port = instream and type output_port = outstream

type char
type 'a Option

(* CONSTRUCTORS *)

val open_input_file: string -> input_port
val open_output_file: string -> output_port

(* STANDARD PROCEDURES *)

val call_with_input_file: string * (input_port -> 'b) -> 'b
val call_with_output_file: string * (output_port -> 'b) -> 'b

val current_input_port: unit -> input_port
val current_output_port: unit -> output_port

val with_input_from_file: string * (unit -> 'b) -> 'b
val with_output_to_file: string * (unit -> 'b) -> 'b

val close_input_port: input_port -> unit
val close_output_port: output_port -> unit

val read_char: input_port -> char Option
val peek_char: input_port -> char Option

val is_eof_object: char Option -> bool
val is_char_ready: input_port -> bool

val newline: output_port -> unit
val write_char: char * output_port -> unit

val transcript_on: string -> unit
val transcript_off: unit -> unit

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

end


(*$SchemeInputOutput: SCHEMEINPUTOUTPUT SchemeGeneral
              SchemeChar SchemeString SchemeProcedure *)

structure SchemeInputOutput: SCHEMEINPUTOUTPUT =
  struct
  local open SchemeGeneral SchemeChar SchemeString SchemeProcedure in

  type input_port = instream
  type output_port = outstream

  type char = char
  type 'a Option = 'a Option

  val open_input_file = open_in 
  val open_output_file = open_out

  fun call_with_input_file (s, p) = p (open_in s)
  fun call_with_output_file (s, p) = p (open_out s)

  fun current_input_port () = std_in (* this is wrong!! *)
  fun current_output_port () = std_out (* this is wrong!! *)
  
  fun with_input_from_file (s, p) = raise Unimplemented "with_input_from_file"
  fun with_output_to_file (s, p) = raise Unimplemented "with_output_to_file"

  val close_input_port = close_in
  val close_output_port = close_out

  fun read_char ip = 
      let val ic = input (ip, 1)
      in if ic = "" then None else Some ic
      end 

  fun peek_char ip = 
      let val ic = lookahead ip
      in if ic = "" then None else Some ic
      end
  
  fun is_eof_object None = true
    | is_eof_object (Some _) = false

  fun is_char_ready ip = raise Unimplemented "is_char_ready"

  fun newline oport = output (oport, "\n")
  fun write_char (c, oport) = output (oport, c)

  fun transcript_on ss = raise Unimplemented "transcript_on"
  fun transcript_off () = raise Unimplemented "transcript_off"
  end
  end 
