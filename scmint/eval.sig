(*$ SCHEMEEVAL *)

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
type datum
type expression
type formals
type definition
type command_or_definition
type program

val eval_datum: datum -> dynamic
exception ArgumentMismatch of formals * dynamic list
val eval_exp: env -> expression -> dynamic
val eval_definition: definition -> unit
val eval_command_or_definition: command_or_definition -> dynamic
val eval_program: program -> unit

end
