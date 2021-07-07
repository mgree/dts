(* use "/home/fenris/dts/scmint/types.sml"; 
use "/home/fenris/dts/scmint/scheme.sig"; 
use "/home/fenris/dts/scmint/error.sml"; 
use "/home/fenris/dts/scmint/datum.sml"; 
use "/home/fenris/dts/scmint/command.sml";

val parse = Command.parse o Datum.read_datum;
*)

(* Types for annotated commands, expressions, etc. *)

  local val counter = ref 0
  in 
  fun new_att() = (counter := !counter + 1; !counter)
  end


  type 'v var = string * 'v

  datatype 'a dat =
      BOOLDAT of bool |
      CHARDAT of string |
      STRIDAT of string |
      SYMBDAT of string |
      NUMBDAT of string |
      VECTDAT of 'a datum list |
      PAIRDAT of 'a datum * 'a datum |   
      NILDAT 
  withtype
      'a datum = 'a dat * 'a

  datatype 'a exp =
      LITERAL of 'a datum |
      VARIABLE of 'a var |
      CALL of 'a expression * 'a expression list |
      LAMBDA of 'a formals * ('a definition list * 'a expression list) |
      IF of 'a expression * 'a expression * 'a expression |  
      ASSIGN of 'a var * 'a expression |
      COND of 'a cond_clause_body |
      CASE of 'a expression * 'a case_clause_body |
      AND of 'a expression list |	
      OR of 'a expression list |
      LET of ('a var * 'a expression) list * ('a definition list * 'a expression list) |
      NAMEDLET of 'a var * ('a var * 'a expression) list * 
  		('a definition list * 'a expression list) |
      LETS of ('a var * 'a expression) list * ('a definition list * 'a expression list) |
      LETREC of ('a var * 'a expression) list * ('a definition list * 'a expression list) |
      BEGIN of 'a expression list |
      DO of ('a var * 'a expression * 'a expression) list * ('a expression * 
  	  	'a expression list) * 'a expression list |
      DELAY of 'a expression |
      QUASIQUOTE of 'a template |
      ID of 'a expression |
      UNDEFEXP 
  and 'a cond_clause_body =
      CONDCLAUSE of 'a cond_clause * 'a cond_clause_body |
      NULLCOND |
      CONDDEFAULT of 'a expression list
  and 'a cond_clause =
      TESTSEQ of 'a expression * 'a expression list |
      TEST of 'a expression |
      TESTREC of 'a expression * 'a expression
  and 'a case_clause_body = 
      CASECLAUSE of ('a datum list * 'a expression list) * 'a case_clause_body |
      NULLCASE |
      CASEDEFAULT of 'a expression list 
  and 'a definition =
      VARDEF of 'a var * 'a expression |
      FUNDEF of 'a var * 'a formals * ('a definition list * 'a expression list) |
      BEGINDEF of 'a definition list
  and 'a temp =
      SIMPLETEMP of 'a datum |
      PAIRTEMP of 'a template_or_splice * 'a template |
      VECTTEMP of 'a template_or_splice list |
      UNQUOTE of 'a expression
  and 'a temp_or_spl =
      TEMPLATE of 'a template |
      SPLICE of 'a expression
  and 'a form =
      VARPAR of 'a var |
      PAIRPAR of 'a var * 'a formals |
      NULLPAR
  withtype
      'a expression = 'a exp * 'a
  and 'a template = 'a temp * 'a
  and 'a template_or_splice = 'a temp_or_spl * 'a
  and 'a formals = 'a form * 'a

  datatype 'a command =
      EXP of 'a expression |
      DEF of 'a definition
