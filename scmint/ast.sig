(*$ SCHEMEAST *)

signature SCHEMEAST =
sig

(* AST

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Scheme abstract syntax specification

*)

(* TYPES *)

type 'a option

type variable
type sequence 
type body
type case_clause
type binding_spec
type iteration_spec
type program

datatype datum =
    BOOLDAT of string |
    CHARDAT of string |
    NUMBDAT of string |
    STRIDAT of string |
    SYMBDAT of string |
    LISTDAT of datum list |
    ILISTDAT of datum list * datum |
    VECTDAT of datum list 

datatype expression =
    VARIABLE of variable |
    CALL of expression * expression list |
    LAMBDA of formals * body |
    IF of expression * expression * expression option|  
    ASSIGN of variable * expression |
    COND of cond_clause list * sequence option |
    CASE of expression * case_clause list * sequence option |
    AND of expression list |	
    OR of expression list |
    LET of binding_spec list * body |
    NAMEDLET of variable * binding_spec list * body |
    LETS of binding_spec list * body |
    LETREC of binding_spec list * body |
    BEGIN of sequence |
    DO of iteration_spec list * expression * sequence * 
    	  expression list |
    DELAY of expression |
    QUOTE of datum |
    QUASIQUOTE of template 
  and cond_clause =
    TESTSEQ of expression * sequence |
    TEST of expression |
    TESTREC of expression * expression
  and definition =
    VARDEF of variable * expression |
    FUNDEF of variable * formals * body |
    BEGINDEF of definition list
  and template =
    SIMPLETEMP of datum |
    LISTTEMP of template_or_splice list |
    ILISTTEMP of template_or_splice list * template |
    VECTEMP of template_or_splice list |
    UNQUOTE of expression 
  and template_or_splice =
    TEMPLATE of template |
    UNQSPL of expression
  and formals =
    FORMALS of variable list |
    DOTTED of variable list * variable 

datatype command_or_definition =
    COMMAND of expression |
    DEFINITION of definition

end


