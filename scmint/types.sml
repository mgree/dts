structure Types =
  struct

  (* TYPES *)

  datatype 'a Option =
      Some of 'a
    | No
  datatype datum 
      BOOLDAT of boo
      CHARDAT of string |
      STRIDAT of string |
      SYMBDAT of strin
      NUMBDAT of string 
      VECTDAT of datum list |
      PAIRDAT of datum * datum |   
      NILDAT  
  datatype expression 
      LITERAL of datum |
      VARIABLE of string 
      CALL of expression * expression list |
      LAMBDA of formals * (definition list * expression list) |
      IF of expression * expression * expression |  
      ASSIGN of (string * expression) |
      COND of cond_clause_body |
      CASE of expression * case_clause_body |
      AND of expression list |	
      OR of expression list 
      LET of (string * expression) list * (definition list * expression list) |
      NAMEDLET of string * (string * expression) list 
  		(definition list * expression list) 
      LETS of (string * expression) list * (definition list * expression list) |
      LETREC of (string * expression) list * (definition list * expression list) |
      BEGIN of expression list |
      DO of (string * expression * expression) list * (expression * 
  	  	expression list) * expression list |
      DELAY of expression |
      QUASIQUOTE of template |
      UNDEFEXP
  and cond_clause_body =
      CONDCLAUSE of cond_clause * cond_clause_body |
      NULLCOND |
      CONDDEFAULT of expression list
  and cond_clause =
      TESTSEQ of expression * expression list |
      TEST of expression |
      TESTREC of expression * expression
  and case_clause_body = 
      CASECLAUSE of (datum list * expression list) * case_clause_body 
      NULLCASE 
      CASEDEFAULT of expression lis
  and definition =
      VARDEF of string * expression 
      FUNDEF of string * formals * (definition list * expression list) |
      BEGINDEF of definition list
  and template 
      SIMPLETEMP of datum |
      PAIRTEMP of template_or_splice * template 
      VECTTEMP of template_or_splice list |
      UNQUOTE of expression 
  and template_or_splice =
      TEMPLATE of template |
      SPLICE of expressio
  and formals =
      VARPAR of string |
      PAIRPAR of string * formals |
      NULLPAR
      
  datatype command 
      EXP of expression 
      DEF of definition

  datatype mstring = 
    FIXED of string |
    MUTABLE of string ref lis
  datatype dynamic =
      BOOL of boolean | 
      CHAR of char |
      STRING of mstring |
      SYMBOL of symbol |
      NUMBER of number |
      VECTOR of vector 
      PAIR of pair 
      PROCEDURE of procedure |
      LIST of plist |
      INPUT_PORT of input_port |
      OUTPUT_PORT of output_port |
      UNSPECIFIE
  withtype object = dynamic re
  and boolean = bool
  and char = string
  and symbol = strin
  and number = int
  and vector = object Vector.vector
  and pair = object * object
  and procedure = object list -> object
  and plist = object list
  and input_port = instrea
  and output_port = outstream

  type alist = (object * object) list
  and complex = int
  and real = int
  and rational = in
  and integer = int
  and natural = int
  and radix = int
  and unspec = un
  end
