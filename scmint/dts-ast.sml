(*$ DTSAST *)

signature DTSAST =
sig

(* AST

Created by: Fritz Henglein, Jakob Rehof,
            DIKU, University of Copenhagen, 
	    henglein@diku.dk, rehof@diku.dk
Date: 13 Dec 1993

Maintenance: Authors

DESCRIPTION

DTS abstract syntax specification

*)

end

(*$ DTSAst *)

structure DTSAst: DTSAST =
  struct

local open SchemeGeneral 
in

datatype 'a dat =
      BOOLDAT of bool |
      CHARDAT of string |
      STRIDAT of string |
      SYMBDAT of string |
      NUMBDAT of string |
      VECTDAT of 'a tdatum list |
      LISTDAT of 'a tdatum list |
      ILISTDAT of 'a tdatum list * 'a tdatum
withtype 'a tdatum = 'a * 'a dat


type typevar = string (* only a suggestion *)
type coercionvar = string (* only a suggestion *)
type typevariable = string
type 'a tvariable = 'a * string
(* This declaration is only a suggestion,                *)
(* and nothing should depend on its internal structure   *) 
datatype typeexp =
    TYVAR of typevariable |
    TNUMBER |
    TBOOLEAN |
    TVECTOR |
    TSTRING |
    TSYMBOL |
    TCHARACTER |
    TPAIR of typeexp * typeexp |
    TPROCEDURE of typeexp * typeexp | 
    TLIST of typevariable 


datatype coercion =
    BOOL_TAG |
    NUMBER_TAG |
    (* ... and so on ... *)
    COMP of coercion * coercion |
    ARROW of coercion * coercion 
    

datatype ('a ,'b) al     = ANIL | ACONS of 'b * ('a ,'b) alist
withtype ('a ,'b) alist = 'a * ('a ,'b) al


datatype 'a texp =
    TLITERAL of 'a tdatum | 
    TVARIABLE of 'a tvariable |
    TCALL of 'a texpression * ('a ,'a texpression) alist |
    TLAMBDA of 'a tformals * 'a tbody |
    TIF of 'a texpression * 'a texpression * 'a texpression Option |
    TASSIGN of 'a tvariable * 'a texpression |
    TCOND of 'a tcond_clause_body |
    TCASE of 'a texpression * 'a tcase_clause_body |
    TAND of 'a texpression list |
    TOR of 'a texpression list |
    TLET of 'a tbinding_spec list * 'a tbody |
    TNAMEDLET of 'a tvariable * 'a tbinding_spec list * 'a tbody |
    TLETS of 'a tbinding_spec list * 'a tbody |
    TLETREC of 'a tbinding_spec list * 'a tbody |
    TBEGIN of 'a tsequence |
    TDO of 'a titeration_spec list * 'a texpression * 'a tsequence * 
      'a texpression list |
    TDELAY of 'a texpression |
    TQUASIQUOTE of 'a ttemplate 
and 'a tcond_clause_body =
    TCONDCLAUSE of 'a tcond_clause * 'a tcond_clause_body |
    TNULLCOND |
    TCONDDEFAULT of 'a tsequence
and 'a tcond_clause =
    TTESTSEQ of 'a texpression * 'a tsequence |
    TTEST of 'a texpression |
    TTESTREC of 'a texpression * 'a texpression
and 'a tcase_clause_body = 
    TCASECLAUSE of 'a tcase_clause * 'a tcase_clause_body |
    TNULLCASE |
    TCASEDEFAULT of 'a tsequence 
and 'a tdefinition =
    TVARDEF of 'a tvariable * 'a texpression |
    TFUNDEF of 'a tvariable * 'a tformals * 'a tbody |
    TBEGINDEF of 'a tdefinition list
and 'a ttemplate =
    TSIMPLETEMP of 'a tdatum |
    TLISTTEMP of 'a ttemplate_or_splice list |
    TILISTTEMP of 'a ttemplate_or_splice list * 'a ttemplate |
    TVECTEMP of 'a ttemplate_or_splice list |
    TUNQUOTE of 'a texpression 
and 'a ttemplate_or_splice =
    TTEMPLATE of 'a ttemplate |
    TUNQSPL of 'a texpression
and 'a tforms =
    TVARPAR of 'a tvariable |
    TPAIRPAR of 'a tvariable * 'a tformals |
    TNULLPAR

withtype 'a texpression = 'a * 'a texp 
and 'a tsequence = 'a texpression list * 'a texpression
and 'a tbody = 'a tdefinition list * 'a tsequence
and 'a tcase_clause = 'a tdatum list * 'a tsequence
and 'a tbinding_spec = 'a tvariable * 'a texpression
and 'a titeration_spec = 'a tvariable * 'a texpression * 'a texpression Option
and 'a tformals = 'a * 'a tforms

datatype 'a tcommand_or_definition =
      TCOMMAND of 'a texpression |
      TDEFINITION of 'a tdefinition
       

  
end
end




