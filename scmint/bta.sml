
(* PARSER FOR BTA-LANGUAGE :


   The following  CONCRETE SYNTAX is parsed into annotated abstract syntax:

   <exp> ::= <value> | <variable> | (lambda (<variable>) <exp>) |
             (<exp> <exp>) | (<primop> <exp0> ... <expn>) | 
             (fix <exp>) | (if <exp> <exp> <exp>)

   <value> ::= boolean | integer

   <primop> ::= + | - | * | / | = | > | < | and | or | not

   
   The format of the annotated abstract syntax is given by the datatype definition
   'a texp in signature BTAAST below.

   The parser is a function 

                   parse : (unit -> 'a) -> string -> 'a texp list

   taking a function f: (unit -> 'a) and a string s as input and giving a list
   of 'a texp elements as output. The function f must be a function which returns a
   reference to an element of the intended annotation type. The string s is the name
   of the file to be parsed. In case std_in is wanted one calls with the string "std_in".
   A valid input file has format <file> ::= <exp> | <exp> <file> where line breaks
   and white spaces are ignored. In case the file parses the result will be a list
   [<texp1> ... <texpn>] of parsed expressions. 

   EXAMPLE :

   Suppose we want to annotate the abstract syntax with elements of the type
   
        datatype annotationtype = NULLINFO | INFO of string
   
   Then we could define an annotation initialization function f as

        fun f () = ref NULLINFO

   and call the parser with

        parse f "<filename>"

*)

signature BTAAST =
sig  
  
  (* ANNOTATED ABSTRACT SYNTAX DATATYPE *)
   
  datatype primop = PLUS | MINUS | TIMES | DIV | EQUAL | GT | LT | OR | AND | NOT 
                   
  datatype value = INT  of int | BOOL  of bool

  type 'a texp 

  datatype 'a exp =
    VAR of string |
    CONST of value |
    LAMBDA of string * 'a texp |
    APP of 'a texp * 'a texp |
    IF of 'a texp * 'a texp * 'a texp |
    PRIMAPP of primop * 'a texp list |
    FIX of 'a texp
  
  exception SyntaxError

  (* PARSE FUNCTION *)

  val parse : (unit -> 'a) -> string -> 'a texp list

end




structure BtaAst : BTAAST   = 
struct
 
  local open SchemeGeneral SchemeAnnast SchemeAst in
    


  exception SyntaxError


(* DATATYPE FOR ANNOTATED EXPRESSIONS *)


  datatype primop = PLUS | MINUS | TIMES | DIV | EQUAL | GT | LT | OR | AND | NOT 
                   
  datatype value = INT  of int | BOOL  of bool


  datatype 'a exp =
    VAR of string |
    CONST of value |
    LAMBDA of string * 'a texp |
    APP of 'a texp * 'a texp |
    IF of 'a texp * 'a texp * 'a texp |
    PRIMAPP of primop * 'a texp list |
    FIX of 'a texp
  withtype 'a texp = 'a * 'a exp


  

(* PARSER TO ANNOTATED ABSTRACT SYNTAX *)


  fun t_exp e =

      let fun prim p =
              case p of
                   AVARIABLE(a, "+") => PLUS |
                   AVARIABLE(a, "-") => MINUS |
                   AVARIABLE(a, "*") => TIMES |
                   AVARIABLE(a, "/") => DIV |
                   AVARIABLE(a, "=") => EQUAL |
                   AVARIABLE(a, ">") => GT |
                   AVARIABLE(a, "<") => LT |
                   AVARIABLE(a, "not") => NOT |
                   _ => raise SyntaxError 
                   
          fun isprimop e =
              case e of
                   AVARIABLE(a, "+") => true |
                   AVARIABLE(a, "-") => true |
                   AVARIABLE(a, "*") => true |
                   AVARIABLE(a, "/") => true |
                   AVARIABLE(a, "=") => true |
                   AVARIABLE(a, ">") => true |
                   AVARIABLE(a, "<") => true |
                   AVARIABLE(a, "not") => true |
                   AOR _ => true |
                   AAND _ => true |
                   _ => false

          fun isboolconst v =
              case v of
                   "true" => true |
                   "false" => true |
                   _ => false

          fun boolconst v = 
              case v of
                   "true" => BOOL true |
                   "false" => BOOL false |
                   _ => raise SyntaxError
      in           
    
      case e of 

           ALITERAL (a, ad) => (a, CONST (t_const ad)) |
           AVARIABLE (a, v) => if (isprimop e) then raise SyntaxError else 
                                  if (isboolconst v) then (a, CONST (boolconst v)) else
                                     (a, VAR v) |
           AOR(a, ael) => (a, PRIMAPP(OR, map t_exp ael)) |
           AAND(a, ael) => (a, PRIMAPP(AND, map t_exp ael)) |
           ACALL (a, (AVARIABLE (a',"fix"),[ae])) => (a, FIX (t_exp ae)) |
           ACALL (a, (ae, ael)) => if (isprimop ae) then (a, PRIMAPP(prim ae, map t_exp ael))
                                   else (case ael of
                                        [ae'] => (a, APP(t_exp ae, t_exp ae')) |
                                         _   => raise SyntaxError) |        
           ALAMBDA (a, (par, 
                        ([],([],ae)))) => (a, LAMBDA(t_par par, t_exp ae)) |
           AIF (a, (ae,ae',Some ae'')) => (a, IF(t_exp ae, t_exp ae', t_exp ae'')) |
           
           _ => raise SyntaxError
      end

  and t_par par =
      case par of
           APAIRPAR(a, (v, ANULLPAR _)) => v |
           _ => raise SyntaxError        
      
  and t_const ad =
      case ad of
           ANUMBDAT n => INT n  |
           _ => raise SyntaxError

           
  and translate def_or_exp =
      case def_or_exp of
           ACOMMAND(a,ae) => t_exp ae |
      _ => raise SyntaxError
           

  fun pf_stdin f codlst = f(read_datum std_in) 


  fun pf_file f instream codlst =
       
            let val cod = f(read_datum instream) in
                    (pf_file f instream (codlst@[cod]))
                     handle EOF => 
                        (close_in instream ; codlst@[cod])
            end


  fun parse init infile = 
                                    
      (output(std_out,"Loading from "^infile^" ...\n");
      let val instream = if (infile = "std_in") then std_in else open_in infile
          val f = translate o (dat2annexp init)
          val codlst = 
              if (infile = "std_in") then [pf_stdin f []]
              else
                 pf_file f instream []
      in
          codlst
           
      end) 
      


end

end
