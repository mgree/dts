(*$SCHEMEAST *)

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

type 'a Option
type dynamic
type number
type bool

type variable
type sequence 
type body
type case_clause
type binding_spec
type iteration_spec
    
datatype expression =
    LITERAL of dynamic |
    VARIABLE of variable |
    CALL of expression * expression list |
    LAMBDA of formals * body |
    IF of expression * expression * expression Option |  
    ASSIGN of variable * expression |
    COND of cond_clause_body |
    CASE of expression * case_clause_body |
    AND of expression list |	
    OR of expression list |
    LET of binding_spec list * body |
    NAMEDLET of variable * binding_spec list * body |
    LETS of binding_spec list * body |
    LETREC of binding_spec list * body |
    BEGIN of sequence |
    DO of iteration_spec list * expression * sequence * expression list |
    DELAY of expression |
    QUASIQUOTE of template 
and cond_clause_body =
    CONDCLAUSE of cond_clause * cond_clause_body |
    NULLCOND |
    CONDDEFAULT of sequence
and cond_clause =
    TESTSEQ of expression * sequence |
    TEST of expression |
    TESTREC of expression * expression
and case_clause_body = 
    CASECLAUSE of case_clause * case_clause_body |
    NULLCASE |
    CASEDEFAULT of sequence 
and definition =
    VARDEF of variable * expression |
    FUNDEF of variable * formals * body |
    BEGINDEF of definition list
and template =
    SIMPLETEMP of dynamic |
    LISTTEMP of template_or_splice list |
    ILISTTEMP of template_or_splice list * template |
    VECTEMP of template_or_splice list |
    UNQUOTE of expression 
and template_or_splice =
    TEMPLATE of template |
    UNQSPL of expression
and formals =
    VARPAR of variable |
    PAIRPAR of variable * formals |
    NULLPAR
    
datatype command_or_definition =
    COMMAND of expression |
    DEFINITION of definition



datatype datum =
      BOOLDAT of bool |
      CHARDAT of string |
      STRIDAT of string |
      SYMBDAT of string |
      NUMBDAT of number |
      VECTDAT of datum list |
      LISTDAT of datum list |
      ILISTDAT of datum list * datum


val read_datum: instream -> datum
val dat2command_or_definition: datum -> command_or_definition
    
end


(*$SchemeAst:  SCHEMEAST SchemeBool SchemeChar SchemeNumber SchemeString
               SchemeSymbol SchemeVector SchemePair SchemeList
	       SchemeProcedure SchemeGeneral SchemeDynamic *)

structure SchemeAst: SCHEMEAST =
  struct
  local open SchemeGeneral SchemeChar SchemeNumber SchemeDynamic 
             SchemeBool SchemeSymbol SchemeString SchemeList 
             SchemePair SchemeVector 
  in

  type 'a Option = 'a Option
  type variable = string
  type dynamic = dynamic
  type number = number
  type bool = bool
      
  datatype expression =
      LITERAL of dynamic |
      VARIABLE of variable |
      CALL of expression * expression list |
      LAMBDA of formals * body |
      IF of expression * expression * expression Option |  
      ASSIGN of variable * expression |
      COND of cond_clause_body |
      CASE of expression * case_clause_body |
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
      QUASIQUOTE of template 
  and cond_clause_body =
      CONDCLAUSE of cond_clause * cond_clause_body |
      NULLCOND |
      CONDDEFAULT of sequence
  and cond_clause =
      TESTSEQ of expression * sequence |
      TEST of expression |
      TESTREC of expression * expression
  and case_clause_body = 
      CASECLAUSE of case_clause * case_clause_body |
      NULLCASE |
      CASEDEFAULT of sequence 
  and definition =
      VARDEF of variable * expression |
      FUNDEF of variable * formals * body |
      BEGINDEF of definition list
  and template =
      SIMPLETEMP of dynamic |
      LISTTEMP of template_or_splice list |
      ILISTTEMP of template_or_splice list * template |
      VECTEMP of template_or_splice list |
      UNQUOTE of expression 
  and template_or_splice =
      TEMPLATE of template |
      UNQSPL of expression
  and formals =
      VARPAR of variable |
      PAIRPAR of variable * formals |
      NULLPAR
  withtype sequence = expression list * expression
  and body = definition list * sequence
  and case_clause = dynamic list * sequence
  and binding_spec = variable * expression
  and iteration_spec = variable * expression * expression Option
      
  datatype command_or_definition =
      COMMAND of expression |
      DEFINITION of definition

  datatype datum =
      BOOLDAT of bool |
      CHARDAT of string |
      STRIDAT of string |
      SYMBDAT of string |
      NUMBDAT of number |
      VECTDAT of datum list |
      LISTDAT of datum list |
      ILISTDAT of datum list * datum

  datatype token =
      Identifier of symbol
    | BoolSym of bool
    | NumbSym of number
    | CharSym of char
    | QuotationMark
    | LeftParen
    | RightParen
    | VectorSym
    | QuoteSym
    | QuasiquoteSym
    | UnquoteSym
    | UnqSplSym
    | DotSym
    | EndOfInput
  
  fun read_datum ip = 
      let val read_so_far = ref ""
	  fun read_error msg =
	      (input_line ip;
	       raise IllegalInput (msg, !read_so_far))
	  fun get_next_char() =
	      let val next_char = input(ip,1)
	      in
		 (read_so_far := !read_so_far ^ next_char; next_char)
	      end
	  fun get_identifier() =
	      let val next_char = lookahead ip 
	      in case next_char of
		  "" => ""
		| " " => ""
		| "\t" => ""
		| "\n" => ""
		| "(" => ""
		| ")" => ""
		| "\"" => ""
		| ";" => ""
		| _ => (get_next_char() ^ get_identifier())
	      end
	  fun get_string() =
	      let val c = get_next_char() 
	      in case c of
		  "\"" => ""
		| "\\" => get_next_char() ^ get_string() 
		| _ => c ^ get_string()
	      end
	  fun get_line() = input_line ip
          fun is_delimiter c = case c of
                                    ")" => true
                               |    "(" => true
                               |    "\"" => true
	                       |    ";" => true
                               |     _  => is_char_whitespace c
          fun is_digit c = member c ["0","1","2","3","4","5","6","7","8","9"]
          fun get_next_token() =
	      let val next_char = get_next_char()
	      in case next_char of
                   ""  => EndOfInput
		|  " " => get_next_token()
		| "\t" => get_next_token()
		| "\n" => get_next_token()
		| ";" => (get_line(); get_next_token()) (* SemiColon *)
		| "\"" => QuotationMark
		| "(" => LeftParen
		| ")" => RightParen
		| "`" => QuasiquoteSym
		| "'" => QuoteSym
		| "." => if is_delimiter (lookahead ip) then DotSym
                         else Identifier ("."^get_identifier())
			       
			    
		| "," => if lookahead ip = "@" then (get_next_char(); UnqSplSym)
			 else UnquoteSym
		| "#" => (case get_next_char() of
			      "(" => VectorSym
			    | "t" => BoolSym true
			    | "f" => BoolSym false
			    | "\\" => let val c = get_next_char()
				          val rem_chars = get_identifier()
				      in if rem_chars = ""
					     then CharSym c
					 else read_error "Illegal character constant"
				      end
			    | c => (Identifier (c ^ get_identifier())))  (* Non-standard identifier *)
		| "+" => let val lh = lookahead ip in
                             if lh = "." orelse is_digit lh then NumbSym (str2number (get_identifier()))
                             else Identifier ("+"^get_identifier()) (* Non-standard identifier *)
                         end
		| "-" => let val lh = lookahead ip in
                             if lh = "." orelse is_digit lh then NumbSym (str2number (get_identifier()))
                             else Identifier ("-"^get_identifier())  (* Non-standard identifier *)
                         end
		| "0" => NumbSym (str2number ("0" ^ get_identifier()))
		| "1" => NumbSym (str2number ("1" ^ get_identifier()))
		| "2" => NumbSym (str2number ("2" ^ get_identifier()))
		| "3" => NumbSym (str2number ("3" ^ get_identifier()))
		| "4" => NumbSym (str2number ("4" ^ get_identifier()))
		| "5" => NumbSym (str2number ("5" ^ get_identifier()))
		| "6" => NumbSym (str2number ("6" ^ get_identifier()))
		| "7" => NumbSym (str2number ("7" ^ get_identifier()))
		| "8" => NumbSym (str2number ("8" ^ get_identifier()))
		| "9" => NumbSym (str2number ("9" ^ get_identifier()))
		| c => Identifier (c ^ get_identifier())
	      end
	  fun parse_datum(tok) =
	      (* parses the input stream with tok prepended as the 
	       first token *)
	      case tok of
		  Identifier s => SYMBDAT s
		| BoolSym b => BOOLDAT b 
		| NumbSym n => NUMBDAT n
		| CharSym c => CHARDAT c
		| QuotationMark => STRIDAT (get_string())
		| LeftParen => parse_list()
		| VectorSym => VECTDAT (parse_vector())
		| QuoteSym => LISTDAT [SYMBDAT "quote",
					parse_datum (get_next_token())]
		| QuasiquoteSym =>
		      LISTDAT [SYMBDAT "quasiquote",
				parse_datum (get_next_token())]
		| UnquoteSym =>
		      LISTDAT [SYMBDAT "unquote",
				parse_datum (get_next_token())]
		| UnqSplSym =>
		      LISTDAT [SYMBDAT "unquote-splicing",
				parse_datum (get_next_token())]
		| _ => read_error "Illegal input"
	  and parse_list() =
	      let val tok = get_next_token()
	      in if tok = RightParen then
		  LISTDAT nil
		 else let val fd = parse_datum(tok)
		      in case parse_list_rem() of
			  LISTDAT l => LISTDAT (fd :: l)
			| ILISTDAT (l, f) => ILISTDAT (fd :: l, f)
			| _ => read_error "Manifestly impossible"
		      end
	      end
	  and parse_list_rem() =
	      let val tok = get_next_token()
	      in case tok of
		  RightParen => LISTDAT nil
		| DotSym => let val pd = parse_datum(get_next_token())
			    in if get_next_token() = RightParen
				   then ILISTDAT ([], pd)
			       else read_error "Illegal input: ) expected"
			    end
		| _ => let val fd = parse_datum(tok)
		       in case parse_list_rem() of
			   LISTDAT l => LISTDAT (fd :: l)
			 | ILISTDAT (l, f) => ILISTDAT (fd :: l, f)
			 | _ => read_error "Manifestly impossible"
		       end
	      end
	  and parse_vector() =
	      let val tok = get_next_token()
	      in case tok of 
		  RightParen => nil
		| _ => parse_datum(tok) :: parse_vector()
	      end
      in 
         let val tok = get_next_token() in
             case tok of 
                  EndOfInput => raise EOF
             |    _ => parse_datum(tok)
         end
      end



exception ParseError of string

  fun dat2dyn d =
      case d of
           BOOLDAT b => BOOL_TAG b
      |    CHARDAT c => CHAR_TAG (str2char c)
      |    STRIDAT s => STRING_TAG (str2sstring s)
      |    SYMBDAT s => SYMBOL_TAG (str2symbol s)
      |    NUMBDAT n => NUMBER_TAG n
      |    VECTDAT v => VECTOR_TAG (map dat2dyn v)
      |    ILISTDAT (datlst, dat) => 
                    ilist2pairlist (map dat2dyn datlst,dat2dyn dat)
      |    LISTDAT l => LIST_TAG (map dat2dyn l)

  and ilist2pairlist ([], dyn) = dyn
  |   ilist2pairlist ((x::tl), dyn) =  PAIR_TAG(x, ilist2pairlist (tl, dyn)) 

                      (* take parameter list to PAIRPAR-list *)
  fun parlist2pairpar ([], null) = null
  |   parlist2pairpar ((x::tl), null) = 
                      PAIRPAR (x, parlist2pairpar (tl, null))

                        (* take cond_clause list list to CONDCLAUSE-list *)
                          fun condlist2condcons ([], null) = null
  |   condlist2condcons ((x::tl), null) = 
                        CONDCLAUSE (x, condlist2condcons (tl, null))

                        (* take cond_clause list list to CONDCLAUSE-list *)
  fun caselist2casecons ([], null) = null
  |   caselist2casecons ((x::tl), null) = 
                        CASECLAUSE (x, caselist2casecons (tl,null))

  fun sig_err(mes,dat) = 
      let fun strlist2string l =
              case l of
                    [] => ""
              |    [x] => x
              | (x::tl) => x^" "^(strlist2string tl)
          fun unparse_datum dat =
              case dat of
                   BOOLDAT b => boolean2str b
              |    CHARDAT c => c
              |    STRIDAT s => s
              |    SYMBDAT s => s
              |    NUMBDAT n => number2str n
              |    VECTDAT dlist => 
                       "#("^(strlist2string (map unparse_datum dlist))^")"
              |    LISTDAT dlist => 
                       "("^(strlist2string (map unparse_datum dlist))^")"
              |    ILISTDAT (dlist,d) => 
                       "("^(strlist2string (map unparse_datum dlist))^" . "^
                                                        (unparse_datum d)^")"
          val illegal_argument = unparse_datum dat
      in
          raise IllegalInput(mes,illegal_argument)
      end

  fun stringsym s = (symbol2str o SYMBOL_UNTAG) s

  fun isdefinition d =
      (* decide if FIRST(d) is consistent with d being definition. *)
      (* Note the "begin" case and compare with production for    *)
      (* derived begin-expression.                                 *)
      case d of
         (LISTDAT((SYMBDAT "define")::tl)) => true
      |  (LISTDAT([SYMBDAT "begin"])) => true (* Empty def list *)
      |  (LISTDAT((SYMBDAT "begin")::def::deflist))  => isdefinition def 
      |  _  => false;

  fun split_else [] = ([],[])
  |   split_else [LISTDAT((SYMBDAT "else")::sequence)] = ([],sequence) 
  |   split_else (hd::tl) = 
                 let val (cond_part,else_part) = split_else tl
                 in
                     (hd::cond_part,else_part)
                 end

  fun char_str2dyn s = CHAR_TAG (str2char s)
  fun string2dyn   s = STRING_TAG (str2sstring s)
  fun sym_str2dyn  s = SYMBOL_TAG (str2symbol s)

  fun SyntacticKeyword s =  member s 

                 ["quote", "lambda", "if", "set!", "begin", "cond",
                  "and", "or", "case", "let", "let*", "letrec", "do",
                  "delay", "quasiquote", "else", "=>", "define", "unquote",
                  "unquote-splicing"]


  (* TRANSLATION FUNCTIONS *)

  fun Tvariable (s as (SYMBDAT symbol)) = 
                if not (SyntacticKeyword symbol)
                then (str2symbol symbol)
                else sig_err("Syntactic keyword used as variable",s)

  |   Tvariable s = sig_err("Expected a variable",s)
	 
  fun Tformals parlist par = parlist2pairpar ((map Tvariable parlist), par)
  
  fun Tquasiquotation (LISTDAT([SYMBDAT quasiquote, template])) =
                      QUASIQUOTE(Ttemplate(template)1)

  |   Tquasiquotation q = sig_err("Expected quasiquotation",q)

                (* <simple datum> *)
  and Ttemplate (BOOLDAT b)D = SIMPLETEMP (BOOL_TAG b)
  |   Ttemplate (CHARDAT c)D = SIMPLETEMP (char_str2dyn c)
  |   Ttemplate (NUMBDAT n)D = SIMPLETEMP (NUMBER_TAG n)
  |   Ttemplate (STRIDAT s)D = SIMPLETEMP (string2dyn s)
  |   Ttemplate (SYMBDAT s)D = SIMPLETEMP (sym_str2dyn s)

                (* <vector template> *)
  |   Ttemplate (VECTDAT(template_or_splice_lst))D =
                 VECTEMP(map (fn t => Ttemplate_or_splice t D)
                                      template_or_splice_lst)
    
                (* <unquotation D> *)
  |   Ttemplate (LISTDAT([SYMBDAT "unquote",template]))D =
                if D=1 then
                   UNQUOTE(Texp template)
                else
                   LISTTEMP([TEMPLATE(SIMPLETEMP(sym_str2dyn  "unquote")),
                   TEMPLATE(Ttemplate(template)(D-1))])

                (* <list template> *)
                (* <quasiquotation D+1> *)
  |   Ttemplate (LISTDAT([SYMBDAT "quasiquote", template]))D =
                 LISTTEMP([TEMPLATE(SIMPLETEMP(sym_str2dyn "quasiquote")),
                 TEMPLATE(Ttemplate(template)(D+1))])

                (* (quote <template D>) *)
  |   Ttemplate (LISTDAT([SYMBDAT "quote",template]))D =
                 LISTTEMP([TEMPLATE(SIMPLETEMP(sym_str2dyn "quote")),
                 TEMPLATE(Ttemplate(template)D)])
 
                (* (<template or splice D>* ) *)
  |   Ttemplate (LISTDAT(template_or_splice_lst))D =
                 LISTTEMP(map (fn t => Ttemplate_or_splice t D)
                                      template_or_splice_lst)

                (* (<template or splice D>+ . <template D> ) *)
  |   Ttemplate (ILISTDAT(template_or_splice_lst, template))D =
                ILISTTEMP(map (fn t => Ttemplate_or_splice t D)
                               template_or_splice_lst , Ttemplate template D)

  and Ttemplate_or_splice (LISTDAT([SYMBDAT "unquote-splicing", template]))D =
                          if D=1 then
                             UNQSPL(Texp template)
                           else
                              TEMPLATE(LISTTEMP([TEMPLATE(
                                  SIMPLETEMP(sym_str2dyn "unquote-splicing")),
                              TEMPLATE(Ttemplate(template)(D-1))]))

  |   Ttemplate_or_splice (template)D = 
                           TEMPLATE(Ttemplate(template)D)

  and Tdefinition (LISTDAT([SYMBDAT "define", 
                          variable as (SYMBDAT _), 
                          exp])) =
                   let val variable' = Tvariable variable
                       val exp'      = Texp exp
                   in
                       VARDEF(variable',exp')
                   end

  |   Tdefinition (def as (LISTDAT((SYMBDAT "define")::
                  (ILISTDAT(variable1::formals, variable2))::
                  body))) =
                  let val variable'   = Tvariable variable1
                      val variable''  = Tvariable variable2
                      val formals'    = Tformals formals (VARPAR variable'')
                      val body'       = Tbody body
                  in
                      (FUNDEF(variable',formals',body'))
                       handle ParseError mes => sig_err(mes,def)
                  end
                        
  |   Tdefinition (def as (LISTDAT((SYMBDAT "define")::
                          (LISTDAT(variable::formals))::
                          body))) =
	           let val variable' = Tvariable variable
                       val formals'  = Tformals formals NULLPAR
                       val body'     = Tbody body
                   in
                       (FUNDEF(variable',formals',body'))
                        handle ParseError mes => sig_err(mes,def)
                   end

  |   Tdefinition (LISTDAT((SYMBDAT "begin")::deflist)) =
                   let val deflist' = map Tdefinition deflist
                   in
                       BEGINDEF(deflist')
                   end

  |   Tdefinition d = sig_err("Expected definition",d)

           (* Note that Tbody is supposed to be 
              called with a list argument.*)
           (* singleton case *)
  and Tbody b = 
      case b of
           [exp]    => ([],([],Texp exp))
      |    (hd::tl) =>  if (isdefinition hd) then
                            let val (dlist,sequence) = Tbody(tl)
                                val d                = Tdefinition hd
                            in
                                (d::dlist,sequence)
                            end
                         else
                            let val (dlist,(explist,exp)) = Tbody(tl)
                                val exp'                  = Texp hd
                            in
                                (dlist,(exp'::explist,exp))
                            end     
      |   _         => raise ParseError "Illegal empty body"
   
                   (* (<test> => <recipient>) *)
  and Tcond_clause (LISTDAT([exp1,SYMBDAT "=>",exp2])) =
                   TESTREC(Texp exp1, Texp exp2)

                   (* (<test>) *)
  |   Tcond_clause (LISTDAT([exp])) =
                   TEST(Texp exp)

                   (* (<test> <sequence>) *)
  |   Tcond_clause (cc as (LISTDAT(test::exp_tl))) =
                   (TESTSEQ(Texp test,Tsequence exp_tl) 
                    handle ParseError mes => sig_err(mes,cc))

  |   Tcond_clause c = sig_err("Expected cond_clause",c)
      
  and Tcase_clause (cc as (LISTDAT(LISTDAT(datumlst)::sequence))) =
                   ((map dat2dyn datumlst,Tsequence sequence)
                    handle ParseError mes => sig_err(mes,cc))

  |   Tcase_clause c = sig_err("Expected case_clause",c)

  and Tsequence s = 
      case s of
           [exp]         => ([],Texp exp)
      |    (exp::exp_tl) => let val (explst,exp') = Tsequence exp_tl
                            in
                                ((Texp exp)::explst,exp')
                            end
      | _                => raise ParseError "Illegal empty sequence"
      
  and Tbindingspec (LISTDAT([variable,exp])) =
                   (Tvariable variable, Texp exp)

  |   Tbindingspec b = sig_err("Expected bindingspec",b)
          
  and Titerationspec (LISTDAT([variable, init, step])) =
                     (Tvariable variable, Texp init, Some(Texp step))

  |   Titerationspec (LISTDAT([variable, init])) =
                     (Tvariable variable, Texp init, None)

  |   Titerationspec i = sig_err("Expected iterationspec",i)
          
           (* <variable> *)
  and Texp (s as (SYMBDAT symbol)) = 
           if not (SyntacticKeyword symbol) then 
              (VARIABLE symbol)
           else 
               sig_err("Illegal syntactic keyword, expected expression",s)

           (* <literal> *)
           (* <self-evaluating> = <boolean>, <number>, <character>, <string> *)
  |   Texp (BOOLDAT b) = LITERAL (BOOL_TAG b)
  |   Texp (CHARDAT c) = LITERAL (CHAR_TAG (str2char c))
  |   Texp (NUMBDAT n) = LITERAL (NUMBER_TAG n)
  |   Texp (STRIDAT s) = LITERAL (STRING_TAG (str2sstring s))

           (* <quotation> *)
  |   Texp (LISTDAT([SYMBDAT "quote",datum])) =
            LITERAL (dat2dyn datum)

           (* <lambda expression> *)
           (* (lambda (<variable>* ) <body>) *)
  |   Texp (exp as (LISTDAT((SYMBDAT "lambda")::
                   (formals as (LISTDAT(lst)))::
                   body))) =
           (LAMBDA((Tformals lst NULLPAR), Tbody body)
           handle ParseError mes => sig_err(mes,exp))
    
           (* (lambda (<variable>+ . <variable>) <body>) *)
  |   Texp (exp as (LISTDAT((SYMBDAT "lambda")::
                  (formals as (ILISTDAT(lst,variable)))::
                  body))) =
            let val variable' = Tvariable variable
            in
                (LAMBDA(Tformals lst (VARPAR variable'),Tbody body)
                 handle ParseError mes => sig_err(mes,exp))
            end

           (* (lambda <variable> <body>) *)
  |   Texp (exp as (LISTDAT((SYMBDAT "lambda")::
                   (formals as (SYMBDAT variable))::
                   body))) =
           let val variable' = Tvariable formals
               val body'     = Tbody body
           in
               (LAMBDA(VARPAR variable', body')
               handle ParseError mes => sig_err(mes,exp))
           end 

           (* <conditional> *)
           (* (if <expression> <expression> <expression>) *)
  |   Texp (LISTDAT([SYMBDAT "if", test, consequent, alternate])) =
           IF(Texp test, Texp consequent, Some (Texp alternate))

           (* (if <expression> <expression>) *)
  |   Texp (LISTDAT([SYMBDAT "if", test, consequent])) =
            IF(Texp test, Texp consequent, None)

           (* <assignment> *)
           (* (set! <variable> <expression>) *)
  |   Texp (LISTDAT([SYMBDAT "set!", variable, expression])) =
            ASSIGN(Tvariable variable, Texp expression)

           (* <derived expression> *)
           (* 
              (cond <cond clause>+ )                 | 
              (cond <cond_clause>* (else <sequence>))
           *)
  |   Texp (c as (LISTDAT(SYMBDAT "cond"::cond_clauses))) =
           let val (cond_part,else_part) = split_else cond_clauses
           in
               if else_part = [] then
                  let val mp = 
                      if cond_part = [] then 
                     sig_err("Illegal empty conditional clause encountered",c) 
                     else map Tcond_clause cond_part
                  val ccbody = condlist2condcons (mp,NULLCOND)
               in
                  COND ccbody
               end
             else
               let val mp = map Tcond_clause cond_part
                   val seq = Tsequence else_part
                   val ccbody = condlist2condcons (mp, CONDDEFAULT seq)
               in
                   COND ccbody
               end
           end
    
           (*  (case <expression> <case clause>+ )                 |
               (case <expression> <case clause>* (else <sequence>)
            *)
  |   Texp (c as (LISTDAT(SYMBDAT "case"::exp::case_clauses))) =
            let val (case_part,else_part) = split_else case_clauses
            in
               if else_part = [] then
                  let val mp = if case_part = [] then 
	                     sig_err("Illegal empty case clause encountered",c)
                          else
		             map Tcase_clause case_part
                      val ccbody = caselist2casecons (mp, NULLCASE)
                  in
                      CASE(Texp exp, ccbody)
                  end
               else
                  let val mp = map Tcase_clause case_part
                      val seq = Tsequence else_part
                      val ccbody = caselist2casecons (mp, CASEDEFAULT seq)
                  in
                      CASE(Texp exp, ccbody)
                  end
            end  

           (*  (and <test>* ) *)
  |   Texp (LISTDAT(SYMBDAT "and"::tests)) = AND(map Texp tests)

           (*  (or <test>* ) *)
  |   Texp (LISTDAT(SYMBDAT "or"::tests)) = OR(map Texp tests)

           (*  (let (<binding spec>* ) <body>) *)
  |   Texp (exp as (LISTDAT(SYMBDAT "let":: 
                  LISTDAT(bindingspecs)::
                  body))) =
           (LET(map Tbindingspec bindingspecs, Tbody body)
            handle ParseError mes => sig_err(mes,exp))

           (*  (let <variable> (<binding spec>* ) <body>) *)
  |   Texp (exp as (LISTDAT(SYMBDAT "let"::
                  variable::
                  LISTDAT(bindingspecs)::
                  body))) =
           (NAMEDLET(Tvariable variable, 
                     map Tbindingspec bindingspecs,
                     Tbody body)
            handle ParseError mes => sig_err(mes,exp))

           (*  (let* (<binding spec>* ) <body>) *)
  |   Texp (exp as (LISTDAT(SYMBDAT "let*"::
                  LISTDAT(bindingspecs)::
                  body))) =
           (LETS(map Tbindingspec bindingspecs, Tbody body)
            handle ParseError mes => sig_err(mes, exp))

           (*  (letrec (<binding spec>* ) <body>) *)
  |   Texp (exp as (LISTDAT(SYMBDAT "letrec"::
                  LISTDAT(bindingspecs)::
                  body))) =
           (LETREC(map Tbindingspec bindingspecs, Tbody body)
            handle ParseError mes => sig_err(mes,exp))

           (*  (begin <sequence>) *)
  |   Texp (LISTDAT(SYMBDAT "begin"::sequence)) = BEGIN(Tsequence sequence)

           (* (do (<iteration spec>* )
              (<test> <sequence> )
               <command>* )
           *)
  |   Texp (LISTDAT(SYMBDAT "do"::
                  LISTDAT(iterationspecs)::
                  LISTDAT(test::sequence)::
                  commands)) =
            DO(map Titerationspec iterationspecs,
            Texp test,
            Tsequence sequence,
            map Texp commands)

           (* (delay <expression> ) *)
  |   Texp (LISTDAT([SYMBDAT "delay", exp])) = DELAY(Texp exp)

           (* <quasiquotation> *)
  |   Texp (lst as LISTDAT(SYMBDAT "quasiquote"::_ )) = Tquasiquotation lst

           (* <procedure call> *)
           (* (<expression> <expression>* ) *)
           (* 
              NB! It is essential that this case of the definition is
              the final (non-error) one, since its pattern is very
              admissive and the case should only apply if none of
              the others do.
            *)
  |   Texp (LISTDAT(operator::operandlist)) =
           CALL(Texp operator, map Texp operandlist)

  |   Texp e = sig_err("Illegal expression",e)
      
  fun Tcommand_or_definition cd =
                             if (isdefinition cd) then 
                                DEFINITION(Tdefinition cd)
                             else
                                COMMAND(Texp cd)

  fun dat2command_or_definition d = Tcommand_or_definition d
      
 




  end
  end

