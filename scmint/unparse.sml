signature SCHEMEANNAST = 
sig

type 'a Option
type datum
type infotype
type 'a anncommand_or_definition
type pptype

type ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j) fcn_record

type ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l) afcn_record

val Pdat2command_or_definition : ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j) fcn_record -> datum -> 'e

val dat2annexp : datum -> infotype ref anncommand_or_definition

val mapacommand_or_definition : ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l) afcn_record 
                                -> 'i anncommand_or_definition -> 'f


val unparse_anncommand_or_definition : infotype ref anncommand_or_definition -> pptype

(* test function *)

val annotate_and_unparse_file : string -> string -> unit 


val pp : outstream * pptype * int -> unit

end



(*------------------- Parametrized version -----------------------------------------------------*)



structure SchemeAnnast : SCHEMEANNAST    =
  struct
  local open SchemeGeneral SchemeNumber SchemeBool 
             SchemeAst
  in

type 'a Option = 'a Option
type datum = datum


type ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j) fcn_record =
   {and_fcn:'a list -> 'a,
    assign_fcn:string * 'a -> 'a,
    begin_fcn:'a list * 'a -> 'a,
    begindef_fcn:'b list -> 'b,
    call_fcn:'a * 'a list -> 'a,
    case_fcn:'a * 'c -> 'a,
    caseclause_fcn:('d list * ('a list * 'a)) * 'c -> 'c,
    casedefault_fcn:'a list * 'a -> 'c,
    command_fcn:'a -> 'e, 
    cond_fcn:'f -> 'a,
    condclause_fcn:'g * 'f -> 'f,
    conddefault_fcn:'a list * 'a -> 'f,
    datum_fcn:datum -> 'd,
    definition_fcn:'b -> 'e,
    delay_fcn:'a -> 'a,
    do_fcn:(string * 'a * 'a Option) list * 'a * ('a list * 'a) * 'a list -> 'a,
    fundef_fcn:string * 'h * ('b list * ('a list * 'a)) -> 'b,
    if_fcn:'a * 'a * 'a Option -> 'a,
    ilisttemp_fcn:'i list * 'j -> 'j,
    lambda_fcn:'h * ('b list * ('a list * 'a)) -> 'a,
    let_fcn:(string * 'a) list * ('b list * ('a list * 'a)) -> 'a,
    letrec_fcn:(string * 'a) list * ('b list * ('a list * 'a)) -> 'a,
    lets_fcn:(string * 'a) list * ('b list * ('a list * 'a)) -> 'a,
    listtemp_fcn:'i list -> 'j,
    literal_fcn:'d -> 'a,
    namedlet_fcn:string * (string * 'a) list * ('b list * ('a list * 'a)) -> 'a, 
    nullcase_fcn:'c,
    nullcond_fcn:'f, 
    nullpar_fcn:'h,
    or_fcn:'a list -> 'a,
    pairpar_fcn:string * 'h -> 'h,
    quasiquote_fcn:'j -> 'a,
    simpletemp_fcn:'d -> 'j,
    template_fcn:'j -> 'i, 
    test_fcn:'a -> 'g,
    testrec_fcn:'a * 'a -> 'g,
    testseq_fcn:'a * ('a list * 'a) -> 'g,
    unqspl_fcn:'a -> 'i,
    unquote_fcn:'a -> 'j,
    vardef_fcn:string * 'a -> 'b,
    variable_fcn:string -> 'a,
    varpar_fcn:string -> 'h,
    vectemp_fcn:'i list -> 'j} 
     

(*

 {literal_fcn ,
  variable_fcn ,
  call_fcn ,
  lambda_fcn ,
  if_fcn ,
  assign_fcn ,
  cond_fcn ,
  case_fcn ,
  and_fcn ,
  or_fcn  ,
  let_fcn ,
  namedlet_fcn ,
  lets_fcn ,
  letrec_fcn ,
  begin_fcn ,
  do_fcn ,
  delay_fcn ,
  quasiquote_fcn ,
  condclause_fcn ,
  nullcond_fcn ,
  conddefault_fcn ,
  testseq_fcn ,
  test_fcn ,
  testrec_fcn ,
  caseclause_fcn ,
  nullcase_fcn ,
  casedefault_fcn ,
  vardef_fcn ,
  fundef_fcn ,
  begindef_fcn ,
  simpletemp_fcn ,
  listtemp_fcn ,
  ilisttemp_fcn ,
  vectemp_fcn ,
  unquote_fcn ,
  template_fcn ,
  unqspl_fcn ,
  varpar_fcn ,
  pairpar_fcn ,
  nullpar_fcn ,
  command_fcn ,
  definition_fcn ,
  datum_fcn}

*)




  exception ParseError of string

  fun strlist2string l =
              case l of
                    [] => ""
              |    [x] => x
              | (x::tl) => x^" "^(strlist2string tl)


  fun sig_err(mes,dat) = 
      let fun unparse_datum dat =
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


  
  fun SyntacticKeyword s =  member s 

                 ["quote", "lambda", "if", "set!", "begin", "cond",
                  "and", "or", "case", "let", "let*", "letrec", "do",
                  "delay", "quasiquote", "else", "=>", "define", "unquote",
                  "unquote-splicing"]

  fun split_else [] = ([],[])
  |   split_else [LISTDAT((SYMBDAT "else")::sequence)] = ([],sequence) 
  |   split_else (hd::tl) = 
                 let val (cond_part,else_part) = split_else tl
                 in
                     (hd::cond_part,else_part)
                 end


  fun isdefinition d =
      (* decide if FIRST(d) is consistent with d being definition. *)
      (* Note the "begin" case and compare with production for    *)
      (* derived begin-expression.                                 *)
      case d of
         (LISTDAT((SYMBDAT "define")::tl)) => true
      |  (LISTDAT([SYMBDAT "begin"])) => true (* Empty def list *)
      |  (LISTDAT((SYMBDAT "begin")::def::deflist))  => isdefinition def 
      |  _  => false;


                      (* take parameter list to PAIRPAR-list *)
  fun Pparlist2pairpar (F : ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j) fcn_record) ([], null) = null
  |   Pparlist2pairpar F ((x::tl), null) = 
                      (#pairpar_fcn F) (x, Pparlist2pairpar F (tl, null))

                        (* take cond_clause list list to CONDCLAUSE-list *)
  fun Pcondlist2condcons (F : ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j) fcn_record) ([], null) = null
  |   Pcondlist2condcons F ((x::tl), null) = 
                        (#condclause_fcn F) (x, Pcondlist2condcons F (tl, null))

                        (* take cond_clause list list to CONDCLAUSE-list *)
  fun Pcaselist2casecons (F : ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j) fcn_record) ([], null) = null
  |   Pcaselist2casecons F ((x::tl), null) = 
                        (#caseclause_fcn F) (x, Pcaselist2casecons F (tl,null))







  (* TRANSLATION FUNCTIONS *)

  fun PTvariable (F : ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j) fcn_record) (s as (SYMBDAT symbol)) = 
                if not (SyntacticKeyword symbol)
                then symbol
                else sig_err("Syntactic keyword used as variable",s)

  |   PTvariable F s = sig_err("Expected a variable",s)
	 
  fun PTformals F parlist par = Pparlist2pairpar F ((map (PTvariable F) parlist), par)
  
  fun PTquasiquotation F (LISTDAT([SYMBDAT quasiquote, template])) =
                      (#quasiquote_fcn F) (PTtemplate F template 1)

  |   PTquasiquotation F q = sig_err("Expected quasiquotation",q)

                (* <simple datum> *)
  and PTtemplate F (x as (BOOLDAT _)) D = (#simpletemp_fcn F) ((#datum_fcn F) x)
  |   PTtemplate F (x as (CHARDAT _)) D = (#simpletemp_fcn F) ((#datum_fcn F) x) 
  |   PTtemplate F (x as (NUMBDAT _)) D = (#simpletemp_fcn F) ((#datum_fcn F) x)
  |   PTtemplate F (x as (STRIDAT _)) D = (#simpletemp_fcn F) ((#datum_fcn F) x)
  |   PTtemplate F (x as (SYMBDAT _)) D = (#simpletemp_fcn F) ((#datum_fcn F) x)

                (* <vector template> *)
  |   PTtemplate F (VECTDAT(template_or_splice_lst))D =
                 (#vectemp_fcn F) (map (fn t => PTtemplate_or_splice F t D)
                                      template_or_splice_lst)
    
                (* <unquotation D> *)
  |   PTtemplate F (LISTDAT([SYMBDAT "unquote",template]))D =
                if D=1 then
                   (#unquote_fcn F) (PTexp F template)
                else
                   (#listtemp_fcn F) ([(#template_fcn F) 
                                          ((#simpletemp_fcn F)
                                               ((#datum_fcn F) (SYMBDAT  "unquote"))),
                   (#template_fcn F) (PTtemplate F template (D-1))])

                (* <list template> *)
                (* <quasiquotation D+1> *)
  |   PTtemplate F (LISTDAT([SYMBDAT "quasiquote", template]))D =
                 (#listtemp_fcn F) ([(#template_fcn F) 
                                        ((#simpletemp_fcn F)
                                             ((#datum_fcn F) (SYMBDAT "quasiquote"))),
                 (#template_fcn F) (PTtemplate F template (D+1))])

                (* (quote <template D>) *)
  |   PTtemplate F (LISTDAT([SYMBDAT "quote",template]))D =
                 (#listtemp_fcn F) ([(#template_fcn F)
                                         ((#simpletemp_fcn F)
                                              ((#datum_fcn F) (SYMBDAT "quote"))),
                 (#template_fcn F) (PTtemplate F template D)])
 
                (* (<template or splice D>* ) *)
  |   PTtemplate F (LISTDAT(template_or_splice_lst))D =
                 (#listtemp_fcn F) (map (fn t => PTtemplate_or_splice F t D)
                                      template_or_splice_lst)

                (* (<template or splice D>+ . <template D> ) *)
  |   PTtemplate F (ILISTDAT(template_or_splice_lst, template)) D =
                (#ilisttemp_fcn F) (map (fn t => PTtemplate_or_splice F t D)
                               template_or_splice_lst , PTtemplate F template D)

  and PTtemplate_or_splice F (LISTDAT([SYMBDAT "unquote-splicing", template]))D =
                          if D=1 then
                             (#unqspl_fcn F) (PTexp F template)
                           else
                              (#template_fcn F) ((#listtemp_fcn F) ([(#template_fcn F)
                              ((#simpletemp_fcn F) 
                                   ((#datum_fcn F) (SYMBDAT "unquote-splicing"))),
                              (#template_fcn F) (PTtemplate F template (D-1))]))

  |   PTtemplate_or_splice F (template)D = 
                           (#template_fcn F) (PTtemplate F template D)

  and PTdefinition F (LISTDAT([SYMBDAT "define", 
                          variable as (SYMBDAT _), 
                          exp])) =
                   let val variable' = PTvariable F variable
                       val exp'      = PTexp F exp
                   in
                       (#vardef_fcn F)(variable',exp')
                   end

  |   PTdefinition F (def as (LISTDAT((SYMBDAT "define")::
                  (ILISTDAT(variable1::formals, variable2))::
                  body))) =
                  let val variable'   = PTvariable F variable1
                      val variable''  = PTvariable F variable2
                      val formals'    = PTformals F formals ((#varpar_fcn F) variable'')
                      val body'       = PTbody F body
                  in
                      ((#fundef_fcn F) (variable',formals',body'))
                       handle ParseError mes => sig_err(mes,def)
                  end
                        
  |   PTdefinition F (def as (LISTDAT((SYMBDAT "define")::
                          (LISTDAT(variable::formals))::
                          body))) =
	           let val variable' = PTvariable F variable
                       val formals'  = PTformals F formals (#nullpar_fcn F)
                       val body'     = PTbody F body
                   in
                       ((#fundef_fcn F) (variable',formals',body'))
                        handle ParseError mes => sig_err(mes,def)
                   end

  |   PTdefinition F (LISTDAT((SYMBDAT "begin")::deflist)) =
                   let val deflist' = map (PTdefinition F) deflist
                   in
                       (#begindef_fcn F) deflist'
                   end

  |   PTdefinition F d = sig_err("Expected definition",d)

           (* Note that Tbody is supposed to be 
              called with a list argument.*)
           (* singleton case *)
  and PTbody F b = 
      case b of
           [exp]    => ([],([],PTexp F exp))
      |    (hd::tl) =>  if (isdefinition hd) then
                            let val (dlist,sequence) = PTbody F tl
                                val d                = PTdefinition F hd
                            in
                                (d::dlist,sequence)
                            end
                         else
                            let val (dlist,(explist,exp)) = PTbody F tl
                                val exp'                  = PTexp F hd
                            in
                                (dlist,(exp'::explist,exp))
                            end     
      |   _         => raise ParseError "Illegal empty body"
   
                   (* (<test> => <recipient>) *)
  and PTcond_clause F (LISTDAT([exp1,SYMBDAT "=>",exp2])) =
                   (#testrec_fcn F) (PTexp F exp1, PTexp F exp2)

                   (* (<test>) *)
  |   PTcond_clause F (LISTDAT([exp])) =
                   (#test_fcn F) (PTexp F exp)

                   (* (<test> <sequence>) *)
  |   PTcond_clause F (cc as (LISTDAT(test::exp_tl))) =
                   ((#testseq_fcn F) (PTexp F test,PTsequence F exp_tl) 
                    handle ParseError mes => sig_err(mes,cc))

  |   PTcond_clause F c = sig_err("Expected cond_clause",c)
      
  and PTcase_clause F (cc as (LISTDAT(LISTDAT(datumlst)::sequence))) =
                   ((map (#datum_fcn F) datumlst,PTsequence F sequence)
                    handle ParseError mes => sig_err(mes,cc))

  |   PTcase_clause F c = sig_err("Expected case_clause",c)

  and PTsequence F s = 
      case s of
           [exp]         => ([],PTexp F exp)
      |    (exp::exp_tl) => let val (explst,exp') = PTsequence F exp_tl
                            in
                                ((PTexp F exp)::explst,exp')
                            end
      | _                => raise ParseError "Illegal empty sequence"
      
  and PTbindingspec F (LISTDAT([variable,exp])) =
                   (PTvariable F variable, PTexp F exp)

  |   PTbindingspec F b = sig_err("Expected bindingspec",b)
          
  and PTiterationspec F (LISTDAT([variable, init, step])) =
                     (PTvariable F variable, PTexp F init, Some(PTexp F step))

  |   PTiterationspec F (LISTDAT([variable, init])) =
                     (PTvariable F variable, PTexp F init, None)

  |   PTiterationspec F i = sig_err("Expected iterationspec",i)
          
           (* <variable> *)
  and PTexp F (s as (SYMBDAT symbol)) = 
           if not (SyntacticKeyword symbol) then 
              (#variable_fcn F) symbol
           else 
               sig_err("Illegal syntactic keyword, expected expression",s)

           (* <literal> *)
           (* <self-evaluating> = <boolean>, <number>, <character>, <string> *)
  |   PTexp F (x as (BOOLDAT b))  = (#literal_fcn F) ((#datum_fcn F) x) 
  |   PTexp F (x as (CHARDAT c))  = (#literal_fcn F) ((#datum_fcn F) x)
  |   PTexp F (x as (NUMBDAT n))  = (#literal_fcn F) ((#datum_fcn F) x)
  |   PTexp F (x as (STRIDAT s))  = (#literal_fcn F) ((#datum_fcn F) x)

           (* <quotation> *)
  |   PTexp F (x as (LISTDAT([SYMBDAT "quote",datum]))) =
            (#literal_fcn F) ((#datum_fcn F) x)

           (* <lambda expression> *)
           (* (lambda (<variable>* ) <body>) *)
  |   PTexp F (exp as (LISTDAT((SYMBDAT "lambda")::
                   (formals as (LISTDAT(lst)))::
                   body))) =
           ((#lambda_fcn F) ((PTformals F lst (#nullpar_fcn F)), PTbody F body)
           handle ParseError mes => sig_err(mes,exp))
    
           (* (lambda (<variable>+ . <variable>) <body>) *)
  |   PTexp F (exp as (LISTDAT((SYMBDAT "lambda")::
                  (formals as (ILISTDAT(lst,variable)))::
                  body))) =
            let val variable' = PTvariable F variable
            in
                ((#lambda_fcn F) (PTformals F lst ((#varpar_fcn F) variable'), PTbody F body)
                 handle ParseError mes => sig_err(mes,exp))
            end

           (* (lambda <variable> <body>) *)
  |   PTexp F (exp as (LISTDAT((SYMBDAT "lambda")::
                   (formals as (SYMBDAT variable))::
                   body))) =
           let val variable' = PTvariable F formals
               val body'     = PTbody F body
           in
               ((#lambda_fcn F) ((#varpar_fcn F) variable', body')
               handle ParseError mes => sig_err(mes,exp))
           end 

           (* <conditional> *)
           (* (if <expression> <expression> <expression>) *)
  |   PTexp F (LISTDAT([SYMBDAT "if", test, consequent, alternate])) =
           (#if_fcn F) (PTexp F test, PTexp F consequent, Some (PTexp F alternate))

           (* (if <expression> <expression>) *)
  |   PTexp F (LISTDAT([SYMBDAT "if", test, consequent])) =
            (#if_fcn F) (PTexp F test, PTexp F consequent, None)

           (* <assignment> *)
           (* (set! <variable> <expression>) *)
  |   PTexp F (LISTDAT([SYMBDAT "set!", variable, expression])) =
            (#assign_fcn F) (PTvariable F variable, PTexp F expression)

           (* <derived expression> *)
           (* 
              (cond <cond clause>+ )                 | 
              (cond <cond_clause>* (else <sequence>))
           *)
  |   PTexp F (c as (LISTDAT(SYMBDAT "cond"::cond_clauses))) =
           let val (cond_part,else_part) = split_else cond_clauses
           in
               if else_part = [] then
                  let val mp = 
                      if cond_part = [] then 
                     sig_err("Illegal empty conditional clause encountered",c) 
                     else map (PTcond_clause F) cond_part
                  val ccbody = Pcondlist2condcons F (mp, #nullcond_fcn F)
               in
                  (#cond_fcn F) ccbody
               end
             else
               let val mp = map (PTcond_clause F) cond_part
                   val seq = PTsequence F else_part
                   val ccbody = Pcondlist2condcons F (mp, (#conddefault_fcn F)  seq)
               in
                   (#cond_fcn F) ccbody
               end
           end
    
           (*  (case <expression> <case clause>+ )                 |
               (case <expression> <case clause>* (else <sequence>)
            *)
  |   PTexp F (c as (LISTDAT(SYMBDAT "case"::exp::case_clauses))) =
            let val (case_part,else_part) = split_else case_clauses
            in
               if else_part = [] then
                  let val mp = if case_part = [] then 
	                     sig_err("Illegal empty case clause encountered",c)
                          else
		             map (PTcase_clause F) case_part
                      val ccbody = Pcaselist2casecons F (mp, #nullcase_fcn F)
                  in
                      (#case_fcn F) (PTexp F exp, ccbody)
                  end
               else
                  let val mp = map (PTcase_clause F) case_part
                      val seq = PTsequence F else_part
                      val ccbody = Pcaselist2casecons F (mp, (#casedefault_fcn F) seq)
                  in
                      (#case_fcn F) (PTexp F exp, ccbody)
                  end
            end  

           (*  (and <test>* ) *)
  |   PTexp F (LISTDAT(SYMBDAT "and"::tests)) = (#and_fcn F) (map (PTexp F) tests)

           (*  (or <test>* ) *)
  |   PTexp F (LISTDAT(SYMBDAT "or"::tests)) = (#or_fcn F) (map (PTexp F) tests)

           (*  (let (<binding spec>* ) <body>) *)
  |   PTexp F (exp as (LISTDAT(SYMBDAT "let":: 
                  LISTDAT(bindingspecs)::
                  body))) =
           ((#let_fcn F) (map (PTbindingspec F) bindingspecs, PTbody F body)
            handle ParseError mes => sig_err(mes,exp))

           (*  (let <variable> (<binding spec>* ) <body>) *)
  |   PTexp F (exp as (LISTDAT(SYMBDAT "let"::
                  variable::
                  LISTDAT(bindingspecs)::
                  body))) =
           ((#namedlet_fcn F) (PTvariable F variable, 
                     map (PTbindingspec F) bindingspecs,
                     PTbody F body)
            handle ParseError mes => sig_err(mes,exp))

           (*  (let* (<binding spec>* ) <body>) *)
  |   PTexp F (exp as (LISTDAT(SYMBDAT "let*"::
                  LISTDAT(bindingspecs)::
                  body))) =
           ((#lets_fcn F) (map (PTbindingspec F) bindingspecs, PTbody F body)
            handle ParseError mes => sig_err(mes, exp))

           (*  (letrec (<binding spec>* ) <body>) *)
  |   PTexp F (exp as (LISTDAT(SYMBDAT "letrec"::
                  LISTDAT(bindingspecs)::
                  body))) =
           ((#letrec_fcn F) (map (PTbindingspec F) bindingspecs, PTbody F body)
            handle ParseError mes => sig_err(mes,exp))

           (*  (begin <sequence>) *)
  |   PTexp F (LISTDAT(SYMBDAT "begin"::sequence)) = (#begin_fcn F) (PTsequence F sequence)

           (* (do (<iteration spec>* )
              (<test> <sequence> )
               <command>* )
           *)
  |   PTexp F (LISTDAT(SYMBDAT "do"::
                  LISTDAT(iterationspecs)::
                  LISTDAT(test::sequence)::
                  commands)) =
            (#do_fcn F) (map (PTiterationspec F) iterationspecs,
            PTexp F test,
            PTsequence F sequence,
            map (PTexp F) commands)

           (* (delay <expression> ) *)
  |   PTexp F (LISTDAT([SYMBDAT "delay", exp])) = (#delay_fcn F) (PTexp F exp)

           (* <quasiquotation> *)
  |   PTexp F (lst as LISTDAT(SYMBDAT "quasiquote"::_ )) = PTquasiquotation F lst

           (* <procedure call> *)
           (* (<expression> <expression>* ) *)
           (* 
              NB! It is essential that this case of the definition is
              the final (non-error) one, since its pattern is very
              admissive and the case should only apply if none of
              the others do.
            *)
  |   PTexp F (LISTDAT(operator::operandlist)) =
           (#call_fcn F) (PTexp F operator, map (PTexp F) operandlist)

  |   PTexp F e = sig_err("Illegal expression",e)
      
  fun PTcommand_or_definition (F : ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j) fcn_record) cd =
                             if (isdefinition cd) then 
                                (#definition_fcn F) (PTdefinition F cd)
                             else
                                (#command_fcn F) (PTexp F cd)



  fun Pdat2command_or_definition F d = PTcommand_or_definition F d
      
 


(*------------- Translation to annotated AST -----------*)



datatype 'a anndat = 
      ABOOLDAT of bool |
      ACHARDAT of string |
      ASTRIDAT of string |
      ASYMBDAT of string |
      ANUMBDAT of number |
      AVECTDAT of 'a anndatum list |
      ALISTDAT of 'a anndatum list |
      AILISTDAT of 'a anndatum list * 'a anndatum
withtype 'a anndatum = 'a * 'a anndat

datatype 'a annexp =

  ALITERAL of 'a anndatum |
  AVARIABLE of 'a * variable |
  ACALL of 'a * ('a annexp * ('a annexp) list) |
  ALAMBDA of 'a * ('a annformals * 'a annbody) |
  AIF of 'a * ('a annexp * 'a annexp * ('a annexp) Option) |
  AASSIGN of 'a * (variable * 'a annexp)  |
  ACOND of 'a * 'a anncond_clause_body |
  ACASE of 'a * ('a annexp * 'a anncase_clause_body) |
  AAND of 'a * ('a annexp) list |
  AOR of 'a * ('a annexp) list |
  ALET of 'a * (('a annbinding_spec) list * 'a annbody) |
  ANAMEDLET of 'a * (variable * ('a annbinding_spec) list * 'a annbody) |
  ALETS of 'a * (('a annbinding_spec) list * 'a annbody) |
  ALETREC of 'a * (('a annbinding_spec) list * 'a annbody) |
  ABEGIN of 'a * 'a annsequence |
  ADO of 'a * (('a anniteration_spec) list * 'a annexp * 'a annsequence *
         ('a annexp) list) |
  ADELAY of 'a * 'a annexp |
  AQUASIQUOTE of 'a * 'a anntemplate 
and 'a anncond_clause_body =
  ACONDCLAUSE of 'a * ('a anncond_clause * 'a anncond_clause_body) |
  ANULLCOND of 'a |
  ACONDDEFAULT of 'a * 'a annsequence 
and 'a anncond_clause =
  ATESTSEQ of 'a * ('a annexp * 'a annsequence) |
  ATEST of 'a * 'a annexp |
  ATESTREC of 'a * ('a annexp * 'a annexp) 
and 'a anncase_clause_body =
  ACASECLAUSE of 'a * ('a anncase_clause * 'a anncase_clause_body) |
  ANULLCASE of 'a |
  ACASEDEFAULT of 'a * 'a annsequence 
and 'a anndefinition =
  AVARDEF of 'a * (variable * 'a annexp) |
  AFUNDEF of 'a * (variable * 'a annformals * 'a annbody) |
  ABEGINDEF of 'a * ('a anndefinition) list 
and 'a anntemplate =
  ASIMPLETEMP of 'a anndatum |
  ALISTTEMP of 'a * ('a anntemplate_or_splice) list |
  AILISTTEMP of 'a * (('a anntemplate_or_splice) list * 'a anntemplate) |
  AVECTEMP of 'a * ('a anntemplate_or_splice) list |
  AUNQUOTE of 'a * 'a annexp 
and 'a anntemplate_or_splice =
  ATEMPLATE of 'a * 'a anntemplate |
  AUNQSPL of 'a * 'a annexp 
and 'a annformals =
  AVARPAR of 'a * variable |
  APAIRPAR of 'a * (variable * 'a annformals) |
  ANULLPAR of 'a 
withtype 'a annsequence = ('a annexp) list * 'a annexp
and      'a annbody = ('a anndefinition) list * 'a annsequence
and      'a anncase_clause = 'a anndatum list * 'a annsequence
and      'a annbinding_spec = variable * 'a annexp
and      'a anniteration_spec = variable * 'a annexp * ('a annexp) Option

datatype 'a anncommand_or_definition =
  ACOMMAND of 'a * 'a annexp |
  ADEFINITION of 'a * 'a anndefinition 


 
  datatype infotype = NULLINFO | INFO of string

  fun init() = ref NULLINFO
  
  fun datum_fcn x =     
                let fun simpledat_fcn y = 
                                      case y of 
                                        BOOLDAT b => ABOOLDAT b
                                      | CHARDAT c => ACHARDAT c
                                      | STRIDAT s => ASTRIDAT s
                                      | SYMBDAT s => ASYMBDAT s
                                      | NUMBDAT n => ANUMBDAT n
                                      | _ => raise ParseError "This can't happen!\n"
                in
                     case x of
                       VECTDAT dl => (init(), AVECTDAT (map datum_fcn dl))
                     | LISTDAT dl => (init(), ALISTDAT (map datum_fcn dl))
                     | ILISTDAT (dl,d) => (init(),AILISTDAT (map datum_fcn dl, datum_fcn d))
                     | d => (init(), simpledat_fcn d)
                 end

  val literal_fcn = fn x => ALITERAL x
  val variable_fcn = fn x => AVARIABLE (init(), x)
  val call_fcn = fn x => ACALL (init(), x)
  val lambda_fcn = fn x => ALAMBDA (init(), x)
  val if_fcn = fn x => AIF (init(), x)
  val assign_fcn = fn x => AASSIGN (init(), x)
  val cond_fcn = fn x => ACOND (init(), x)
  val case_fcn = fn x => ACASE (init(), x)
  val and_fcn = fn x => AAND (init(), x)
  val or_fcn  = fn x => AOR (init(), x)
  val let_fcn = fn x => ALET (init(), x)
  val namedlet_fcn = fn x => ANAMEDLET (init(), x)
  val lets_fcn = fn x => ALETS (init(), x)
  val letrec_fcn = fn x => ALETREC (init(), x)
  val begin_fcn = fn x => ABEGIN (init(), x)
  val do_fcn = fn x => ADO (init(), x)
  val delay_fcn = fn x => ADELAY (init(), x)
  val quasiquote_fcn = fn x => AQUASIQUOTE (init(), x)
  val condclause_fcn = fn x => ACONDCLAUSE (init(), x)
  val nullcond_fcn = ANULLCOND (init())
  val conddefault_fcn = fn x => ACONDDEFAULT (init(), x)
  val testseq_fcn = fn x => ATESTSEQ (init(), x)
  val test_fcn = fn x => ATEST (init(), x)
  val testrec_fcn = fn x => ATESTREC (init(), x)
  val caseclause_fcn = fn x => ACASECLAUSE (init(), x)
  val nullcase_fcn = ANULLCASE (init())
  val casedefault_fcn = fn x => ACASEDEFAULT (init(), x)
  val vardef_fcn = fn x => AVARDEF (init(), x)
  val fundef_fcn = fn x => AFUNDEF (init(), x)
  val begindef_fcn = fn x => ABEGINDEF (init(), x)
  val simpletemp_fcn = fn x => ASIMPLETEMP x
  val listtemp_fcn = fn x => ALISTTEMP (init(), x)
  val ilisttemp_fcn = fn x => AILISTTEMP (init(), x)
  val vectemp_fcn = fn x => AVECTEMP (init(), x)
  val unquote_fcn = fn x => AUNQUOTE (init(), x)
  val template_fcn = fn x => ATEMPLATE (init(), x)
  val unqspl_fcn = fn x => AUNQSPL (init(), x)
  val varpar_fcn = fn x => AVARPAR (init(), x)
  val pairpar_fcn = fn x => APAIRPAR (init(), x)
  val nullpar_fcn = ANULLPAR (init())
  val command_fcn = fn x => ACOMMAND (init(), x)
  val definition_fcn = fn x => ADEFINITION (init(), x)
  

  fun dat2annexp d  = 
      let val F = 
                   {literal_fcn = literal_fcn,
                    variable_fcn = variable_fcn,
                    call_fcn = call_fcn,
                    lambda_fcn = lambda_fcn, 
                    if_fcn = if_fcn,
                    assign_fcn = assign_fcn,
                    cond_fcn = cond_fcn,
                    case_fcn = case_fcn,
                    and_fcn = and_fcn,
                    or_fcn  = or_fcn ,
                    let_fcn = let_fcn,
                    namedlet_fcn = namedlet_fcn,
                    lets_fcn = lets_fcn,
                    letrec_fcn = letrec_fcn,
                    begin_fcn = begin_fcn,
                    do_fcn = do_fcn ,
                    delay_fcn = delay_fcn ,
                    quasiquote_fcn = quasiquote_fcn ,
                    condclause_fcn = condclause_fcn ,
                    nullcond_fcn = nullcond_fcn ,
                    conddefault_fcn = conddefault_fcn ,
                    testseq_fcn = testseq_fcn ,
                    test_fcn = test_fcn ,
                    testrec_fcn = testrec_fcn ,
                    caseclause_fcn = caseclause_fcn,
                    nullcase_fcn = nullcase_fcn,
                    casedefault_fcn = casedefault_fcn,
                    vardef_fcn = vardef_fcn,
                    fundef_fcn = fundef_fcn ,
                    begindef_fcn = begindef_fcn,
                    simpletemp_fcn = simpletemp_fcn,
                    listtemp_fcn = listtemp_fcn,
                    ilisttemp_fcn = ilisttemp_fcn,
                    vectemp_fcn = vectemp_fcn,
                    unquote_fcn = unquote_fcn,
                    template_fcn = template_fcn,
                    unqspl_fcn = unqspl_fcn,
                    varpar_fcn = varpar_fcn,
                    pairpar_fcn = pairpar_fcn,
                    nullpar_fcn = nullpar_fcn,
                    command_fcn = command_fcn,
                    definition_fcn = definition_fcn,
                    datum_fcn = datum_fcn} 
       in
             Pdat2command_or_definition F d
       end 







(*--------------------- Homomorphic map on annotated AST ------------------*)




type ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l) afcn_record =
        {aand_fcn:'a * 'b list -> 'b,
        aassign_fcn:'a * (string * 'b) -> 'b,
        abegin_fcn:'a * ('b list * 'b) -> 'b,
        abegindef_fcn:'a * 'c list -> 'c,
        acall_fcn:'a * ('b * 'b list) -> 'b,
        acase_fcn:'a * ('b * 'd) -> 'b,
        acaseclause_fcn:'a * (('e list * ('b list * 'b)) * 'd) -> 'd,
        acasedefault_fcn:'a * ('b list * 'b) -> 'd,
        acommand_fcn:'a * 'b -> 'f,
        acond_fcn:'a * 'g -> 'b,
        acondclause_fcn:'a * ('h * 'g) -> 'g,
        aconddefault_fcn:'a * ('b list * 'b) -> 'g,
        adatum_fcn:'i anndatum -> 'e,
        adefinition_fcn:'a * 'c -> 'f,
        adelay_fcn:'a * 'b -> 'b,
        ado_fcn:'a * ((string * 'b * 'b Option) list * 'b * ('b list * 'b) * 'b list) -> 'b,
        afundef_fcn:'a * (string * 'j * ('c list* ('b list * 'b))) -> 'c, 
        aif_fcn:'a * ('b * 'b * 'b Option) -> 'b,
        ailisttemp_fcn:'a * ('k list * 'l) -> 'l,
        alambda_fcn:'a * ('j * ('c list * ('b list * 'b))) -> 'b,
        alet_fcn:'a * ((string * 'b) list * ('c list * ('b list * 'b))) -> 'b,
        aletrec_fcn:'a * ((string * 'b) list * ('c list * ('b list * 'b))) -> 'b,
        alets_fcn:'a * ((string * 'b) list * ('c list * ('b list * 'b))) -> 'b,
        alisttemp_fcn:'a * 'k list -> 'l,
        aliteral_fcn:'e -> 'b,
        anamedlet_fcn:'a * (string * (string * 'b) list * ('c list * ('b list * 'b))) -> 'b, 
        ann_fcn:'i -> 'a,
        anullcase_fcn:'a -> 'd,
        anullcond_fcn:'a -> 'g,
        anullpar_fcn:'a -> 'j,
        aor_fcn:'a * 'b list -> 'b,
        apairpar_fcn:'a * (string * 'j) -> 'j,
        aquasiquote_fcn:'a * 'l -> 'b,
        asimpletemp_fcn:'e -> 'l,
        atemplate_fcn:'a * 'l -> 'k,
        atest_fcn:'a * 'b -> 'h,
        atestrec_fcn:'a * ('b * 'b) -> 'h,
        atestseq_fcn:'a * ('b * ('b list * 'b)) -> 'h, 
        aunqspl_fcn:'a * 'b -> 'k,
        aunquote_fcn:'a * 'b -> 'l,
        avardef_fcn:'a * (string * 'b) -> 'c,
        avariable_fcn:'a * string -> 'b,
        avarpar_fcn:'a * string -> 'j,
        avectemp_fcn:'a * 'k list -> 'l} 



(*
 {aliteral_fcn ,
  ann_fcn,
  avariable_fcn ,
  acall_fcn ,
  alambda_fcn ,
  aif_fcn ,
  aassign_fcn ,
  acond_fcn ,
  acase_fcn ,
  aand_fcn ,
  aor_fcn  ,
  alet_fcn ,
  anamedlet_fcn ,
  alets_fcn ,
  aletrec_fcn ,
  abegin_fcn ,
  ado_fcn ,
  adelay_fcn ,
  aquasiquote_fcn ,
  acondclause_fcn ,
  anullcond_fcn ,
  aconddefault_fcn ,
  atestseq_fcn ,
  atest_fcn ,
  atestrec_fcn ,
  acaseclause_fcn ,
  anullcase_fcn ,
  acasedefault_fcn ,
  avardef_fcn ,
  afundef_fcn ,
  abegindef_fcn ,
  asimpletemp_fcn ,
  alisttemp_fcn ,
  ailisttemp_fcn ,
  avectemp_fcn ,
  aunquote_fcn ,
  atemplate_fcn ,
  aunqspl_fcn ,
  avarpar_fcn ,
  apairpar_fcn ,
  anullpar_fcn ,
  acommand_fcn ,
  adefinition_fcn ,
  adatum_fcn}


*)




   fun mapaexp (F : ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l) afcn_record) aexp = 
                 case aexp of
                   ALITERAL ad => (#aliteral_fcn F) ((#adatum_fcn F) ad)
                 | AVARIABLE (a,v) => (#avariable_fcn F) ((#ann_fcn F) a, v) 
                 | ACALL (a,(ae,ael)) => (#acall_fcn F) 
                                           ((#ann_fcn F) a, (mapaexp F ae, map (mapaexp F) ael))
                 | ALAMBDA (a,(af,ab)) => (#alambda_fcn F) 
                                            ((#ann_fcn F) a, (mapaformals F af, mapabody F ab))
                 | AIF (a,(ae,ae',aeo)) => (#aif_fcn F) ((#ann_fcn F) a, 
                                              (mapaexp F ae, 
                                               mapaexp F ae',
                                               case aeo of
                                                 Some x => Some (mapaexp F x)
                                               | None => None))   
                    
                 | AASSIGN (a,(v,ae)) => (#aassign_fcn F) ((#ann_fcn F) a, (v, mapaexp F ae))
                 | ACOND (a,acb) => (#acond_fcn F) ((#ann_fcn F) a, (mapacond_clause_body F acb))
                 | ACASE (a,(ae,accb)) => (#acase_fcn F) ((#ann_fcn F) a,
                                                          (mapaexp F ae,
                                                           mapacase_clause_body F accb))
                 | AAND (a,ael) => (#aand_fcn F) ((#ann_fcn F) a, map (mapaexp F) ael)
                 | AOR (a,ael) => (#aor_fcn F) ((#ann_fcn F) a, map (mapaexp F) ael)
                 | ALET (a,(absl,ab)) => (#alet_fcn F) ((#ann_fcn F) a,
                                                        (map (mapabinding_spec F) absl,
                                                         mapabody F ab))
                 | ANAMEDLET (a,(v,absl,ab)) => (#anamedlet_fcn F) 
                                                   ((#ann_fcn F) a,
                                                       (v, 
                                                        map (mapabinding_spec F) absl,
                                                        mapabody F ab))
                 | ALETS (a,(absl,ab)) => (#alets_fcn F) ((#ann_fcn F) a,
                                                             (map (mapabinding_spec F) absl,
                                                              mapabody F ab))
                 | ALETREC (a,(absl,ab)) => (#aletrec_fcn F) ((#ann_fcn F) a,
                                                                 (map (mapabinding_spec F) absl,
                                                                  mapabody F ab))
                 | ABEGIN (a,aseq) => (#abegin_fcn F) ((#ann_fcn F) a, mapasequence F aseq)
                 | ADO (a,(aisl,ae,aseq,ael)) => (#ado_fcn F) 
                                                  ((#ann_fcn F) a,
                                                      (map (mapaiteration_spec F) aisl,
                                                       mapaexp F ae,
                                                       mapasequence F aseq,
                                                       map (mapaexp F) ael)) 
                 | ADELAY (a,ae) => (#adelay_fcn F) ((#ann_fcn F) a, mapaexp F ae)
                 | AQUASIQUOTE (a,at) => (#aquasiquote_fcn F) ((#ann_fcn F) a, mapatemplate F at)

  

  and mapaformals F afmls =
                    case afmls of
                      AVARPAR (a,v) => (#avarpar_fcn F) ((#ann_fcn F) a, v)
                    | APAIRPAR (a,(v,af)) => (#apairpar_fcn F) 
                                                ((#ann_fcn F) a,
                                                   (v,
                                                    mapaformals F af))
                    | ANULLPAR a => (#anullpar_fcn F) ((#ann_fcn F) a)

  and mapabody F abody = 
                 let val (adfl,aseq) = abody in
                     (map (mapadefinition F) adfl, mapasequence F aseq)
                 end

  and mapacond_clause_body F accbody =
                             case accbody of
                               ACONDCLAUSE (a,(acc,accb)) => (#acondclause_fcn F) 
                                                                ((#ann_fcn F) a,
                                                                   (mapacond_clause F acc,
                                                                    mapacond_clause_body F accb))
                             | ANULLCOND a => (#anullcond_fcn F) ((#ann_fcn F) a)
                             | ACONDDEFAULT (a,aseq) => (#aconddefault_fcn F) 
                                                         ((#ann_fcn F) a, mapasequence F aseq)

  and mapacond_clause F acclause =
                        case acclause of
                          ATESTSEQ (a,(ae,aseq)) => (#atestseq_fcn F) 
                                                     ((#ann_fcn F) a,
                                                         (mapaexp F ae, mapasequence F aseq))
                        | ATEST (a,ae) => (#atest_fcn F) ((#ann_fcn F) a, mapaexp F ae)
                        | ATESTREC (a,(ae,ae')) => (#atestrec_fcn F) 
                                                      ((#ann_fcn F) a, (mapaexp F ae, mapaexp F ae'))

  and mapacase_clause_body F accbody =
                             case accbody of
                               ACASECLAUSE (a,(acc,accb)) => (#acaseclause_fcn F)
                                                               ((#ann_fcn F) a,
                                                                (mapacase_clause F acc,
                                                                 mapacase_clause_body F accb))
                             | ANULLCASE a => (#anullcase_fcn F) ((#ann_fcn F) a)
                             | ACASEDEFAULT (a,aseq) => (#acasedefault_fcn F) 
                                                         ((#ann_fcn F) a, mapasequence F aseq)

  and mapacase_clause F acclause = 
                        let val (adl,aseq) = acclause in
                            (map (#adatum_fcn F) adl, mapasequence F aseq)
                        end

  and mapadefinition F adef =
                       case adef of
                         AVARDEF (a,(v,ae)) => (#avardef_fcn F) ((#ann_fcn F) a, (v, mapaexp F ae))
                       | AFUNDEF (a,(v,af,ab)) => (#afundef_fcn F) 
                                                     ((#ann_fcn F) a,
                                                       (v, mapaformals F af, mapabody F ab))
                       | ABEGINDEF (a,adfl) => (#abegindef_fcn F) ((#ann_fcn F) a, 
                                                                      map (mapadefinition F) adfl)

  and mapatemplate F atem =
                     case atem of
                       ASIMPLETEMP ad => (#asimpletemp_fcn F) ((#adatum_fcn F) ad)
                     | ALISTTEMP (a,atosl) => (#alisttemp_fcn F) 
                                                ((#ann_fcn F) a, map (mapatemplate_or_splice F) atosl)
                     | AILISTTEMP (a,(atosl,at)) => (#ailisttemp_fcn F)
                                                      ((#ann_fcn F) a,
                                                          (map (mapatemplate_or_splice F) atosl,
                                                           mapatemplate F at))
                     | AVECTEMP (a,atosl) => (#avectemp_fcn F) 
                                               ((#ann_fcn F) a,
                                                 map (mapatemplate_or_splice F) atosl)
                     | AUNQUOTE (a,ae) => (#aunquote_fcn F) ((#ann_fcn F) a, mapaexp F ae)

  and mapatemplate_or_splice F atos =
                               case atos of
                                 ATEMPLATE (a,at) => (#atemplate_fcn F) 
                                                        ((#ann_fcn F) a, mapatemplate F at)
                               | AUNQSPL (a,ae) => (#aunqspl_fcn F) ((#ann_fcn F) a, mapaexp F ae)


  and mapasequence F seq =
                     let val (ael,ae) = seq in
                         (map (mapaexp F) ael, mapaexp F ae)
                     end

  and mapabinding_spec F abspec =
                         let val (v,ae) = abspec in
                             (v, mapaexp F ae)
                         end

  and mapaiteration_spec F aitspec =
                           let val (v,ae,aeo) = aitspec in
                               (v,
                                mapaexp F ae,
                                case aeo of
                                  Some x => Some (mapaexp F x)
                                | None => None)
                            end

  and mapacommand_or_definition F acod =
                                  case acod of
                                    ACOMMAND (a,ae) => (#acommand_fcn F)((#ann_fcn F) a, mapaexp F ae)
                                  | ADEFINITION (a,ad) => (#adefinition_fcn F) 
                                                            ((#ann_fcn F) a, mapadefinition F ad)  




(*
 {aliteral_fcn ,
  ann_fcn,
  avariable_fcn ,
  acall_fcn ,
  alambda_fcn ,
  aif_fcn ,
  aassign_fcn ,
  acond_fcn ,
  acase_fcn ,
  aand_fcn ,
  aor_fcn  ,
  alet_fcn ,
  anamedlet_fcn ,
  alets_fcn ,
  aletrec_fcn ,
  abegin_fcn ,
  ado_fcn ,
  adelay_fcn ,
  aquasiquote_fcn ,
  acondclause_fcn ,
  anullcond_fcn ,
  aconddefault_fcn ,
  atestseq_fcn ,
  atest_fcn ,
  atestrec_fcn ,
  acaseclause_fcn ,
  anullcase_fcn ,
  acasedefault_fcn ,
  avardef_fcn ,
  afundef_fcn ,
  abegindef_fcn ,
  asimpletemp_fcn ,
  alisttemp_fcn ,
  ailisttemp_fcn ,
  avectemp_fcn ,
  aunquote_fcn ,
  atemplate_fcn ,
  aunqspl_fcn ,
  avarpar_fcn ,
  apairpar_fcn ,
  anullpar_fcn ,
  acommand_fcn ,
  adefinition_fcn ,
  adatum_fcn}


*)


(*---------------- Unparser for annotated AST --------------------*)




(* ------- Pretty Printing Auxiliaries --------------- *)



  datatype delimit = NO | DELIM of string * string

  datatype pptype = BLOCK of (pptype list) * int * int
                  | STR of string
                  | BRK of int





  fun pplength ppelem =
             case ppelem of
               BLOCK(_,_,len) => len
             | STR s => size s
             | BRK len => len

  val str = STR
  val brk = BRK

  val emptyblock = str ""
  val space = str " "

  fun blo (indent,es) =
          let fun sum (es',k) =
                      case es' of
                        [] => k
                      | e::es'' => sum (es'',pplength e + k)
          in
              BLOCK (es,indent,sum(es,0))
          end 
          


  infix 9 &; 
  (* High precedence secures that e.g. "("^""&"a" => "(a" and not ... => "( a" *)
     
  


  fun conc x y break =
             let fun noinfo ppelem = 
                      case ppelem of
                        STR "" => true
                      | BLOCK(_,_,0) => true
                      | BRK 0 => true
                      | _ => false
              in
                 case (noinfo x,noinfo y) of
                   (true,true) => emptyblock
                 | (true,false) => y
                 | (false,true) => x
                 | (false,false) => if break then 
                                       blo(1, [x, brk 1, y]) (* later on we'll make & more powerful *) 
                                    else  blo(1, [x,y])     
              end

   

  infix 9 &&;
  fun x && y = conc x y false
  
  fun x & y = conc x y true  


  fun unparse_ann ann =
                  case !ann of
                    NULLINFO => ""
                  | INFO s   => s

 
  fun combine info delim el =
              case delim of
                NO => if (info = "") then blo(1,[el]) 
                      else blo(1,[str "[", str info, brk 1, el, str "]"])
              | DELIM(d_begin,d_end) => 
                     if (info = "") then blo(1,[str d_begin, el, str d_end])
                     else blo(1, [str "[", str info, brk 1, str d_begin, el, str d_end, str "]"])


  


  fun pack x = if x = "" then (emptyblock) else blo(1, [str "[", str x, str "]"]) 


  fun paren ppelem = blo(1,[str "(", ppelem, str ")"])
  
  fun list2block l = 
                 let fun convert lst =
                         case lst of
                            [] => []
                         |  [x] => [x]
                         | x::tl => x::space::(convert tl)
                 in
                     blo(1,convert l)
                 end

      
  datatype partype = NPAR of string
                   | VPAR of string * string  
                   | PPAR of string * string * partype

 fun pairpar2parblock p =
                 case p of
                   NPAR a => pack a
                 | VPAR(a,v) => (str ".") & (combine a NO (str v))
                 | PPAR(a,v,pp) => combine a NO ((str v) & (pairpar2parblock pp))
         
   
  fun formals2block fmls =
           case fmls of
             NPAR a => pack a
           | VPAR(a,v) => combine a NO (str v)
           | _ => blo(1, [str "(", pairpar2parblock fmls, str ")"])
   



  fun defformals2block dfmls =
         case dfmls of
           NPAR a => pack a
         | VPAR(a,v) => combine a NO (str v)
         | _ => pairpar2parblock dfmls

  fun sequence2block aseq =
        let val (ael,ae) = aseq in
            (blo(1,ael)) & ae
        end

  fun body2block abody =
        let val (adfl,aseq) = abody in
            (blo(1,adfl)) & (sequence2block aseq)
        end

  fun caseclause2block acc =
        let val (adl,aseq) = acc in
            blo(1,[str "((", blo(1,adl), str ")", brk 1, (sequence2block aseq), str ")"])
        end

  fun bindingspec2block abs =
        let val (v,ae) = abs in
            blo(1,[str "(", str v, brk 1, ae, str ")"])
        end

  fun iterationspec2block ais =
        let val (v,ae,aeo) = ais in
            blo(1,[str "(", str v, brk 1, ae, brk 1, (case aeo of Some ae' => ae' | None => emptyblock)])
        end
               





  fun unparse_adatum adat =              
              case adat of
                (a, ABOOLDAT b) => combine (unparse_ann a) NO (str (boolean2str b))
              | (a, ACHARDAT c) => combine (unparse_ann a) NO (str ("#\\"^c))
              | (a, ASTRIDAT s) => combine (unparse_ann a) NO (str ("\"^s^\""))
              | (a, ASYMBDAT s) => combine (unparse_ann a) NO (str s)
              | (a, ANUMBDAT n) => combine (unparse_ann a) NO (str (number2str n))
              | (a, AVECTDAT dlist) => combine  (unparse_ann a) (DELIM("#(",")"))
                       (list2block (map unparse_adatum dlist))
              | (a, ALISTDAT dlist) => combine  (unparse_ann a) (DELIM("(",")"))
                       (list2block (map unparse_adatum dlist))
              | (a, AILISTDAT (dlist,d)) => combine  (unparse_ann a) (DELIM("(",")"))
                       ((list2block ((map unparse_adatum dlist) @ [str " . "])) & (unparse_adatum d))
      
  

(* -------------------------- HERFRA ---------------------------------------------------------------- *)



  val unparse_aliteral = fn x => x
  val unparse_avariable = fn x => let val (a,v) = x in combine a NO (str v) end
  val unparse_acall = fn x => let val (a,(ae,ael)) = x in
                                  combine  a (DELIM("(",")")) (ae & (list2block ael))
                              end
  val unparse_alambda = fn x => let val (a,(af,ab)) = x 
                                in
                                    combine a (DELIM("(lambda ",")"))  ((formals2block af) & 
                                                 (body2block ab))
                                end
  val unparse_aif = fn x => let val (a,(ae,ae',aeo)) = x 
                                val aeo' = (case aeo of
                                             None => emptyblock
                                           | Some x => x) in
                                combine  a (DELIM("(if ",")")) (ae & ae' & aeo')
                            end
  val unparse_aassign = fn x => let val (a,(v,ae)) = x in
                                    combine a (DELIM("(set! ",")")) ((str v) & ae)
                                end
  val unparse_acond = fn x => let val (a,acb) = x in
                                  combine a (DELIM("(cond ",")")) acb
                              end
  val unparse_acase = fn x => let val (a,(ae,accb)) = x in
                                  combine a (DELIM("(case ",")")) (ae & accb)
                              end


  val unparse_aand = fn x => let val (a,ael) = x in
                                 combine a (DELIM("(and ",")")) (list2block ael)
                             end
  val unparse_aor = fn x => let val (a,ael) = x in
                                combine a (DELIM("(or ",")")) (list2block ael)
                            end
  val unparse_alet = fn x => let val (a,(absl,ab)) = x 
                                 val absl' = map bindingspec2block absl in
                                 combine a (DELIM("(let ",")")) ((paren (list2block absl')) & 
                                                               (body2block ab))
                             end
  val unparse_anamedlet = fn x => let val (a,(v,absl,ab)) = x 
                                      val absl' = map bindingspec2block absl in
                                      combine a (DELIM("(let ",")")) ((str v) & 
                                                                      (paren (list2block absl')) & 
                                                                      (body2block ab))
                                  end
  val unparse_alets = fn x => let val (a,(absl,ab)) = x 
                                  val absl' = map bindingspec2block absl in
                                  combine a (DELIM("(let* ",")")) ((paren (list2block absl')) & 
                                                                 (body2block ab))
                              end
  val unparse_aletrec = fn x => let val (a,(absl,ab)) = x 
                                    val absl' = map bindingspec2block absl in
                                    combine a (DELIM("(letrec ",")")) ((paren (list2block absl')) & 
                                                                     (body2block ab))
                                end
  val unparse_abegin = fn x => let val (a,aseq) = x in
                                   combine a (DELIM("(begin ",")")) (sequence2block aseq)
                               end
  val unparse_ado = fn x => let val (a,(aisl,ae,aseq,ael)) = x 
                                val aisl' = map iterationspec2block aisl
                                val aisl'' = paren (list2block aisl')
                                val aseq' = paren (sequence2block aseq) in 
                                combine a (DELIM("(do ",")"))
                                          (aisl'' & ae & aseq' & (list2block ael))
                             end
  val unparse_adelay = fn x => let val (a,ae) = x in
                                   combine a (DELIM("(delay ",")")) ae
                               end
  val unparse_aquasiquote = fn x => let val (a,at) = x in
                                        combine a (DELIM("(quasiquote ",")")) at
                                    end
  val unparse_avarpar = fn x => let val (a,v) = x in
                                    VPAR(a,v)
                                end
  val unparse_apairpar = fn x => let val (a,(v,af)) = x in
                                     PPAR(a,v,af)
                                 end
  val unparse_anullpar = fn x => NPAR x
  val unparse_avarpar = fn x => VPAR x
  val unparse_acondclause = fn x => let val (a,(acc,accb)) = x in
                                         combine a NO (acc & accb)
                                     end
  val unparse_anullcond = fn x => pack x  
  val unparse_aconddefault = fn x => let val (a,aseq) = x in
                                         combine a (DELIM("(else ",")")) (sequence2block aseq)
                                     end
  val unparse_atestseq = fn x => let val (a,(ae,aseq)) = x in
                                     combine a (DELIM("(",")")) (ae & (sequence2block aseq))
                                 end
  val unparse_atest = fn x => let val (a,ae) = x in
                                  combine a (DELIM("(",")")) ae
                              end
  val unparse_atestrec = fn x => let val (a,(ae,ae')) = x in
                                     combine a (DELIM("(",")")) (ae & (str "=>") & ae')
                                 end
  val unparse_acaseclause = fn x => let val (a,(acc,accb)) = x  
                                        val acc' = caseclause2block acc in
                                        combine a NO (acc' & accb)
                                    end
  val unparse_anullcase = fn x => pack x
  val unparse_acasedefault = fn x => let val (a,aseq) = x in
                                         combine a (DELIM("(else ",")")) (sequence2block aseq)
                                     end
  val unparse_avardef = fn x => let val (a,(v,ae)) = x in
                                    combine a (DELIM("(define ",")")) ((str v) & ae)
                                end
  val unparse_afundef = fn x => let val (a,(v,af,ab)) = x 
                                    val af' = defformals2block af
                                    val ab' = body2block ab in
                                    combine a (DELIM("(define ",")")) 
                                              ((str ("("^v)) & af' && (str ")") & ab')
                                end
  val unparse_abegindef = fn x => let val (a,adfl) = x in
                                      combine a (DELIM("(begin ",")")) (list2block adfl)
                                  end
  val unparse_asimpletemp = fn x => x
  val unparse_alisttemp = fn x => let val (a,atosl) = x in
                                      combine a (DELIM("(",")")) (list2block atosl)
                                  end
  val unparse_ailisttemp = fn x => let val (a,(atosl,at)) = x in
                                       combine a (DELIM("(",")")) ((list2block atosl) & (str ".") & at)
                                   end
  val unparse_avectemp = fn x => let val (a,atosl) = x in
                                     combine a (DELIM("#(",")")) (list2block atosl)
                                 end
  val unparse_aunquote = fn x => let val (a,ae) = x in
                                     combine a (DELIM("(unquote ",")")) ae
                                 end
  val unparse_atemplate = fn x => let val (a,at) = x in
                                      combine a NO at
                                  end
  val unparse_aunqspl = fn x => let val (a,ae) = x in
                                    combine a (DELIM("unquote-splicing ",")")) ae
                                end
  val unparse_acommand = fn x => let val (a,ae) = x in
                                     combine a NO ae
                                 end
  val unparse_adefinition = fn x => let val (a,ad) = x in
                                        combine a NO ad
                                    end
                                    
                                                                               





  fun unparse_anncommand_or_definition anncod =
      let val F =

 {aliteral_fcn = unparse_aliteral,
  ann_fcn = unparse_ann,
  avariable_fcn = unparse_avariable,
  acall_fcn = unparse_acall,
  alambda_fcn = unparse_alambda,
  aif_fcn = unparse_aif,
  aassign_fcn = unparse_aassign,
  acond_fcn = unparse_acond,
  acase_fcn = unparse_acase,
  aand_fcn = unparse_aand,
  aor_fcn  = unparse_aor,
  alet_fcn = unparse_alet,
  anamedlet_fcn = unparse_anamedlet,
  alets_fcn = unparse_alets,
  aletrec_fcn = unparse_aletrec,
  abegin_fcn = unparse_abegin,
  ado_fcn = unparse_ado,
  adelay_fcn = unparse_adelay,
  aquasiquote_fcn = unparse_aquasiquote,
  acondclause_fcn = unparse_acondclause,
  anullcond_fcn = unparse_anullcond,
  aconddefault_fcn = unparse_aconddefault,
  atestseq_fcn = unparse_atestseq,
  atest_fcn = unparse_atest,
  atestrec_fcn = unparse_atestrec,
  acaseclause_fcn = unparse_acaseclause,
  anullcase_fcn = unparse_anullcase,
  acasedefault_fcn = unparse_acasedefault,
  avardef_fcn = unparse_avardef,
  afundef_fcn = unparse_afundef,
  abegindef_fcn = unparse_abegindef,
  asimpletemp_fcn = unparse_asimpletemp,
  alisttemp_fcn = unparse_alisttemp,
  ailisttemp_fcn = unparse_ailisttemp,
  avectemp_fcn = unparse_avectemp,
  aunquote_fcn = unparse_aunquote,
  atemplate_fcn = unparse_atemplate,
  aunqspl_fcn = unparse_aunqspl,
  avarpar_fcn = unparse_avarpar,
  apairpar_fcn = unparse_apairpar,
  anullpar_fcn = unparse_anullpar,
  acommand_fcn = unparse_acommand,
  adefinition_fcn = unparse_adefinition,
  adatum_fcn = unparse_adatum}

  in
      mapacommand_or_definition F anncod
  end


 



  fun breakdist (es, after) =
                case es of
                  (BLOCK(_,_,len))::es' => len + breakdist(es',after)
                | (STR s)::es' => size s + breakdist(es',after)
                | (BRK _)::es' => 0
                | [] => after



  fun pp (os,e,margin) =
         let val space = ref margin
	     fun blanks n = if (n = 0) then () else (output(os," ");
						     space := !space - 1;
		      				     blanks (n-1))

             fun newline () = (output(os,"\n"); space := margin)

             fun printing ([],_,_) = ()
             |   printing (e::es,blockspace,after) =
                          (case e of
                             BLOCK(bes,indent,len) => printing(bes,!space - indent,
                                                               breakdist(es,after))        
	     	           | STR s => (output(os,s); space := !space - size s)
                           | BRK len => if len + breakdist(es,after) <= !space
                                        then blanks len 
                                        else (newline(); blanks(margin-blockspace));
			   printing(es,blockspace,after))
             in
	         (printing([e],margin,0); newline())
             end
		
					      
				       


 
(*----------------- Test ---------------------------------*)

 
  fun annotate_and_unparse_file infile outfile = 

           (output(std_out,"Loading from "^infile^" ...\n");
           let val instream = if (infile = "std_in") then std_in else open_in infile
               val outstream = if (outfile = "std_out") then std_out else open_out outfile
               val f = unparse_anncommand_or_definition o dat2annexp
               val margin = 80
           in 
               (while not (end_of_stream instream) do
                  (output(outstream,"\n");
                   let val cod = f(read_datum instream)
                   in
		       pp(outstream,cod,margin)
                   end
                   handle EOF => output(std_out,"End of input reached ..."));
		  
           
	       if (infile <> "std_in") then close_in instream else ();
	       if (outfile <> "std_out") then close_out outstream else ())
           end)
         
 












end
end
