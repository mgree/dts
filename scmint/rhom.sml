(* Structure SchemeRast gives a restricted view of annotated abstract syntax
   as given by datatype 'a anncommand_or_definition in structure SchemeAnnast.
   The structure offers a function parse which is parametrized over
   (1) AFR : Annotation function record of the form

               {ann_fcn,
               ann_lambda,
               ann_literal,
               ann_variable,
               ann_call,
               ann_if,
               ann_let,
               ann_vardef,
               ann_fundef,
               ann_begindef,
               ann_varpar,
               ann_pairpar,
               ann_command,
               ann_definition}
       
       This form reflects the restriction : Only lambda, literals, variables,
       


*)

(*
signature SCHEMERAST =
sig  
    type ('a,'b,'c,'d,'e,'f) afrec
    eqtype 'a anncommand_or_definition
    exception Undef
    val parse : ('a,'b,'c,'d,'e,'f) afrec
                -> (unit -> 'e) -> string -> 'd anncommand_or_definition list
    val r_hom : ('a,'b,'c,'d,'e,'f) afrec
                -> 'e anncommand_or_definition -> 'd anncommand_or_definition
  end

*)





structure SchemeRast (* : SCHEMERAST *)  = 
struct
 
  local open SchemeGeneral SchemeAnnast in


  type 'a anncommand_or_definition = 'a anncommand_or_definition
  

   datatype 'a ann_cnst = 
            A_BOOLCNST of bool |
            A_NUMCNST of int

  datatype 'a ann_expression =
            A_CNST of 'a * ('a ann_cnst) |
            A_VAR of 'a * string |
            A_CALL of 'a * ('a ann_expression) * ('a ann_expression) list |
            A_LAMBDA of 'a * ('a ann_formal) * ('a ann_expression) |
            A_IF of 'a * ('a ann_expression) * ('a ann_expression) * 
                         ('a ann_expression) Option |
            A_LET of 'a * ('a ann_bindingspec) list * ('a ann_expression) 
  and 'a ann_formal = 
            A_VARPAR of 'a * string
  and 'a ann_vardef =
            A_VARDEF of 'a * string * ('a ann_expression)
         
  withtype
      'a ann_bindingspec = string * ('a ann_expression) 
 
  datatype 'a ann_definition =
            A_DEFINITION of 'a * ('a ann_vardef)

  exception Undef



(*
  
  fun r_hom  anncod =

let
  val r_literal_fcn = fn x => x
  val r_ann_fcn = fn x => x
  val r_variable_fcn = fn x => x
  val r_call_fcn = fn x =>  x
  val r_lambda_fcn = fn x => x
  val r_if_fcn = fn x => x
  val r_assign_fcn = fn x => raise Undef
  val r_cond_fcn = fn x => raise Undef
  val r_case_fcn = fn x => raise Undef
  val r_and_fcn = fn x => raise Undef
  val r_or_fcn  = fn x => raise Undef
  val r_let_fcn = fn x => x
  val r_namedlet_fcn = fn x => raise Undef
  val r_lets_fcn = fn x => raise Undef
  val r_letrec_fcn = fn x => raise Undef
  val r_begin_fcn = fn x => raise Undef
  val r_do_fcn = fn x => raise Undef
  val r_delay_fcn = fn x => raise Undef
  val r_quasiquote_fcn = fn x => raise Undef
  val r_condclause_fcn = fn x => raise Undef
  val r_nullcond_fcn = fn x => raise Undef
  val r_conddefault_fcn = fn x => raise Undef
  val r_testseq_fcn = fn x => raise Undef
  val r_test_fcn = fn x => raise Undef
  val r_testrec_fcn = fn x => raise Undef
  val r_caseclause_fcn = fn x => raise Undef
  val r_nullcase_fcn = fn x => raise Undef
  val r_casedefault_fcn = fn x => raise Undef
  val r_vardef_fcn = fn x =>  x
  val r_fundef_fcn = fn x => raise Undef
  val r_begindef_fcn = fn x => raise Undef
  val r_simpletemp_fcn = fn x => raise Undef
  val r_listtemp_fcn = fn x => raise Undef
  val r_ilisttemp_fcn = fn x => raise Undef
  val r_vectemp_fcn = fn x => raise Undef
  val r_unquote_fcn = fn x => raise Undef
  val r_template_fcn = fn x => raise Undef
  val r_unqspl_fcn = fn x => raise Undef
  val r_varpar_fcn = fn x => x
  val r_pairpar_fcn = fn x => raise Undef
  val r_nullpar_fcn = fn x => raise Undef 
  val r_command_fcn = fn x => raise Undef
  val r_definition_fcn = fn x => x
  val r_adatum_fcn = fn x => x  




val F =
 {aliteral_fcn = r_literal_fcn,
  ann_fcn= r_ann_fcn,
  avariable_fcn = r_variable_fcn,
  acall_fcn = r_call_fcn,
  alambda_fcn = r_lambda_fcn,
  aif_fcn = r_if_fcn,
  aassign_fcn = r_assign_fcn,
  acond_fcn = r_cond_fcn,
  acase_fcn = r_case_fcn,
  aand_fcn = r_and_fcn, 
  aor_fcn  = r_or_fcn,
  alet_fcn = r_let_fcn,
  anamedlet_fcn = r_namedlet_fcn,
  alets_fcn = r_lets_fcn,
  aletrec_fcn = r_letrec_fcn, 
  abegin_fcn = r_begin_fcn, 
  ado_fcn = r_do_fcn,
  adelay_fcn = r_delay_fcn,
  aquasiquote_fcn = r_quasiquote_fcn,
  acondclause_fcn = r_condclause_fcn,
  anullcond_fcn = r_nullcond_fcn,
  aconddefault_fcn = r_conddefault_fcn,
  atestseq_fcn = r_testseq_fcn,
  atest_fcn = r_test_fcn,
  atestrec_fcn = r_testrec_fcn,
  acaseclause_fcn = r_caseclause_fcn,
  anullcase_fcn = r_nullcase_fcn,
  acasedefault_fcn = r_casedefault_fcn,
  avardef_fcn = r_vardef_fcn,
  afundef_fcn = r_fundef_fcn,
  abegindef_fcn = r_begindef_fcn,
  asimpletemp_fcn = r_simpletemp_fcn,
  alisttemp_fcn = r_listtemp_fcn,
  ailisttemp_fcn = r_ilisttemp_fcn,
  avectemp_fcn = r_vectemp_fcn,
  aunquote_fcn = r_unquote_fcn,
  atemplate_fcn = r_template_fcn,
  aunqspl_fcn = r_unqspl_fcn,
  avarpar_fcn = r_varpar_fcn,
  apairpar_fcn = r_pairpar_fcn,
  anullpar_fcn = r_nullpar_fcn,
  acommand_fcn = r_command_fcn,
  adefinition_fcn = r_definition_fcn,
  adatum_fcn = r_adatum_fcn} in

      mapacommand_or_definition F anncod
  end
 *)




  fun t_exp cod =
      case cod of 

           ALITERAL (a, d) => A_CNST (a, t_cnst d) |
           AVARIABLE (a, v) => A_VAR (a, v) |
           ACALL (a, (ae,ael)) => A_CALL (a, t_exp ae, map t_exp ael) |
           ALAMBDA (a, (AVARPAR (a', v), 
                        ([],[],ae))) => A_LAMBDA (a, A_VARPAR (a',v), t_exp ae) |
           AIF (a, (ae,ae',Some ae'')) => A_IF (a, t_exp ae, t_exp ae', t_exp ae'') |
           ALET (a, (bsl,([],[],ae'))) => A_LET (a, map t_bspec bsl, t_exp ae') |
           _ => raise Undef
      
  and t_cnst c =
      case c of
           ABOOLDAT b => A_BOOLCNST b |
           ANUMBDAT n => A_NUMCNST n |
           _ => raise Undef


  and t_bspec bs =
      case bs of
           (v, ae) => (v, t_exp ae) |
           _ => raise Undef

  and t_definition def =
      case def of
           ADEFINITION(a, AVARDEF (a', v, ae)) => 
                      A_DEFINITION(a, A_VARDEF (a',v,t_exp ae)) |
      _ => raise Undef
           

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
          val f = t_exp o (dat2annexp init)
          val codlst = 
              if (infile = "std_in") then [pf_stdin f []]
              else
                 pf_file f instream []
      in
          codlst
           
      end) 
      


 

end
end


