structure SchemeKernel  =
  struct
  local open SchemeGeneral SchemeNumber SchemeBool SchemeAst SchemeAnnast
             KernelTypes
  in

type atype = KernelTypes.atype

type 'a variable = 'a * string 

datatype 'a exp =
    EMPTY |
    LITERAL of 'a anndatum |
    VARIABLE of 'a variable |
    CALL of 'a annexp * 'a annargs |
    LAMBDA of 'a annformals * 'a annexp |
    IF of 'a annexp * 'a annexp * 'a annexp |  
    ASSIGN of 'a variable * 'a annexp
    

and 'a args =
    PAIRARG of 'a annexp * 'a annargs |
    NULLARG 
and 'a formals =
  AVARPAR of 'a variable |
  APAIRPAR of 'a variable * 'a annformals |
  ANULLPAR


and 'a datum = 
      ABOOLDAT of bool |
      ACHARDAT of string |
      ASTRIDAT of string |
      ASYMBDAT of string |
      ANUMBDAT of number |
      AVECTDAT of 'a anndatumlst |
      ALISTDAT of 'a anndatumlst |
      AILISTDAT of 'a anndatumilist

and 'a datumlst = 
  APAIRDAT of 'a anndatum * 'a anndatumlst |
  ANULLDAT


and 'a datumilist =
  AIPAIRDAT of 'a anndatum * 'a anndatumilist |
  LAST of 'a anndatum 


and 'a definition = 
    VARDEF of 'a variable * 'a annexp |
    FUNDEF of 'a variable * 'a annformals * 'a annexp |
    BEGINDEF of ('a anndefinition) list


and 'a command_or_definition =
    COMMAND of 'a annexp |
    DEFINITION of 'a anndefinition


withtype 'a annexp = 'a * 'a exp
and 'a anndatum = 'a * 'a datum
and 'a annargs = 'a * 'a args
and 'a annformals = 'a * 'a formals 
and 'a anndatumlst = 'a * 'a datumlst
and 'a anndatumilist = 'a * 'a datumilist
and 'a anndefinition = 'a * 'a definition
and 'a anncommand_or_definition = 'a * 'a command_or_definition




type constraints = ((atype * atype) list) ref


val Cset = (ref []) : constraints 

fun reset_Cset () = Cset := []


fun init () = (new_typevar(), new_typevar()) : (atype * atype)




exception ParseError of string;

exception LambdaError of string;

fun kdat2annexp ini d =

    let fun simpledat_fcn sd =
            case sd of
              BOOLDAT b => (ini(), ABOOLDAT b)
            | CHARDAT c => (ini(), ACHARDAT c)
            | STRIDAT s => (ini(), ASTRIDAT s)
            | SYMBDAT s => (ini(), ASYMBDAT s)
            | NUMBDAT n => (ini(), ANUMBDAT n)
            | _ => raise ParseError "This can't happen!\n"
        


        fun map_datlst f dl =
            case dl of
              []     => (ini(), ANULLDAT) 
            | hd::tl => (ini(), APAIRDAT(f hd, map_datlst f tl)) 



        fun mkilist (datlst, dat) =
            case datlst of
              [d] => (ini(), AIPAIRDAT (d, (ini(), LAST dat)))
            | d::tl => (ini(), AIPAIRDAT(d, mkilist (tl, dat)))
            | _ => raise ParseError ""


        fun datum_fcn x =
                     case x of
                       VECTDAT dl => (ini(), AVECTDAT (map_datlst datum_fcn dl))
                     | LISTDAT dl => (ini(), ALISTDAT (map_datlst datum_fcn dl))
                     | ILISTDAT (dl,d) => (ini(),AILISTDAT 
                                                 (mkilist (map datum_fcn dl, datum_fcn d)))
                     | d => simpledat_fcn d
       
        
  val literal_fcn = fn x => (ini(), LITERAL x) 
  val variable_fcn = fn x =>  (ini(), VARIABLE (ini(), x)) 

  val call_fcn = fn x =>  let fun randlst2annargs lst =
                                 case lst of
                                   []     => (ini(), NULLARG)
                                 | hd::tl => (ini(), PAIRARG(hd,randlst2annargs tl))
                              val (rator, randlst) = x 
                          in

                             (ini(), CALL(rator, randlst2annargs randlst))

                          end 

  val lambda_fcn = fn x => let val (fmls, annexp) = x 
                           in
                              case annexp of
                               ([], ([], annexp')) => (ini(), LAMBDA (fmls, annexp'))
                              | _ => raise LambdaError ""
                           end
                           

  val if_fcn = fn x => let val (annexp', annexp'', option) = x
                       in
                           case option of
                             None => (ini(), IF(annexp', annexp'', (ini(), EMPTY)))
                           | Some annexp''' => (ini(), IF(annexp', annexp'', annexp'''))
                       end
  val assign_fcn = fn x => let val (var, annexp) = x in (ini(), ASSIGN((ini(),var), annexp)) end

  val cond_fcn = fn x => raise ParseError ""
  val case_fcn = fn x =>raise ParseError ""
  val and_fcn = fn x =>raise ParseError ""
  val or_fcn  = fn x =>raise ParseError ""
  val let_fcn = fn x =>raise ParseError ""
  val namedlet_fcn = fn x =>raise ParseError ""
  val lets_fcn = fn x => raise ParseError ""
  val letrec_fcn = fn x => raise ParseError ""
  val begin_fcn = fn x => raise ParseError ""
  val do_fcn = fn x => raise ParseError ""
  val delay_fcn = fn x => raise ParseError ""
  val quasiquote_fcn = fn x => raise ParseError ""
  val condclause_fcn = fn x =>  raise ParseError ""
  val nullcond_fcn =  fn () =>  raise ParseError ""
  val conddefault_fcn = fn x =>  raise ParseError ""
  val testseq_fcn = fn x =>  raise ParseError ""
  val test_fcn = fn x => raise ParseError ""
  val testrec_fcn = fn x => raise ParseError ""
  val caseclause_fcn = fn x => raise ParseError ""
  val nullcase_fcn =  fn () => raise ParseError ""
  val casedefault_fcn = fn x => raise ParseError ""

  val vardef_fcn = fn x => let val (var, annexp) = x in (ini(), VARDEF ((ini(),var), annexp)) end

  val fundef_fcn = fn x => 

      let val (var, fmls, annexp) = x 
      in
          case annexp of
            ([], ([], annexp')) => (ini(), FUNDEF ((ini(), var), fmls, annexp'))
          | _ => raise ParseError ""
      end

  val begindef_fcn = fn x => (ini(), BEGINDEF x)

  val simpletemp_fcn = fn x => raise ParseError ""
  val listtemp_fcn = fn x => raise ParseError ""
  val ilisttemp_fcn = fn x => raise ParseError ""
  val vectemp_fcn = fn x => raise ParseError ""
  val unquote_fcn = fn x => raise ParseError ""
  val template_fcn = fn x => raise ParseError ""
  val unqspl_fcn = fn x => raise ParseError ""

  val varpar_fcn = fn x => (ini(), AVARPAR (ini(), x))

  val pairpar_fcn = fn x => let val (var, fmls) = x 
                            in
                               (ini(), APAIRPAR ((ini(), var),  fmls))
                            end 

  val nullpar_fcn =  fn () => (ini(), ANULLPAR)

  val command_fcn = fn x => (ini(), COMMAND x)

  val definition_fcn = fn x => (ini(), DEFINITION x)
  


  val F = 
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




fun AnnAst s =
      kdat2annexp init (read_datum s)          







(* ----------- Constraint Extraction ------------------------ *)


(* -------- Utilities ------------- *)

exception pr_aty_Error;
fun pr_uty uty = 
    case uty of
      VAR i => "a"^(makestring i)
    | SIMPLE (FUNC, [dom, ran]) => "("^(pr_aty dom)^"->"^(pr_aty ran)^")"
    | SIMPLE (BOOL, []) => "bool"
    | SIMPLE (NIL, []) => "nil"
    | SIMPLE (PAIR, [fst, snd]) => "(cons "^(pr_aty fst)^" "^(pr_aty snd)^")"
    | DYN [s1, s2, s3, s4] => "("^(pr_aty s1)^"+"^
                                  (pr_aty s2)^"+"^
                                  (pr_aty s3)^"+"^
                                  (pr_aty s4)^")"
    | _ => raise pr_aty_Error

and pr_aty aty =  
    case aty of
      ref (Some (uty, _)) => pr_uty uty
    | _ => raise pr_aty_Error                               







(* ----------- Extraction ----------- *)

exception SchemeKernelNotImplemented;



fun inC c = Cset := (c::(!Cset)) 


val emptyEnv = []

  
exception LookupError;
fun lookup var Env =
      case Env of 
        [] => raise LookupError
      | (v, ann)::tl => if var = v then ann else lookup var tl




fun inDom var Env = (lookup var Env; true) handle LookupError => false


fun extend newlst Env = newlst @ Env 



fun get_bindings annfmls =
    case annfmls of
      (_, ANULLPAR) => []
    | ((h,_), AVARPAR (_, var)) =>  [(var, h)]
    | (_, APAIRPAR (((h,_), var), annfmls')) => (var, h)::(get_bindings annfmls') 







fun Cadlst adlst =
    case adlst of
      ((h,l), ANULLDAT) => 
              let val nilty = make_type (NIL,[])
              in
                  (alias l nilty;   (* l = nil *)
                   inC (l,h);       (* l < h *)
                   (emptyEnv, h))
              end

    | ((h,l), APAIRDAT(ad, adlst')) =>
              let val (_, ad_h) = Cdatum ad
                  val (_, adlst_h) = Cadlst adlst'
                  val consty = make_type (PAIR, [ad_h, adlst_h])
              in
                  (alias l consty;    (* l = (cons ad_h adlst_h) *)
                   inC (l,h);         (* l < h *)
                   (emptyEnv, h))
              end

    


and Cadilist adilist =
    case adilist of 
      ((h,l), LAST (ad as ((h',l'), _))) => 
             let val (_, _) = Cdatum ad
             in
                 (alias h l;
                  alias l h';
                  (emptyEnv, h))
             end

   | ((h,l), AIPAIRDAT (ad, adilist')) => 
            let val (_, ad_h) = Cdatum ad
                val (_, adilist_h) = Cadilist adilist'
                val consty = make_type (PAIR, [ad_h, adilist_h])
            in
                (alias l consty;    (* l = (cons ad_h adilist_h) *)
                 inC (l,h);         (* l < h *)
                 (emptyEnv, h))
            end




and Cdatum ad = 
    case ad of
      ((h,l), ABOOLDAT _) => 
              let val boolty = make_type (BOOL, []) 
              in
                  (alias l boolty;  (* l = bool *)
                   inC (l,h);       (* l < h *)
                   (emptyEnv, h))   (* return empty environment *)
              end                              


    | ((h,l), ALISTDAT (adlst as ((h',l'), _))) =>
              let val (_,_) = Cadlst adlst 
              in
                  (alias h' l';
                   alias l h;
                   (emptyEnv, h))
              end

   | ((h,l), AILISTDAT (adilist as ((h',l'), _))) =>
             let val (_,_) = Cadilist adilist 
              in
                  (alias h' l';
                   alias l h;
                   (emptyEnv, h))
              end



    | _ => raise SchemeKernelNotImplemented          






infix 9 AAND;

infix 9 AINTER;

infix 9 AOR;

infix 9 AMINUS;

infix 9 AUNION;

fun A AINTER A' = 
(* unify all variables in the intersection of A and A' *)
    case A of
      [] => ()
    | (var, ann)::tl => if inDom var A' 
                        then (alias ann (lookup var A'); tl AINTER A')
                        else tl AINTER A'


fun phantom A A' =
(* 
   For all x in Dom(A) : if x not in Dom(A') then generate phantom coercion
                         A(x) < 'b where 'b is fresh
*)
   case A of
     [] => ()
   | (var, ann)::tl => if inDom var A' then phantom tl A'
                       else (inC (ann, new_typevar()); phantom tl A')


fun A AMINUS A' =
    case A of
      [] => []
    | (fst as (var, _))::tl => if inDom var A' then tl AMINUS A' else fst::(tl AMINUS A')


fun A AUNION A' = A @ (A' AMINUS A)


fun A AAND A' =
    (A AINTER A'; 
     A AUNION A')


fun A AOR A' =
    (A AINTER A';  (* first union all variables in the intersection of A and A' *)

     phantom A A';

     phantom A' A;
     
     A AUNION A')




fun Cannfmls annfmls A = 
    case annfmls of
      ((h,l), ANULLPAR) => 
              let val nilty = make_type (NIL, [])
              in
                  (alias l nilty;    (* l = nil *)
                   inC (h,l))        (* h < l *) 
              end
    
    | ((h,l), AVARPAR ((h',l'), var)) =>
              (alias h' l';    
               alias l h;   
               alias l h'; 
               if inDom var A then alias (lookup var A) l' else ())			   

    | ((h,l), APAIRPAR (((h',l'), var), annfmls' as ((h'',_),_))) =>
              let val _ = Cannfmls annfmls' (A AMINUS [(var,l')])
                  val consty = make_type (PAIR, [h', h''])
              in
                  (alias l' h'; 
                   inC (h, l);  
                   alias l consty;    (* l = (cons h' h'') *)
                   if inDom var A then alias (lookup var A) l' else ())
              end             
     



fun Cargs annargs  =
    case annargs of
      ((h,l), NULLARG) => 
              let val nilty = make_type (NIL,[])
              in
                  (alias l nilty;      (* l = nil *)
                   inC (l,h);          (* l < h *)
                   ([], h))
              end

    | ((h,l), PAIRARG (a, args')) =>
              let val (A, a_h) = Cexp a 
                  val (A', args'_h) = Cargs args' 
                  val consty = make_type (PAIR, [a_h, args'_h])
              in
                  (alias l consty;    (* l = (cons a_h args'_h) *)
                   inC (l,h);
                   (A AAND A', h))
              end



and Cexp annexp  = 
      case annexp of
        
       ((h:atype,l:atype), LITERAL (ad as ((h',l'), _))) => 
          let val (A, _) = Cdatum ad
          in
              (alias h' l';
               alias l h;
               (A,h))
          end
        

     | ((h,l), VARIABLE ((h',l'), var)) =>
              (alias h' l';
               inC (l,h);
               ([(var, l)], h))
          

      

    | ((h,l), LAMBDA (annfmls as ((h',_),_), annexp' as ((h'',_), _))) =>
              let val (A, annexp'_h) = Cexp annexp'
                  val parameter_bindings = get_bindings annfmls
                  val arrowty = make_type (FUNC, [h', h''])
              in
                  (Cannfmls annfmls A;
                   inC (l,h);
                   alias l arrowty;  
                   (A AMINUS parameter_bindings, h))
              end
                   
                  
              


    | ((h,l), IF (a1, a2, a3)) => 
              let val boolty = make_type (BOOL, [])
                  val (A1, h1) = Cexp a1 
                  val (A2, h2) = Cexp a2 
                  val (A3, h3) = Cexp a3 
              in
                  (alias h1 boolty;      (* bool = h1 *)
                   inC (l, h);          (* l < h *)
                   alias h2 h3;         (* h2 = h3     *)
                   alias h2 l;          (* h2 = l *)
                   (A1 AAND (A2 AOR A3), h))
              end

                


    | ((h,l), CALL(rator, args)) =>
              let val (A, rator_h) = Cexp rator 
                  val (A',args_h) = Cargs args 
                  val arrowtype = make_type (FUNC, [args_h, l])
              in
                  (inC (l,h);            (* l < h *)
                   alias rator_h arrowtype; (* rator_h = args_h -> l *)
                   (A AAND A', h))
              end

    | _ => raise SchemeKernelNotImplemented                  




fun C cmd_or_def =
    (Cset := nil;
    case cmd_or_def of
      ((h,l), COMMAND (annexp as ((h',l'),_))) =>
              let val (A, _) = Cexp annexp 
              in             
                  (alias h l;
                   alias l h';
                   (A, h))
              end

    | _ => raise SchemeKernelNotImplemented
    )
         
               






(* ------------------- Preds and Succs ------------------------------------ *)



fun pred_succ (l,h) =
    let val fl = find #aliasptr l
        val fh = find #aliasptr h
    in
        if fl = fh then () 
        else 
          let val pred_h = get_preds fh
              val succ_l = get_succs fl
          in
              (pred_h := fl :: (!pred_h);
               succ_l := fh :: (!succ_l))
          end  
    end



fun set_preds_and_succs() = map pred_succ (!Cset)




end
end (* struct local open *)

