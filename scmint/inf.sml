exception GetError;



(* DATATYPE Tyexp, Flowval, Constraint *)
(* ty ::= tyvar | Bool | nil | ty->ty | (cons ty ty) *)

datatype Tyexp   = EMPTYTYPE
                 | TV of Tyvar 
                 | BOOLEAN 
                 | NIL
                 | ARROW of Domain * Range 
                 | CONS of First * Second
                 | SUM of Flowref * Flowref * Flowref * Flowref

(* SUM is max free ordered sum : bool + nil + -> + cons *)

and      Flowval = EMPTYFLOW | FLOW of Adm * Leqref * Eqref * Tyref

and      Count   = NOTSEEN | SEEN | RECURSIVE

and      Mark    = NOMARK | MARKED

withtype 
         Tyvar   = int
and      Name    = int
and      Adm     = (Count ref) * (Name ref) * (Mark ref)
and      Tyref   = Tyexp ref
and      Domain  = Flowval ref
and      Range   = Flowval ref
and      First   = Flowval ref
and      Second  = Flowval ref

and      Leqref  = Flowval ref
and      Eqref   = Flowval ref (* reference for union-find equivalence class *)
and      Flowref = Flowval ref


datatype Constraint = EQ of Flowref * Flowref | LEQ of Flowref * Flowref


datatype tyconstructor = TYVAR | BOOLCON | NILCON | ARROWCON | CONSCON | SUMCON




(* Selectors *)

fun get_debug x = case !x of
                    FLOW(_,d,_,_) => d
                  | _           => raise GetError

exception GetLeqrefError;
fun get_leqref x = case !x of
                    FLOW(_,d,_,_) => d
                  | _           => raise GetLeqrefError

exception GetEqrefError; 
fun get_eqref x = case !x of
                    FLOW(_,_,e,_) => e
                  | _           => raise GetEqrefError 

exception GetTyrefError;
fun get_tyref x = case !x of
                    FLOW(_,_,_,t) => t
                  | _           => raise GetTyrefError 

exception GetAdmError;
fun get_adm x =  case !x of
                   FLOW (a,_,_,_) => a
                 | _ => raise GetAdmError




fun get_countref x = let val (c,_,_) = get_adm x in c end

fun get_nameref x = let val (_,n,_) = get_adm x in n end

fun get_markref x = let val (_,_,m) = get_adm x in m end


fun get_domain x = case !x of
                     ARROW(d,_) => d
                   | _          => raise GetError

fun get_range x = case !x of
                     ARROW(_,r) => r
                   | _          => raise GetError


fun get_first x = case !x of
                     CONS (f,_) => f
                   | _          => raise GetError


fun get_second x = case !x of
                     CONS (_,s) => s
                   | _          => raise GetError


fun get_left x = case x of
                   EQ (fr,_)  => fr
                 | LEQ (fr,_) => fr



fun get_right x = case x of
                   EQ (_,fr)   => fr
                 | LEQ (_,fr) => fr




                  




(* Creators *)



local val c = ref 0
in
   fun counter () = (c := !c + 1; !c)

   fun reset_counter() = c := 0
end






fun new_flowval() =
     let val adm     = (ref NOTSEEN, ref 0, ref NOMARK)
         val dref    = ref EMPTYFLOW
         val eqref   = ref EMPTYFLOW
         val tyref   = ref EMPTYTYPE
         val flowval = ref (FLOW(adm,dref,eqref,tyref))
     in
         (eqref := ! flowval; flowval)
         (* At initialization, the eqref points to its own record *)
     end
     


fun newvar() = TV (counter())


fun newsumvar() = TV (~(counter()))

(* creators of flowvalues *)
fun mkfv_tyvar tv = 
      let val nfv = new_flowval() in
          ((get_tyref nfv) := tv; nfv)
      end
          


fun mkfv_newtyvar() = mkfv_tyvar (newvar())




fun mkfv_boolean() =
      let val nfv = new_flowval() in
          ((get_tyref nfv) := BOOLEAN; nfv)
      end


fun mkfv_nil() =
     let val nfv = new_flowval() in
          ((get_tyref nfv) := NIL; nfv)
      end 


fun mkfv_arrow domain range =
      let val nfv = new_flowval() in
          ((get_tyref nfv) := (ARROW(domain,range)); nfv)
      end

          
fun mkfv_cons first second =
      let val nfv = new_flowval() in
          ((get_tyref nfv) := (CONS(first,second)); nfv)
      end           


fun mkfv_newsum() =
    let val nfv = new_flowval() in
        ((get_tyref nfv) := (SUM (ref EMPTYFLOW, 
                                  ref EMPTYFLOW, 
                                  ref EMPTYFLOW, 
                                  ref EMPTYFLOW)); 
          nfv)
    end



fun mkfv_newsumtyvar() = mkfv_tyvar (newsumvar())



exception InsertSumError;

fun insert_sum fvref sum =
      (let val (SUM (boolref, nilref, arrowref, consref)) = ! (get_tyref sum) 
      in
        case ! (get_tyref fvref) of
          BOOLEAN => boolref := ! fvref
        | NIL => nilref := ! fvref
        | ARROW (_,_) => arrowref := ! fvref
        | CONS (_,_) => consref := ! fvref
        | _ => raise InsertSumError
      end;
      sum)


fun freshvars_sum sum =
    let val (SUM(boolref,nilref,arrowref,consref)) = ! (get_tyref sum)
    in
       (if (! boolref) = EMPTYFLOW then boolref := ! (mkfv_newsumtyvar()) else ();
        if (! nilref)  = EMPTYFLOW then nilref := ! (mkfv_newsumtyvar()) else (); 
        if (! arrowref) = EMPTYFLOW then arrowref := ! (mkfv_newsumtyvar()) else ();
        if (! consref) = EMPTYFLOW then consref := ! (mkfv_newsumtyvar()) else ();
        sum)
    end


fun mkfv_sum_of_two fvref1 fvref2 = 
    let val nsum = mkfv_newsum() in
        (freshvars_sum (insert_sum fvref2 (insert_sum fvref1 nsum)))
    end
    


fun mkfv_universal_sum() = freshvars_sum (mkfv_newsum())




fun mkcstr_eq left right = EQ (left,right)

fun mkcstr_leq left right = LEQ (left,right) 








(* I/O Utilities *)







exception Flowref2tystrError;

fun flowref2tystr fr = 
      if (! fr) = EMPTYFLOW then raise Flowref2tystrError
      else tyref2tystr (get_tyref fr)

and tyref2tystr tr = 
      case (! tr) of
        EMPTYTYPE           => "emptytype"
      | TV tyvar            => "a"^(makestring tyvar)
      | BOOLEAN             => "bool"
      | NIL                 => "nil"
      | ARROW(d,r)          => "("^(flowref2tystr d)^"->"^(flowref2tystr r)^")"
      | CONS(f,s)           => "(cons "^(flowref2tystr f)^" "^(flowref2tystr s)^")"
      | SUM(s1, s2, s3, s4) => "("^(flowref2tystr s1)^"+"^
                                   (flowref2tystr s2)^"+"^
                                   (flowref2tystr s3)^"+"^
                                   (flowref2tystr s4)^")"
      











(* CONSTRAINT SOLVING *)







fun inLEQ(fr1,fr2) =
    let val l1 = ref EMPTYFLOW
        val l2 = ref EMPTYFLOW
    in
        (l1 := !fr1; l2 := !fr2; LEQ(l1,l2))
    end


fun inEQ(fr1,fr2) =
    let val l1 = ref EMPTYFLOW
        val l2 = ref EMPTYFLOW
    in
        (l1 := !fr1; l2 := !fr2; EQ(l1,l2))
    end



(* Global constraint list ref called worklist *)
val worklist = (ref []): (Constraint list) ref


fun reset_wl() = worklist := []

fun add_to_wl c = worklist := (c :: (! worklist))




exception Get_constraintError;
fun get_constraint() = 
      case !worklist of
        c :: tl => (worklist := tl; c)
      | _       => raise Get_constraintError



exception ConstrError;
fun constr flowref =
      case !(get_tyref flowref) of
        TV _         => TYVAR
      | BOOLEAN      => BOOLCON
      | NIL          => NILCON
      | ARROW(_,_)   => ARROWCON
      | CONS(_,_)    => CONSCON
      | SUM(_,_,_,_) => SUMCON
      | _            => raise ConstrError



fun is_sum flowref = (constr flowref) = SUMCON



exception EError;
fun E l r =
     case (!(get_tyref l), !(get_tyref r)) of
       (SUM(s1,s2,s3,s4), SUM(s1',s2',s3',s4')) =>
          (add_to_wl (inEQ(s1, s1')); add_to_wl (inEQ(s2, s2'));
           add_to_wl (inEQ(s3, s3')); add_to_wl (inEQ(s4, s4')))
     | (ARROW(dom, ran), ARROW(dom', ran')) =>
          (add_to_wl (inEQ(dom,dom')); 
           add_to_wl (inEQ(ran, ran'))) 
     | (CONS(fst, snd), CONS(fst', snd')) =>
          (add_to_wl (inEQ(fst, fst')); add_to_wl (inEQ(snd, snd')))
     | (BOOLEAN, BOOLEAN) => () 
     | (NIL, NIL) => ()
     | (TV _ , _) => add_to_wl (inEQ(l,r))
     | (_, TV _) => add_to_wl (inEQ(l,r))
     | _          => raise EError 



exception Select_summandError;
fun select_summand tycon sum =
    case !(get_tyref sum) of
      SUM(s1,s2,s3,s4) => case tycon of
                            BOOLCON  => s1
                          | NILCON   => s2
                          | ARROWCON => s3
                          | CONSCON  => s4
    | _                => raise Select_summandError


fun do_leq_sum l sum  =                        (* l < Sum, [N3] *)
      E l (select_summand (constr l) sum)




fun do_leq_uneq_constr l' l'' r' =
      let val newsum = mkfv_sum_of_two l' l''
      in
          (add_to_wl (inEQ(r', newsum));
          (get_leqref r') := EMPTYFLOW) (* added *)
      end


(*
exception Do_both_cnstError;
fun do_both_cnst l r =
    case (!(get_tyref l), !(get_tyref r)) of
      (BOOLEAN, BOOLEAN) => union l r
    | (NIL, NIL)         => union l r
    | _                  => raise Do_both_cnstError

*)


fun do_eq_tyvar l r u =
      (* l and r are both rep's, so u is either l or r *)
      let val other = if isrep l then r else l
      in
          if (leqdefined other) then (
             add_to_wl (inLEQ(find (get_leqref other), u)))
          (*  (get_leqref  other) := EMPTYFLOW) *)
          else ()
      end
             



exception Do_both_cnstError;
(* 
   This function just checks that the conditions of its call (in Solve) 
   are as expected.
*)
fun do_both_cnst l r =
      case (constr l, constr r) of
        (BOOLCON, BOOLCON) => ()
      | (NILCON, NILCON)   => ()
      | _                  => raise Do_both_cnstError
















fun outlst strlst = map (fn s => output(std_out, s)) strlst




fun print_con c =
      case c of
        EQ(l,r) => (fr2tystr (find l))^" = "^(fr2tystr (find r))^"\n"
      | LEQ(l,r) => (fr2tystr (find l))^" < "^(fr2tystr (find r))^"\n"



fun show_wl() = (outlst (map print_con (!worklist)); ())
    






fun Solve() =
    
    while (!worklist) <> [] do (
    if !deb then (output(std_out, "Worklist:\n"); show_wl()) else (); 
    let val c = get_constraint()
    in
          
       case c of
       
         LEQ(l,r) =>
            let val l' = ref (!(debug "find1"; find l))
                val r' = ref (!(debug "find2"; find r))
            in
                if is_sum r' then
                   (debug "do-leq-sum"; 
                   do_leq_sum l' r' )   (* l' < Sum, [N3] *)
                else
                  if not (leqdefined r') then                (* l' < r' *)
                    (* just record that inequality is seen *)
                    (debug "get_leqref";
                    (get_leqref r') := ! l' )                    
                  else 
                    let val l'' = (debug "find3"; find (get_leqref r') )      (* l' , l'' < r' *)
                    in
                        if (constr l') = (constr l'') then   (* constr(l') = constr(l''), [N1] *)
                           (debug "E1"; 
                            E l' l'')
                        else                                 (* constr l' <> constr r'', [N2] *)
                           (debug "do_leq_uneq_constr";
                           do_leq_uneq_constr l' l'' r')
                    end
            end

       | EQ(l,r) =>
           let val l' = ref (!(debug "find4"; find l))
               val r' = ref (!(debug "find5"; find r))
           in
               if (! l') = (! r') then () else
(*           let val u  = (debug "union1"; union l' r')
           in *)
               case (constr l', constr r') of
                 (SUMCON, SUMCON) => (debug "E2";
                                      E l' r')                    (* Sum = Sum,   [N5] *)
               | (ARROWCON, ARROWCON) => (debug "E3";
                                          E l' r')

               | (TYVAR, _)       => (debug "do_eq_tyvar1";
                                      let val u = union l' r' in
                                      do_eq_tyvar l' r' u end)       (* 'a = 'b,   [N6,7] *)
               | (_, TYVAR)       => (debug "do_eq_tyvar2";
                                      let val u = union l' r' in
                                      do_eq_tyvar l' r' u end)       (* 'a = 'b,   [N6,7] *)


               | (_, _)           => (debug "do_both_cnst";
                                       (* do_both_cnst l' r' ; *)())   (*  c =  c,     [N4] *)
                                     (* In this case both constructors must be the same cnst *)
           (* end (* let val u *) *)
             
                   
           end (* let val l' *)

     end (* let val c = getconstraint() *)
     ) (* while *)





















(* ABSTRACT SYNTAX *)




datatype Asyn =   VAR of Variable 
                | CNST of string
                | LAM of Varpar * Asyn
                | APP of Asyn * Asyn
                | PAIR 
                | CAR 
                | CDR 
                | COND of Asyn * Asyn * Asyn
                | NULL 
                | DEF of Variable * Asyn
                | ISNULL (* universal predicate *)
                | PEQ    (* polymorphic equality *)

and   Varpar = VARPAR of Variable


withtype
      Variable = string
        



exception  AnnPlistError;


(* Annotated AST  *)
datatype 'a Ast = AVAR of 'a * Variable    (* body occurrence of variable *)
                | ACNST of 'a * string
                | ALAM of 'a * 'a AVarpar * 'a Ast
                | AAPP of 'a * 'a Ast * 'a Ast
                | APAIR of 'a * 'a * 'a * 'a * 'a
                | ACAR of 'a * 'a * 'a * 'a 
                | ACDR of 'a * 'a * 'a * 'a
                | ACOND of 'a * 'a Ast * 'a Ast * 'a Ast
                | ANULL of 'a
                | ADEF of 'a * 'a * Variable * 'a Ast
                | AISNULL of 'a * 'a
                | APEQ of 'a * 'a * 'a * 'a * 'a 

and   'a AVarpar = AVARPAR of 'a * Variable



        

(* Asyn -> Flowref Ast *)



fun AnnAst asyn = 
      case asyn of
        VAR v                         => AVAR  (mkfv_newtyvar(), v)
      | CNST s                        => ACNST (mkfv_newtyvar(), s)
      | LAM (vpar, asyn')             =>  ALAM (mkfv_newtyvar(), AnnVpar vpar, AnnAst asyn')
      | APP (asyn',asyn'')            => AAPP  (mkfv_newtyvar(), AnnAst asyn', AnnAst asyn'')
      | PAIR                          => APAIR (mkfv_newtyvar(), mkfv_newtyvar(),mkfv_newtyvar(),
                                                mkfv_newtyvar(),mkfv_newtyvar())
      | CAR                           => ACAR  (mkfv_newtyvar(),
                                                mkfv_newtyvar(),
                                                mkfv_newtyvar(),
                                                mkfv_newtyvar())
      | CDR                           => ACDR  (mkfv_newtyvar(),mkfv_newtyvar(),
                                                mkfv_newtyvar(),mkfv_newtyvar())
      | COND (asyn', asyn'', asyn''') => ACOND (mkfv_newtyvar(),
                                                AnnAst asyn',
                                                AnnAst asyn'',
                                                AnnAst asyn''')
      | NULL                          => ANULL  (mkfv_newtyvar())
      | DEF (v, asyn')                => ADEF  (mkfv_newtyvar(), 
                                                mkfv_newtyvar(), v, 
                                                AnnAst asyn') 
      | ISNULL                   => AISNULL(mkfv_newtyvar(),mkfv_newtyvar())
      | PEQ                           => APEQ (mkfv_newtyvar(), mkfv_newtyvar(),mkfv_newtyvar(),
                                               mkfv_newtyvar(),mkfv_newtyvar())

               
and AnnVpar (VARPAR v) = AVARPAR (mkfv_newtyvar(),v)                
         









(* CONSTRAINT EXTRACTION *)






val eqs = (ref []): (Constraint list) ref

val leqs = (ref []): (Constraint list) ref



fun show_con() = 
      (output(std_out, "Equality constraints :\n");
       outlst (map print_con (!eqs));
       output(std_out, "Inequality constraints :\n");
       outlst (map print_con (!leqs)))



fun reset_constraints () = (eqs := ([]: (Constraint list));
                            leqs := ([]: (Constraint list))) 


(* List of references to flowval's occurring at lambda parameter binding points.
   All free and bound variables are assumed to be distinct. *)
val lampars = (ref []) : ((Variable * Flowref) list) ref 


fun reset_lampars() = lampars := []



(* Record that we have met binding occurrence of var, with flowref = [var] *)
fun in_lampars var flowref = lampars := (var, flowref) :: (! lampars)


exception In_lampars_plistError;


fun in_eqs c = eqs := (c :: (! eqs))

fun in_leqs c = leqs := (c :: (! leqs))


exception GetFlowrefError;

(* lookup the flowref associated with var in lampars *)
fun get_flowref var = 
    let fun lookup v l = 
            case l of
              [] =>  raise GetFlowrefError
            | (v', r)::tl => if v' = v then r else lookup v tl
    in
        lookup var (! lampars)
    end      



exception TypeofError;

fun typeof x =
      case x of
        "true" => mkfv_boolean()
      | "false" => mkfv_boolean() 
      |  _      => raise TypeofError




fun get_ann ast =
      case ast of
        AVAR (a, _)     => a
      | ACNST (a, _)    => a
      | ALAM (a,_,_)    => a
      | AAPP (a,_,_)    => a
      | APAIR (a,_,_,_,_)   => a
      | ACAR  (a,_,_,_) => a
      | ACDR (a,_,_,_)  => a
      | ACOND (a,_,_,_) => a
      | ANULL a         => a
      | ADEF (a,_,_,_)  => a 
      | AISNULL (a,_)   => a 
      | APEQ (a,_,_,_,_)   => a








fun universal_ineq flowref =
    let val b = mkfv_boolean()
        val n = mkfv_nil()
        val a = mkfv_arrow (mkfv_newtyvar()) (mkfv_newtyvar())
        val c = mkfv_cons (mkfv_newtyvar()) (mkfv_newtyvar())
    in
        (in_leqs (inLEQ(b, flowref));
         in_leqs (inLEQ(n, flowref));
         in_leqs (inLEQ(a, flowref));
         in_leqs (inLEQ(c, flowref)))
    end






fun C ast =
      case ast of
        AVAR (a, v) => let val binding_occ = get_flowref v in
                           in_eqs (inEQ (a,binding_occ))
                       end
      | ACNST (a, s) => in_leqs (inLEQ (typeof s, a)) 
      | ALAM (a, AVARPAR (a', v), ast'') =>
              let val arrow = mkfv_arrow a' (get_ann ast'')
              in
                 (
                  (* [a'] -> [ast''] < [ALAM] *)
                  in_leqs (inLEQ(arrow, a)); 

                  (* Record that we have met binding occurrences of vars in parameter list *)
                  in_lampars v a';

                  C ast''
                 )            
              end

      | AAPP (a, ast', ast'') => 
             let val arrow = mkfv_arrow (get_ann ast'') a
             in
               (
                (* [ast''] -> [AAPP] < [ast'] *) 
                in_leqs (inLEQ(arrow, (get_ann ast'))); 

                C ast'; C ast'')
             end  


      | APAIR (a, etavar1, etavar2, appvar1, appvar2) =>
             let val consty = mkfv_cons etavar1 etavar2
                 val arrow1 = mkfv_arrow etavar2 appvar1
                 val arrow2 = mkfv_arrow etavar1 appvar2
             in
                (
                  in_leqs(inLEQ(consty, appvar1));

                  in_leqs(inLEQ(arrow1, appvar2));

                  in_leqs(inLEQ(arrow2, a))
                )
             end

      | ANULL a => 
             let val nilty = mkfv_nil() 
             in 
                (* nil < [ANIL] *)  
                in_leqs (inLEQ(nilty, a))
             end

      

      | ACAR (a, etavar, freshvar, appvar)  =>
             let val consty = mkfv_cons appvar freshvar
                 val arrow = mkfv_arrow etavar appvar
             in
               (
                 in_leqs(inLEQ(consty, etavar));

                 in_leqs(inLEQ(arrow, a))

               )
             end


      | ACDR (a, etavar, freshvar, appvar) =>
             let val consty = mkfv_cons freshvar appvar 
                 val arrow = mkfv_arrow etavar appvar
             in
               (
                 in_leqs(inLEQ(consty, etavar));

                 in_leqs(inLEQ(arrow, a))

               )
             end


      | ACOND (a, ast', ast'', ast''') =>
              let val boolty = mkfv_boolean()
              in
                (
                  (* bool < [ast'] *)
                  in_leqs (inLEQ(boolty, get_ann ast'));

                  (* [ast''] = [ast'''] *)
                  in_eqs (inEQ(get_ann ast'', get_ann ast'''));
                      
                  (* [ACOND] = [ast''] *)
                  in_eqs (inEQ(a, get_ann ast''));

                  C ast'; C ast''; C ast'''
                )
              end


      | ADEF (a, a', v, ast') =>
             (
               (* [ADEF] = [v] *)
               in_eqs (inEQ(a, a'));

               (* [v] = [ast'] *)
               in_eqs (inEQ(a', get_ann ast'));

               (* Record that we have met binding occurrences of var in parameter list *)
               in_lampars v a';


               C ast'
             )
      

     | AISNULL(a, etavar) =>
                  let val etaarrow = mkfv_arrow etavar (mkfv_boolean())
                  in
                    (
                     universal_ineq etavar;

                     in_leqs (inLEQ(etaarrow, a))                     

                    )
                  end

    | APEQ (a, etavar1, etavar2, appvar1, appvar2) =>
           let val arrow1 = mkfv_arrow etavar2 appvar1
               val arrow2 = mkfv_arrow etavar1 appvar2
           in
             (
               in_leqs(inLEQ(mkfv_boolean(), appvar1));

               in_leqs(inLEQ(arrow1, appvar2));

               in_leqs(inLEQ(arrow2, a));

               in_eqs (inEQ(etavar1, etavar2))
             )
            end
                
                  



fun show annast = fr2tystr (find (get_ann annast))











(* TRANSLATION TO TYPES *)



fun count2str count =
      case count of
        NOTSEEN   => "NOTSEEN"
      | SEEN      => "SEEN"
      | RECURSIVE => "RECURSIVE"
  
fun adm2str flowref =
     let val (c,n) = (!(get_countref flowref), !(get_nameref flowref))
     in
         (count2str c, makestring n)
     end




fun ismarked flowref = 
      let val m = !(get_countref flowref) in
          (m = SEEN) 
      end
      



fun isrecmarked flowref =
    (!(get_countref flowref)) = RECURSIVE



exception MarkError;
fun mark flowref = 
      let val cref = get_countref flowref in
        case !cref of
          NOTSEEN => (cref := SEEN ; (get_nameref flowref) := (counter()))
          | _ => ()
      end 



fun recmark flowref = (get_countref flowref) := RECURSIVE


exception NameError;
fun name flowref =
     let val cref = get_countref flowref in
       case !cref of
         NOTSEEN => (cref := SEEN; (get_nameref flowref) := (counter()))
       | SEEN    => ()
       | _       => raise NameError
     end


fun get_name node = !(get_nameref node)
    


datatype Pty = PTV of Tyvar
              | PBOOLEAN
              | PNIL
              | PARROW of Pty * Pty
              | PCONS of Pty * Pty
              | PSUM of Pty * Pty * Pty * Pty 
              | PREC of Tyvar * Pty





fun occs tyvar pty =
      case pty of
        PTV tyvar' => if tyvar = tyvar' then 1 else 0
      | PARROW(pr, pr')           => (occs tyvar pr) + (occs tyvar pr')
      | PCONS (pr, pr')           => (occs tyvar pr) + (occs tyvar pr')
      | PSUM (pr1, pr2, pr3, pr4) => (occs tyvar pr1) + (occs tyvar pr2) +
                                     (occs tyvar pr3) + (occs tyvar pr4)
      | PREC (_, pr)              =>  occs tyvar pr
      | _                         =>  0



fun occursin tyvar pty = (occs tyvar pty) > 0


fun clean_pty pty =
      case pty of
        PARROW (pr, pr') => PARROW(clean_pty pr, clean_pty pr')
      | PCONS (pr, pr')  => PCONS(clean_pty pr, clean_pty pr')
      | PSUM (pr1, pr2, pr3, pr4) => PSUM(clean_pty pr1,
                                          clean_pty pr2,
                                          clean_pty pr3,
                                          clean_pty pr4)
      | PREC(tyvar, pr) => if occursin tyvar pr then PREC(tyvar, clean_pty pr)
                           else clean_pty pr
      | _ => pty






(* --------- Pruning of negative sum variables -------------------- *)


fun filter L P = 
      case L of
        []    => []
      | x::tl => if P x then x::(filter tl P) else filter tl P


fun isrealPty x =
      case x of
        PTV tyvar => tyvar > 0
      | _         => true


(* Take list of summands (Ptys) and remove those that are negative variables *)

fun prune_sum summands = filter summands isrealPty

fun sumPty summands =
      let fun s(x,y) = if y = "" then Pty2str x else (Pty2str x)^"+"^y in
          "("^(fold s summands "")^")"
      end
(* ---------------------------------------------------------------- *)

and  Pty2str pty =
      case pty of
        PTV tyvar              => "a"^(makestring tyvar)
      | PBOOLEAN                  => "bool"
      | PNIL                      => "nil"
      | PARROW (pr, pr')          => "("^(Pty2str pr)^"->"^(Pty2str pr')^")"
      | PCONS (pr, pr')           => "(cons "^(Pty2str pr)^" "^(Pty2str pr')^")"
      | PSUM (pr1, pr2, pr3, pr4) => sumPty (prune_sum [pr1,pr2,pr3,pr4])

      | PREC(tyvar, pr)           => "rec "^("a"^(makestring tyvar))^"."^(Pty2str pr)




(* Without pruning of negative sum variables : *)
(*
 fun Pty2str pty =
      case pty of
        PTV tyvar              => "a"^(makestring tyvar)
      | PBOOLEAN                  => "bool"
      | PNIL                      => "nil"
      | PARROW (pr, pr')          => "("^(Pty2str pr)^"->"^(Pty2str pr')^")"
      | PCONS (pr, pr')           => "(cons "^(Pty2str pr)^" "^(Pty2str pr')^")"
      | PSUM (pr1, pr2, pr3, pr4) => "("^(Pty2str pr1)^"+"^
                                         (Pty2str pr2)^"+"^                          
                                         (Pty2str pr3)^"+"^
                                         (Pty2str pr4)^")"
      | PREC(tyvar, pr)           => "rec "^("a"^(makestring tyvar))^"."^(Pty2str pr)

*)







(*
   Translate cyclic type graph into recursive, printable type exp, 
   using recursive abstraction (rec 'a. ... ) 
*)
exception Do_nodeError;
fun do_node node =
      case !(get_tyref node) of
        TV tyvar         => PTV tyvar
      | BOOLEAN          => PBOOLEAN
      | NIL              => PNIL
      | ARROW(dom,ran)   => PARROW (recty2Pty dom, recty2Pty ran)
      | CONS(fst,snd)    => PCONS (recty2Pty fst, recty2Pty snd)
      | SUM(s1,s2,s3,s4) => PSUM (recty2Pty s1, recty2Pty s2,
                                  recty2Pty s3, recty2Pty s4)
      | _ => raise Do_nodeError
	


and recty2Pty flowref =
      if isvariable flowref then
         if ismarked flowref then
            PTV(get_name flowref)
         else (* an unmarked variable *)
            if isvariable (find flowref) then do_node (find flowref)
            else (* an unmarked variable with a non-variable eqref *)
               (mark flowref; PREC(get_name flowref, do_node (find flowref)))
      else (* not a variable *)
         do_node flowref 
            


















val reclst = ref ([] : (Tyvar * Pty) list)


fun reset_reclst() = reclst := []


fun inRec(name, pty) =
      reclst := (!reclst)@[(name,pty)]




fun reclst2str() =
     let fun receq2str(name,pty) =
             "a"^(makestring name)^" = "^(Pty2str pty)
     
         fun folder (x,y) = (receq2str x)^"\n"^y
    in
         fold folder (!reclst) ""
     end


fun show_reclst() =
    output(std_out, reclst2str())



fun Pty2strEQ Pty =
      let val s      = Pty2str Pty 
          val receqs = reclst2str()
      in
          if receqs = "" then s else  
          "LETREC\n"^receqs^"IN\n"^s
      end




(*
   Translate cyclic type graph into recursive, printable type exp, 
   using recursion equations. 
*)

fun do_nodeEQ node =
      case !(get_tyref node) of
        TV tyvar         => PTV tyvar
      | BOOLEAN          => PBOOLEAN
      | NIL              => PNIL
      | ARROW(dom,ran)   => PARROW (recty2PtyEQ dom, recty2PtyEQ ran)
      | CONS(fst,snd)    => PCONS (recty2PtyEQ fst, recty2PtyEQ snd)
      | SUM(s1,s2,s3,s4) => PSUM (recty2PtyEQ s1, recty2PtyEQ s2,
                                  recty2PtyEQ s3, recty2PtyEQ s4)
      | _ => raise Do_nodeError
	



and recty2PtyEQ flowref =
      if isvariable flowref then
         if ismarked flowref then
            PTV(get_name flowref)
         else (* an unmarked variable *)
            if isvariable (find flowref) then do_nodeEQ (find flowref)
            else (* an unmarked variable with a non-variable eqref *)
               (mark flowref; 
                let val Pt = do_nodeEQ (find flowref) in
                    (inRec(get_name flowref, Pt);
                     recty2PtyEQ (find flowref))
                end)
      else (* not a variable *)
         do_nodeEQ flowref 







fun in_righthandside name l =
      case l of
        [] => false
      | (_,pty')::tl => if occursin name pty' then true
                            else in_righthandside name tl


fun clean_reclst l pty =
      case l of
        [] => []
      | (name, pty')::tl => if (occursin name pty) then
                               (name,pty')::(clean_reclst tl pty)
                            else
                                if (in_righthandside name tl) then  
                                   (name,pty')::(clean_reclst tl pty)
                                else
                                     clean_reclst tl pty

                                  
fun cleanRecty pty =
    let val reclst' = (clean_reclst (! reclst) pty)
    in    
        if reclst' = (! reclst) then reclst'
        else (reclst := reclst'; cleanRecty pty)
    end






(*------------------ begin experiments ---------------------------- *)







(*
 
fun mark_node node =
      case !(get_tyref node) of
        ARROW(dom,ran)   => (mark_graph dom; mark_graph ran)
      | CONS(fst,snd)    => (mark_graph fst; mark_graph snd)
      | SUM(s1,s2,s3,s4) => (mark_graph s1; mark_graph s2;
                            mark_graph s3; mark_graph s4)
      | _ => ()
	
           
and mark_graph flowref =
      if isvariable flowref then
         if ismarked flowref then ()
         else
             if isvariable (find flowref) then ()
             else
                 (mark flowref; mark_node (find flowref))
      else
           mark_node flowref


fun do_nodeEQ node =
      case !(get_tyref node) of
        TV tyvar         => if ismarked node then PTV tyvar else
                            do_nodeEQ (find node)
      | BOOLEAN          => PBOOLEAN
      | NIL              => PNIL
      | ARROW(dom,ran)   => PARROW (do_nodeEQ dom, do_nodeEQ ran)
      | CONS(fst,snd)    => PCONS (do_nodeEQ fst, do_nodeEQ snd)
      | SUM(s1,s2,s3,s4) => PSUM (do_nodeEQ s1, do_nodeEQ s2,
                                  do_nodeEQ s3, do_nodeEQ s4)
      | _ => raise Do_nodeError
	



and recty2PtyEQ flowref =
     case !(get_tyref flowref) of
        TV tyvar         => if isrecmarked flowref then 
                               PTV (get_name flowref)
                            else 
                                if ismarked flowref then
                                   (recmark flowref;
                                    let val Pt = recty2PtyEQ (find flowref) in
                                      (inRec(get_name flowref, Pt);
                                      Pt) 
                                 end
                                   )
                                 else 
                                     recty2PtyEQ (find flowref)
      | BOOLEAN          => PBOOLEAN
      | NIL              => PNIL
      | ARROW(dom,ran)   => PARROW (recty2PtyEQ dom, recty2PtyEQ ran)
      | CONS(fst,snd)    => PCONS (recty2PtyEQ fst, recty2PtyEQ snd)
      | SUM(s1,s2,s3,s4) => PSUM (recty2PtyEQ s1, recty2PtyEQ s2,
                                  recty2PtyEQ s3, recty2PtyEQ s4)
      | _ => raise Do_nodeError



*)


(* ------------------ end experiments ------------------------------------------ *)



(* Generate recursion equations directly from type graph *)



val peqlst = (ref []): ((string list) ref)


fun reset_peqlst() = peqlst := []

fun in_peqlst eqstring =
      peqlst := (!peqlst)@[eqstring^"\n"]



fun printeq_node node =
      (if (isrep node) then () 
       else      
            in_peqlst ((flowref2tystr node)^" = "^(flowref2tystr (find node)));
       if leqdefined node then in_peqlst 
       ((flowref2tystr (find (get_leqref node)))^" < "^(flowref2tystr (find node)))
       else ()

      )


fun printeq annast =
      case annast of
        AVAR (a, _) => printeq_node a
      | ACNST (a, _) => printeq_node a
      | ALAM (a, AVARPAR(a',_), ast'') =>
              (printeq_node a; printeq_node a'; printeq ast'')
      | AAPP (a, ast', ast'') =>
             (printeq_node a; printeq ast'; printeq ast'')
      | APAIR (a,a',a'',a''',a'''') =>
              (printeq_node a; printeq_node a'; printeq_node a'';
              printeq_node a'''; printeq_node a'''')
      | ACAR (a, a', a'', a''')    => (printeq_node a; printeq_node a';
                                       printeq_node a'';printeq_node a''')
      | ACDR (a, a', a'', a''') => (printeq_node a; printeq_node a';
                                    printeq_node a''; printeq_node a''')
      | ACOND (a, ast', ast'', ast''') =>
              (printeq_node a; printeq ast'; printeq ast'';printeq ast''')
      | ANULL a => printeq_node a
      | ADEF (a, a', _, ast') =>
             (printeq_node a; printeq_node a'; printeq ast')
      | AISNULL (a, a')  => (printeq_node a; printeq_node a')
      | APEQ (a,a',a'',a''',a'''') =>
              (printeq_node a; printeq_node a'; printeq_node a'';
              printeq_node a'''; printeq_node a'''')
      



fun show_eqlst annast = (reset_peqlst();
                         printeq annast;
                         outlst (!peqlst))



(* EQUATIONAL POST-SOLVING *)
(* 
   After constraint normalization we solve the remaining inequalities
   equationally. This is done by traversing the AST, union'ing every
   leqref with the eqrec of any node having a leqrec.
*)




fun EqSol_node node =
      if leqdefined node then 
           let val (l,r) = (find (get_leqref node), find node) in
             (add_to_wl (inEQ(l,r));
              (get_leqref node) := EMPTYFLOW)
           end
      else ()

fun EqSol annast =
      case annast of
        AVAR (a, _) => EqSol_node a
      | ACNST (a, _) => EqSol_node a
      | ALAM (a, AVARPAR(a', _), ast'') =>
              (EqSol_node a; EqSol_node a'; EqSol ast'')
      | AAPP (a, ast', ast'') =>
             (EqSol_node a; EqSol ast'; EqSol ast'')
      | APAIR (a, a', a'',a''',a'''') =>
              (EqSol_node a; EqSol_node a'; EqSol_node a'';
              EqSol_node a'''; EqSol_node a'''')
      | ACAR (a,a',a'',a''')    => (EqSol_node a; EqSol_node a'; 
                                    EqSol_node a'';EqSol_node a''')  
      | ACDR (a, a',a'',a''') => (EqSol_node a; EqSol_node a';
                                  EqSol_node a''; EqSol_node a''')
      | ACOND (a, ast', ast'', ast''') =>
              (EqSol_node a; EqSol ast'; EqSol ast'';EqSol ast''')
      | ANULL a => EqSol_node a
      | ADEF (a, a', _, ast') =>
             (EqSol_node a; EqSol_node a'; EqSol ast')
      | AISNULL (a, a')     => (EqSol_node a;EqSol_node a')
      | APEQ (a, a', a'',a''',a'''') =>
              (EqSol_node a; EqSol_node a'; EqSol_node a'';
              EqSol_node a'''; EqSol_node a'''')





fun PostSol annast =
    (EqSol annast;
     if !deb then (output(std_out,"\nWORKLIST after EqSol:\n"); 
     show_wl()) else ();
     if !deb then output(std_out,"\nWORKLISTS under Post-Solving:\n") else ();
     Solve())





fun Comp_node node = 
      if isrep node then () else (get_eqref node) := !(find node)
       




fun Comp annast =
      case annast of
        AVAR (a, _) => Comp_node a
      | ACNST (a, _) => Comp_node a
      | ALAM (a, AVARPAR(a', _), ast'') =>
              (Comp_node a; Comp_node a'; Comp ast'')
      | AAPP (a, ast', ast'') =>
             (Comp_node a; Comp ast'; Comp ast'')
      | APAIR (a, a', a'',a''',a'''') =>
              (Comp_node a; Comp_node a'; Comp_node a'';
               Comp_node a'''; Comp_node a'''')
      | ACAR (a,a',a'',a''')  => (Comp_node a; Comp_node a'; 
                                  Comp_node a''; Comp_node a''')
      | ACDR (a, a',a'',a''') => (Comp_node a; Comp_node a';
                                  Comp_node a''; Comp_node a''')
      | ACOND (a, ast', ast'', ast''') =>
              (Comp_node a; Comp ast'; Comp ast'';Comp ast''')
      | ANULL a => Comp_node a
      | ADEF (a, a', _, ast') =>
             (Comp_node a; Comp_node a'; Comp ast')
      | AISNULL (a, a')  => (Comp_node a; Comp_node a)
      | APEQ (a, a', a'',a''',a'''') =>
              (Comp_node a; Comp_node a'; Comp_node a'';
               Comp_node a'''; Comp_node a'''')      












(* ----------------- MAIN ------------------------*)



val A = ref (ANULL (ref EMPTYFLOW))

fun reset() = (reset_counter(); 
               reset_constraints();
               reset_lampars(); 
               reset_wl(); 
               reset_peqlst();
               reset_reclst())








(* Constraint normalization for abstract syntax tree *)


fun Normalize annast =
      (C annast;                       (* extract constraints *)
       worklist := (! eqs) @ (! leqs); (* initialize worklist with extracted constraints *)
       Solve()                         (* normalize *)
       )




fun Inf ast =
    (reset();
    let val annast = AnnAst ast in
      (

       A := annast;          (* current annast available in global variable A *)

       Normalize annast;     (* extract and normalize constraints *)

       PostSol annast;         (* do equational post-solving *)


       (* Comp annast; *)

                             (* translate graph to rec type *) 

(*


           (* get Print-type and clean it for empty rec abstractions *)
       let val pty = recty2Pty (get_ann annast)
           val s   = Pty2str pty
       in             
            output(std_out, "\n"^s^"\n\n"))    (* output type string *)
       end)

*)



      let val pty = recty2PtyEQ (get_ann annast) 
      in  (cleanRecty pty;
          output(std_out, "\n"^(Pty2strEQ pty)^"\n\n")) 
       end)



   end)




(*-------------------------  Test --------------------------- *)



fun eqs2strlst() =
    let val eqlist = ! eqs 
        fun strlist lst =
            case lst of
             [] => []
            | (EQ(fr1,fr2))::tl => ((flowref2tystr fr1)^" = "^(flowref2tystr fr2))::
                                   (strlist tl)
    in
        strlist eqlist
    end



fun leqs2strlst() =
    let val leqlist = ! leqs 
        fun strlist lst =
            case lst of
             [] => []
            | (LEQ(fr1,fr2))::tl => ((flowref2tystr fr1)^" < "^(flowref2tystr fr2))::
                                   (strlist tl)
    in
        strlist leqlist
    end







val id = LAM(VARPAR "x", VAR "x")

val t = CNST "true"

val f = CNST "false"

val idt = APP(id,t)

(* functions for building ast *)

fun cons x y = (APP(APP(PAIR ,x),y))

fun car x = (APP(CAR, x))

fun cdr x = (APP(CDR, x))

fun cond x y z = COND(x,y,z)

fun lambda x e = LAM(VARPAR x, e)

fun app x y = APP(x,y)

fun def x e = DEF(x,e)

fun v x = VAR x

fun isnull x = (APP(ISNULL, x))

fun peq x y = (APP(APP(PEQ, x), y))




val omega = LAM(VARPAR "x", APP(VAR "x", VAR "x"))

val Omega = APP(omega, omega)

val Yf1 = LAM(VARPAR "x", APP(VAR "f", APP(VAR "x", VAR "x")))

val Yf2 = LAM(VARPAR "y", APP(VAR "f", APP(VAR "y", VAR "y")))

val Y = LAM(VARPAR "f", APP(Yf1, Yf2))

val y = LAM(VARPAR "f", LAM(VARPAR "x", APP(VAR "f", APP(VAR "x", VAR "x"))))


val omega3 = LAM(VARPAR "x", APP(VAR "x", APP(VAR "x", VAR "x")))







val e1 = LAM(VARPAR "f", LAM(VARPAR "x", APP(VAR "f", VAR "x")))

val e2 = LAM(VARPAR "g", LAM(VARPAR "y", APP(VAR "g", VAR "y")))

val e3 = APP(e1,e2)


val def1 = DEF("s", id)


val e4 = LAM(VARPAR "x", COND(VAR "x", CNST "true", VAR "x"))

val e5 = DEF("f", e4)


val e6 = DEF("f", LAM(VARPAR "x", COND(VAR "x", VAR "x", APP(VAR "f", t))))


val e7 = COND(t,t,id)


val e8 = DEF("f", LAM(VARPAR "x", APP(VAR "f", VAR "x")))


val e9 = LAM(VARPAR "x", COND(VAR "x", VAR "x", id))


val e10 = lambda "x" (cond (v "x") (app (v "x") t) (app (v "x") t))


val e11 = lambda "x" (cond (v "x") (cons t f) (v "x")) 


val e12 = lambda "x" (cond (isnull (v "x")) t f)


val e13 = (isnull t)

val e14 = lambda "x" (cond (v "x") (isnull (v "x")) t)


val e15 = lambda "x" (isnull (v "x"))


val softmap = (def "sm" (lambda "f" (lambda "l" 
                      (cond (isnull (v "l"))
                            NULL
                            (cons (app (v "f") (car (v "l"))) 
                                  (app (app (v "sm") (v "f")) (cdr (v "l"))))))))
               


val psoftmap = (def "sm" (lambda "f" (lambda "l" 
                      (cond (peq (v "l") NULL)
                            NULL
                            (cons (app (v "f") (car (v "l"))) 
                                  (app (app (v "sm") (v "f")) (cdr (v "l"))))))))



val softmember = (def "mem" (lambda "x" (lambda "l"
                          (cond (peq (v "l") NULL) f
                                (cond (peq (v "x") (car (v "l"))) t
                                      (app (app (v "mem") (v "x")) (cdr (v "l"))))))))






val taut = (def "taut" (lambda "x"
                       (cond (peq (v "x") t) t
                             (cond (peq (v "x") f) f
                                   (cond (app (v "taut") (app (v "x") t))
                                         (app (v "taut") (app (v "x") f))
                                          f)))))
                             


val last = (def "last" (lambda "l"
                       (cond (peq (cdr (v "l")) NULL)
                             (car (v "l"))
                             (app (v "last") (cdr (v "l"))))))




val softappend1 = (def "append" (lambda "l1" (lambda "l2"
                         (cond (peq (v "l1") NULL)
                               (v "l2")
                               (cons (car (v "l1"))
                                     (app (app (v "append") (cdr (v "l1"))) (v "l2")))))))





val softappend2 = (def "append2" (lambda "l1" (lambda "l2"
                         (cond (peq (v "l1") NULL)
                               (v "l2")
                               (cond (peq (v "l2") NULL) (v "l1")
                               (cons (car (v "l1"))
                                     (app (app (v "append2") (cdr (v "l1"))) (v "l2"))))))))





val tst = (cond t (lambda "x" t) t)

val aptst = (app tst t)



val tst' = (cond t (lambda "x" NULL) (lambda "y" t)) 



val tst'' = (cond t (lambda "x" (car (v "x"))) 
                    (lambda "y" (cond (v "y") t t)))


val f = (lambda "x" (cond t (app (v "x") t) (car (v "x"))))
