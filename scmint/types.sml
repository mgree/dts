(*$SCHEMETYPES *)

(*
signature SCHEMETYPES =
sig



  type 'a anncommand_or_definition

  type Schemetype 

  type constraint 




  val counter : unit -> int
  
  val newvar : unit -> Schemetype

  val map_annfcn : ('a -> 'a)
                     -> 'a anncommand_or_definition
                        -> 'a anncommand_or_definition
  




end

*)



(*$SchemeTypes: SCHEMETYPES SchemeAnnast *)
structure SchemeTypes (*: SCHEMETYPES *) =
struct
  local open SchemeAst SchemeAnnast in


  type 'a anncommand_or_definition = 'a anncommand_or_definition

  type variable = variable


  
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





  
val deb = ref false;

fun debug s =
    if !deb then output(std_out, s^"\n") else ()






(* Selectors *)


exception GetError;
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


(* Placeholder variables in fresh sums are negative numbers *)
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
    (case !(get_tyref sum) of
      SUM (boolref, nilref, arrowref, consref) =>

        (case ! (get_tyref fvref) of
          BOOLEAN => boolref := ! fvref
        | NIL => nilref := ! fvref
        | ARROW (_,_) => arrowref := ! fvref
        | CONS (_,_) => consref := ! fvref
        | _ => raise InsertSumError)

    | _ => raise InsertSumError;

      sum)


exception FreshvarsSumError;
fun freshvars_sum sum =
    case !(get_tyref sum) of
      SUM(boolref,nilref,arrowref,consref) =>
       (if (! boolref) = EMPTYFLOW then boolref := ! (mkfv_newsumtyvar()) else ();
        if (! nilref)  = EMPTYFLOW then nilref := ! (mkfv_newsumtyvar()) else (); 
        if (! arrowref) = EMPTYFLOW then arrowref := ! (mkfv_newsumtyvar()) else ();
        if (! consref) = EMPTYFLOW then consref := ! (mkfv_newsumtyvar()) else ();
        sum)
    | _ => raise FreshvarsSumError




fun mkfv_sum_of_two fvref1 fvref2 = 
    let val nsum = mkfv_newsum() in
        (freshvars_sum (insert_sum fvref2 (insert_sum fvref1 nsum)))
    end
    


fun mkfv_universal_sum() = freshvars_sum (mkfv_newsum())









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
      











(* UNION - FIND FUNCTIONS *)



fun isrep flowref = 
(debug "isrep";
(!(get_eqref flowref)) = !flowref)


fun find flowref = 
      if (isrep flowref) then (get_eqref flowref) else find (get_eqref flowref)



fun istyvar tyref =
      case !tyref of
        TV _ => true
      | _    => false

fun isvariable flowref = istyvar (get_tyref flowref)



fun leqdefined flowref = (!(get_leqref flowref)) <> EMPTYFLOW



(* make fr1 new equivalence class representative of [fr1 U fr2] *)
fun newrep fr1 fr2 = 
      ((get_eqref fr2) := !fr1 ; fr1)
      
     
fun newvarrep fr1 fr2 =
      if (leqdefined fr1) then newrep fr1 fr2 else newrep fr2 fr1



fun union flowref1 flowref2 =
      if (!(find flowref1)) = (!(find flowref2)) then find flowref1 else
      let val fr1 = find flowref1
          val fr2 = find flowref2
      in
          case (isvariable fr1, isvariable fr2) of
            (true,true)   => newvarrep fr1 fr2
          | (false, true) => newrep fr1 fr2
          | (true,false)  => newrep fr2 fr1
          | (false,false) => newrep fr1 fr2
      end

















  





(* CONSTRAINT EXTRACTION *)

(* 
   Global variables : 

   eqs  :  ref list of equational constraints
   leqs :  ref list of inequational constraints
   lampars : ref assoc-list of bound variable occurrences and their annotations

*)

val eqs = (ref []): (Constraint list) ref

val leqs = (ref []): (Constraint list) ref

fun reset_constraints () = (eqs := ([]: (Constraint list));
                            leqs := ([]: (Constraint list))) 


(* 
   List of references to flowval's occurring at lambda parameter binding points.
   All free and bound variables are assumed to be distinct. 
*)
val lampars = (ref []) : ((variable * Flowref) list) ref 

fun reset_lampars() = lampars := []


(* Record that we have met binding occurrence of var, with flowref = [var] *)
fun in_lampars var flowref = lampars := (var, flowref) :: (! lampars)

fun outlst strlst = map (fn s => output(std_out, s)) strlst






(* ----- Begin Printing constraints ---------- *)





fun fr2tystr fr = 
      if (! fr) = EMPTYFLOW then raise Flowref2tystrError
      else tr2tystr (get_tyref fr)

and tr2tystr tr = 
      case (! tr) of
        EMPTYTYPE           => "emptytype"
      | TV tyvar            => "a"^(makestring tyvar)
      | BOOLEAN             => "bool"
      | NIL                 => "nil"
      | ARROW(d,r)          => "("^(fr2tystr (find d))^"->"^
                                (fr2tystr (find r))^")"
      | CONS(f,s)           => "(cons "^(fr2tystr (find f))^" "^(fr2tystr (find s))^")"
      | SUM(s1, s2, s3, s4) => "("^(fr2tystr (find s1))^"+"^
                                   (fr2tystr (find s2))^"+"^
                                   (fr2tystr (find s3))^"+"^
                                   (fr2tystr (find s4))^")"
      




fun print_con c =
      case c of
        EQ(l,r) => (fr2tystr (find l))^" = "^(fr2tystr (find r))^"\n"
      | LEQ(l,r) => (fr2tystr (find l))^" < "^(fr2tystr (find r))^"\n"





fun show_con() = 
      (output(std_out, "Equality constraints :\n");
       outlst (map print_con (!eqs));
       output(std_out, "Inequality constraints :\n");
       outlst (map print_con (!leqs)))






(* -------- End printing Constraints ---------------- *)






fun in_eqs c = eqs := (c :: (! eqs))

fun in_leqs c = leqs := (c :: (! leqs))


fun mkLEQ(fr1,fr2) = in_leqs (LEQ(fr1,fr2))

fun mkEQ(fr1,fr2) = in_eqs(EQ(fr1,fr2))



exception GetFlowrefError;

(* lookup the flowref associated with var in lampars *)
fun lookup_var var = 
    let fun lookup v l = 
            case l of
              [] =>  raise GetFlowrefError
            | (v', r)::tl => if v' = v then r else lookup v tl
    in
        lookup var (! lampars)
    end      












(* ------------------------------------------------------------------------ *)



(*  
  fun map_annfcn afcn anncod =
      let val F =
 {aliteral_fcn                 = fn x => ALITERAL x,
  ann_fcn                      = afcn,
  avariable_fcn                = fn x => AVARIABLE x,
  acall_fcn                    = fn x => ACALL x,
  alambda_fcn                  = fn x => ALAMBDA x,
  aif_fcn                      = fn x => AIF x,
  aassign_fcn                  = fn x => AASSIGN x,
  acond_fcn                    = fn x => ACOND x,
  acase_fcn                    = fn x => ACASE x,
  aand_fcn                     = fn x => AAND x,
  aor_fcn                      = fn x => AOR x,
  alet_fcn                     = fn x => ALET x,
  anamedlet_fcn                = fn x => ANAMEDLET x,
  alets_fcn                    = fn x => ALETS x,
  aletrec_fcn                  = fn x => ALETREC x,
  abegin_fcn                   = fn x => ABEGIN x,
  ado_fcn                      = fn x => ADO x,
  adelay_fcn                   = fn x => ADELAY x,
  aquasiquote_fcn              = fn x => AQUASIQUOTE x,
  acondclause_fcn              = fn x => ACONDCLAUSE x,
  anullcond_fcn                = fn x => ANULLCOND x,
  aconddefault_fcn             = fn x => ACONDDEFAULT x,
  atestseq_fcn                 = fn x => ATESTSEQ x,
  atest_fcn                    = fn x => ATEST x,
  atestrec_fcn                 = fn x => ATESTREC x,
  acaseclause_fcn              = fn x => ACASECLAUSE x,
  anullcase_fcn                = fn x => ANULLCASE x,
  acasedefault_fcn             = fn x => ACASEDEFAULT x,
  avardef_fcn                  = fn x => AVARDEF x,
  afundef_fcn                  = fn x => AFUNDEF x,
  abegindef_fcn                = fn x => ABEGINDEF x,
  asimpletemp_fcn              = fn x => ASIMPLETEMP x,
  alisttemp_fcn                = fn x => ALISTTEMP x,
  ailisttemp_fcn               = fn x => AILISTTEMP x,
  avectemp_fcn                 = fn x => AVECTEMP x,
  aunquote_fcn                 = fn x => AUNQUOTE x,
  atemplate_fcn                = fn x => ATEMPLATE x,
  aunqspl_fcn                  = fn x => AUNQSPL x,
  avarpar_fcn                  = fn x => AVARPAR x,
  apairpar_fcn                 = fn x => APAIRPAR x,
  anullpar_fcn                 = fn x => ANULLPAR x,
  acommand_fcn                 = fn x => ACOMMAND x,
  adefinition_fcn              = fn x => ADEFINITION x,
  adatum_fcn                   = fn x => x}

  in
     mapacommand_or_definition F anncod
  end




  fun new_constraint() = {low = ref (newvar()), high = ref (newvar())}


  fun get_annexp() = let val f = dat2annexp new_constraint in
                         f (read_datum std_in)
                     end



  

*)

end
end












































