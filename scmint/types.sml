(*$SCHEMETYPES *)

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


(*$SchemeTypes: SCHEMETYPES SchemeAnnast *)
structure SchemeTypes (*: SCHEMETYPES *) =
struct
  local open SchemeAst SchemeAnnast in


  type 'a anncommand_or_definition = 'a anncommand_or_definition

  datatype Schemetype =
      TYVAR of int |
      NIL |
      BOOL |
      NUMBER |     
      STRING |
      SYMBOL |
      CHAR |
      VECTOR of Schemetype |
      CONS of Schemetype * Schemetype |
      PROCEDURE of Schemetype * Schemetype |
      UNION of Schemetype * Schemetype

  

  type constraint = {low : Schemetype ref, high : Schemetype ref}

  
  local val c = ref 0
  in
     fun counter() =
         (c := !c + 1;
          !c)
  end 

  fun newvar() = TYVAR (counter())  

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
*)



  fun new_constraint() = {low = ref (newvar()), high = ref (newvar())}


  fun get_annexp() = let val f = dat2annexp new_constraint in
                         f (read_datum std_in)
                     end



  



end
end












































