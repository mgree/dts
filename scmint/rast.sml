
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
 
  local open SchemeGeneral SchemeAnnast SchemeAst in


  exception Undef


(* ----------------------------------------------------------------------------------- *)


  type ident = string

  datatype ctype = PLUS | MINUS | TIMES | EQUAL | GT | LT | OR | AND | NOT |
                   INT  of int | BOOL  of bool


  datatype 'a exp =
    VAR of ident |
    CONST of ctype |
    LAMBDA of ident * 'a texp |
    APP of 'a texp * 'a texp |
    IF of 'a texp * 'a texp * 'a texp |
    PRIMAPP of ctype * 'a texp list 
  withtype 'a texp = 'a * 'a exp
  
  datatype 'a def_or_exp = 
    EXP of 'a texp |
    DEF of ident * 'a texp

  

(*  type ('a,'b,'c) program = (ident * ('a,'b,'c) texp) list
*)








(* ------------------------------------------------------------------------------------ *)




  fun t_exp e =

      let fun prim p =
              case p of
                   AVARIABLE(a, "+") => PLUS |
                   AVARIABLE(a, "-") => MINUS |
                   AVARIABLE(a, "*") => TIMES |
                   AVARIABLE(a, "=") => EQUAL |
                   AVARIABLE(a, ">") => GT |
                   AVARIABLE(a, "<") => LT |
                   AVARIABLE(a, "not") => NOT 
                   
          fun isprimop e =
              case e of
                   AVARIABLE(a, "+") => true |
                   AVARIABLE(a, "-") => true |
                   AVARIABLE(a, "*") => true |
                   AVARIABLE(a, "=") => true |
                   AVARIABLE(a, ">") => true |
                   AVARIABLE(a, "<") => true |
                   AVARIABLE(a, "not") => true |
                   AOR _ => true |
                   AAND _ => true |
                   _ => false
      in           
    
      case e of 

           ALITERAL (a, ad) => (a, CONST (t_const ad)) |
           AVARIABLE (a, v) => if (isprimop e) then (a, CONST (prim e)) else (a, VAR v) |
           AOR(a, ael) => (a, PRIMAPP(OR, map t_exp ael)) |
           AAND(a, ael) => (a, PRIMAPP(AND, map t_exp ael)) |
           ACALL (a, (ae, ael)) => if (isprimop ae) then (a, PRIMAPP(prim ae, map t_exp ael))
                                   else (case ael of
                                        [ae] => (a, APP(t_exp ae, t_exp ae)) |
                                         _   => raise Undef) |        
           ALAMBDA (a, (par, 
                        ([],([],ae)))) => (a, LAMBDA(t_par par, t_exp ae)) |
           AIF (a, (ae,ae',Some ae'')) => (a, IF(t_exp ae, t_exp ae', t_exp ae'')) |
           _ => raise Undef
      end

  and t_par par =
      case par of
           APAIRPAR(a, (v, ANULLPAR _)) => v |
           _ => raise Undef        
      
  and t_const ad =
      case ad of
           ABOOLDAT b => BOOL b |
           ANUMBDAT n => INT n  |
           _ => raise Undef

           


  and t_def_or_exp doe =
      case doe of
           ADEFINITION(a, AVARDEF (a', (v, ae))) => DEF(v,t_exp ae) |
           ACOMMAND(a,ae) => EXP(t_exp ae) |
      _ => raise Undef
           


(*


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
      

*)


(* ------------------------------------------------------------------------------------------- *)






 

end
end


