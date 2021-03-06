(*$SchemeTypes: SchemeGeneral UnionFind *)


structure Type =
  struct
  local open SchemeGeneral UnionFind Environment
  in

  (* TYPE TAGS *)

  datatype type_tag = FUNC | BOOL | PAIR | CHAR | SYMBOL |
                      STRING | NUMBER | VECTOR | NIL |
                      LST | UNSPEC

  val type_tags = [FUNC, BOOL, PAIR, CHAR, SYMBOL, NIL,
        	       STRING, NUMBER, VECTOR, LST, UNSPEC]
		
  datatype shade = WHITE | GREY | BLACK

  datatype PPtype = SIM of type_tag * PPtype list |
                    DYNAMIC of PPtype list |
                    TYVAR of int  |
                    RECNAME of string |
                    LETREC of (string * PPtype)list * PPtype |
                    MORE |
                    PPTSCHEME of (PPtype * PPtype)list * PPtype |
                    PPTYPE of PPtype


  (* DYNAMIC TYPES *)

  datatype utype =
    SIMPLE of type_tag * atype list   
  | DYN of type_tag -> atype
  | TVAR of int

  and attributes = 
    ATT of { equivptr: atype Option ref,
             sccptr: atype Option ref, 
	         sources: (type_tag, atype) env ref,
     	     sinks: (type_tag, atype) env ref,
             generic: bool ref,
             preds: atype list ref, 
     	     numpreds: int ref,
             succs: atype list ref,
     	     numsuccs: int ref,
             pos: bool ref, 
             neg: bool ref, 
             interpreted: bool ref,
             collected: bool ref,
             instance: atype Option ref, 
             color: shade ref,
             name: string Option ref,
             isnamed : bool ref }

  withtype atype = (utype * attributes) uref

  fun make_attrib () = 
      ATT { equivptr = ref None, 
            sccptr = ref None,
     	    sources = ref empty_env,
     	    sinks = ref empty_env,
            generic = ref false,
            preds = ref nil, 
    	    numpreds = ref 0,
            succs = ref nil,
            numsuccs = ref 0,
            pos = ref false, 
            neg = ref false, 
            interpreted = ref false, 
            collected = ref false,
            instance = ref None,
            color = ref WHITE,
            name = ref None,
            isnamed = ref false }

  (* selection functions *)

  fun utype (aty: atype) = #1 (!!aty)
  fun attributes aty = (case !!aty of (_, ATT atts) => atts)
  fun get f aty = f (attributes aty)

  (* make new type variables *)

  local 
     val counter = ref 0 
  in
  fun new_typevar () = 
      uref (TVAR (!counter), make_attrib()) before counter := !counter + 1
  end

  (* make a new simple type *)  
  
  infix ::=

  fun make_type stype = uref (SIMPLE stype, make_attrib())

  fun bool() = make_type (BOOL, [])
  fun char() = make_type (CHAR, [])
  fun string() = make_type (STRING, [])
  fun symbol() = make_type (SYMBOL, [])
  fun number() = make_type (NUMBER, [])
  fun vector t = make_type (VECTOR, [t])
  fun pair(t1,t2) = make_type (PAIR, [t1,t2])
  fun unspec() = make_type (UNSPEC, [])
  fun func(t1,t2) = make_type (FUNC, [t1,t2])
  fun listtype t = make_type (LST, [t])
  fun (* unit() = listtype (new_typevar()) *)
      unit() = make_type (NIL, []) 

  (* make a new dynamic type *)

  fun make_dyn_type l =
      let val bindings = map (fn tt => (tt, new_typevar())) type_tags
          fun undef tt = error ("make_dyn_type", "Undefined type_tag -- impossible")
          fun upd ((tt, at), f) x = if x = tt then at else f x
          fun upd_simple (at, f) =
          	  case utype at of
  	  			SIMPLE (tt, _) => upd ((tt, at), f)
  	  		  | TVAR _ => error ("upd", "Only simple types allowed (tvar)")
  	  		  | DYN _ => error ("upd", "Only simple types allowed (dyntype)") 
          val init = fold upd bindings undef
  	  in
  	  uref (DYN (fold upd_simple l init), make_attrib())
  	  end

  fun summands f = 
      fold (fn (tt, l) => 
               let val potsummand = f tt
               in case utype potsummand of
      		        SIMPLE _ => potsummand :: l
      		      | TVAR _ => l
      		      | DYN _ => error ("summands", "Illegal summand (dyn type)")
      		   end) type_tags nil 

  (* find equiv representative of annotated type *)

 fun ecr t = 
      let val equivptr = get #equivptr t
      in case !equivptr of
           None => t
         | Some t' => let val t'' = ecr t'
                      in (equivptr := Some t''; t'')
                      end
      end
      
 (* Precondition for elink: t1, t2 must both be equiv-representatives *)
 fun elink(t1,t2) = get #equivptr t1 := Some t2

 fun sccrep t =
      let val sccptr = get #sccptr t
      in case !sccptr of
      	   None => t
      	 | Some t' => let val t'' = sccrep t'
      	 	          in (sccptr := Some t''; t'')
      	 	          end
      end
    	 	          
  fun show_type t =
    let fun show level t =
      if level = 0 then MORE else
      case utype t of
        SIMPLE(tt, tlist) => SIM(tt, map (show (level-1)) tlist)
      | TVAR i            => TYVAR i 
      | DYN f             => DYNAMIC (map (show (level-1)) (summands f))
    in show (!SML_NJ.Print.printDepth) t
    end











(*-------------- RECURSIVE TYPES ---------------------*)




local open SchemeGeneral

  val Env = ref ([] : (string * PPtype)list)
  fun resetenv() = Env := ([] : (string * PPtype)list)
in

  exception Getname;
  fun getname name =
      case ! name of
        None => raise Getname
      | Some N => N

  fun unfold t =
      let val ut = utype t 
          val name = get #name t
          val isnamed = get #isnamed t
      in
          if !isnamed then RECNAME (getname name) 
          else
            (isnamed := true; 
             (case ut of
               SIMPLE(tt, tlist) => SIM(tt, map unfold tlist)
             | TVAR i => TYVAR i
             | DYN f => DYNAMIC (map unfold (summands f))) before
             isnamed := false (* reset the isnamed field *))
      end
               
                          

  fun named_type t =
      let val ut = utype t 
          val name = get #name t
          fun isin_env(s,E) =
              case E of
                [] => false
              | (s',v)::E' => if s = s' then true else isin_env(s, E')

          fun extenv(s,v) = 
              let val E = !Env
              in
                  if isin_env(s,E) then () else Env := (s,v)::E
              end
      in
          case !name of
                      
            Some N => (extenv(N, unfold t);(* update recursion environment *)  
                       RECNAME N) 
          | None => (case ut of
                       SIMPLE(tt, tlist) => SIM(tt, map named_type tlist)
                     | TVAR i => TYVAR i
                     | DYN f => DYNAMIC (map named_type (summands f)))
      end

  fun ty2PPty t = 
      (resetenv(); 
       let val nty = named_type t
       in
           LETREC(!Env, nty)
       end)




(* --------- GENERATION OF DATATYPES ----------------*)



(* Probably out:

    fun freevars1 ppt = 
        case ppt of
          SIM(tt, lst) => fold (fn (x, y) => x @ y) lst []
        | DYNAMIC lst => fold (fn (x, y) => x @ y) lst []
        | TYVAR i => [TYVAR i]
        | _ => []

   fun lst2set l =
       case l of
         [] => []
       | x::xs => if List.member x xs then lst2set xs
                  else  x::(lst2set xs)

   fun freevars ppt = let val fv = freevars1 ppt
                      in 
                          lst2set fv
                      end
*)


  

  local val c = ref 0
  in
        fun ntv() = TYVAR (!c) before c := !c + 1 
  end
 
  fun freshsubstdyn() = [ntv(), ntv(), ntv(), ntv()]


  fun insert(l, substdyn as [t1, t2, t3, t4]) =
      case l of
        [] => substdyn
      | (x as (SIM(PAIR, [fst, snd])))::xs =>
               insert(xs, [fst, snd, t3, t4])
      | (x as (SIM(FUNC, [dom, ran])))::xs =>
               insert(xs, [t1, t2, dom, ran]) 
      | x::xs => insert(xs, substdyn)
       
        
  fun dyn2substdyn l =
      insert(l, freshsubstdyn())

 
  exception PPerror;
  
  fun PP2str ppt = 
      let fun paren x = "("^x^")"          
          fun sqparen x = "["^x^"]"
          fun strlist2sum l =
              case l of
                [] => ""
              | [x] => x
              | x::xs => x^"+"^(strlist2sum xs)
      in
        case ppt of
          SIM(FUNC, [dom, ran]) => paren((PP2str dom)^"->"^(PP2str ran))
        | SIM(PAIR, [fst, snd]) => paren((PP2str fst)^"*"^(PP2str snd))
        | SIM(BOOL, []) => "bool"
        | SIM(CHAR, []) => "char"
        | SIM(SYMBOL, []) => "symbol"
        | SIM(STRING, []) => "string"
        | SIM(NUMBER, []) => "number"
        | SIM(NIL, []) => "nil"
        | SIM(LST, _) => (output(std_out, "PPerror: LST\n"); raise PPerror)
        | SIM(UNSPEC, _) => 
                      (output(std_out, "PPerror: UNSPEC\n"); raise PPerror)
        | TYVAR i => "'a"^(makestring i)
        | DYNAMIC l => (* PPdyn2str l *)
                       paren(strlist2sum (map PP2str  l)) 
        | RECNAME name => name
        | MORE => "#"
        | _ => raise PPerror 
      end
                  
  and PPdyn2str l =
      let val [t1, t2, t3, t4] = dyn2substdyn l
          val t1str = PP2str t1
          val t2str = PP2str t2
          val t3str = PP2str t3
          val t4str = PP2str t4
      in
          "("^t1str^","^t2str^","^t3str^","^t4str^")"^"dyn"  
      end

  
   

  fun env2str E =
      case E of
        [] => ""
      | [(s,v)] => s^" = "^(PP2str v) 
      | (s,v)::E' => s^" = "^(PP2str v)^"\n"^(env2str E')








(*  
   fun recbind2dataty (recvar, recty) =
       let val fv = freevars recty
       
fun env2MLstr E = 
*)


  fun subst(substty, name) ppty =
      case ppty of
        RECNAME name' => if name = name' then substty else RECNAME name    
      | SIM(tt, l) => SIM(tt, map (subst(substty, name)) l)
      | DYNAMIC l => DYNAMIC (map (subst(substty, name)) l)
      | _ => ppty

 

  fun unfoldPPty(recname, ppty) =
      subst(ppty, recname) ppty


  fun unfoldenv E =
      case E of
        [] => []
      | (name, ppty)::E' => (name, (unfoldPPty(name, ppty)))::(unfoldenv E')

  fun ty2str t = 
      let val LETREC(E, ppt) = ty2PPty t
          (* val E' = unfoldenv E *)
      in
           let val ppstr = PP2str ppt
               val envstr =  env2str E
           in
               (if envstr <> "" then "\nletrec\n"^envstr^"\nin\n"
               else "")^ppstr
           end
      end
       
end (* local val Env ... *)


           

  end

end;

(*$SchemeCoercions: KernelTypes *)

structure Coercion =
  struct
  local open Type UnionFind
  in

  type coercion_sig = atype * atype
  fun lo_type (l, h) = l
  fun hi_type (l, h) = h

  fun new_coercion_sig() = (new_typevar(), new_typevar())

  datatype coercion =
     CVAR of int * atype * atype
   | IDC 
   | ERR
   | TAG of type_tag
   | CHECK of type_tag
   | MAP of type_tag * coercion list
   | COMP of coercion * coercion

  local val coercion_var_counter = ref 0
  in
  fun new_coercionvar(t1, t2) =
      (coercion_var_counter := !coercion_var_counter + 1;
       CVAR (!coercion_var_counter, t1, t2))
  end

  fun canonical_coercion (t1, t2) =
      (case (utype t1, utype t2) of
        (SIMPLE (tt, _), DYN _) => TAG tt |
        (DYN _, SIMPLE (tt, _)) => CHECK tt |
        (SIMPLE _, SIMPLE _) => IDC |
        (DYN _, DYN _) => IDC |
        (TVAR _, _) => 
           if equal(t1, t2) then IDC
           else new_coercionvar(t1, t2) |
        (_, TVAR _) => 
           if equal(t1, t2) then IDC
           else new_coercionvar(t1, t2))
  end

end;

structure Constraint =
  struct
  local  
  open SchemeGeneral Type UnionFind
  in

  fun decompose b t =
        (* decomposes a type into a type variable and a list of flow constraints *)
        case utype t of
            TVAR _ => (t, anil)
          | SIMPLE (FUNC, [td, tr]) =>
                  let val (ntd, Cd) = decompose (not b) td 
                          val (ntr, Cr) = decompose b tr
                          val nstype = func (ntd, ntr)
                          val ntvar = new_typevar()
                          val topconstraint =
                                if b then (nstype, ntvar) else (ntvar, nstype)
                  in (ntvar, APPEND [leaf topconstraint, Cd, Cr])
                  end
          | SIMPLE (LST, [at]) =>
		  let val (nt, C) = decompose b at
       		      val nstype = listtype nt
       		      val ntvar = new_typevar()
		      val eqrtype = pair (nt,ntvar)
       		      val topconstraint =
       		      	  if b then (nstype, ntvar)
       		      	       else (ntvar, nstype)
       		  in (get #equivptr nstype := Some eqrtype;
		     (ntvar, APPEND [leaf topconstraint, C]))
       		  end 
          | SIMPLE (ttag, atl) =>
                  let val tCs = map (decompose b) atl
                      val nstype = make_type (ttag, map #1 tCs)
                      val ntvar = new_typevar()
                      val topconstraint =
                        if b then (nstype, ntvar) else (ntvar, nstype)
                  in (ntvar, APPEND (leaf topconstraint :: map #2 tCs))
                  end
          | DYN f => 
                  let val atl = summands f
                      val (ts, Cs) = unzip (map (decompose b) atl)
                      val ntvar = new_typevar()
                  in (apply (fn t => link(t, ntvar)) ts;
                      (ntvar, APPEND Cs))
                  end
        
  fun constraints (t1, t2) =
        (* generates flow constraints/primitive coercions from arbitrary 
        coercion signature *)
      case (utype t1, utype t2) of
        (TVAR _, TVAR _) => leaf (t1, t2)
      | _ =>  
        let val (tv1, C1) = decompose true t1
            val (tv2, C2) = decompose false t2
        in (union (tv1, tv2); APPEND [C1, C2])
        end

  fun show_constraint (t1, t2) =
          (show_type t1, show_type t2)
           
  fun show_constraints C =
      map show_constraint (aflatten [C])
          
  end
end;

(*$SchemePolyTypes: SchemeCoercions KernelTypes *)

structure PolyType =
  struct
  local open Type Constraint Coercion SchemeGeneral UnionFind
  in

  datatype typescheme =
  	TSCHEME of coercion_sig list * atype
  |	TYPE of atype

  fun close (C, t) = 
  	  let fun make_generic at =
  	  	        let val genatt = get #generic at
  	  	        in if !genatt 
  	  	              then ()
  	  	           else (genatt := true;
  	  	           	     case utype at of
  	  	           	       TVAR _ => () |
  	  	           	       SIMPLE (_, tlist) => apply make_generic tlist |
  	  	           	       DYN f => apply make_generic (summands f))
  	  	        end
  	  	  fun make_nongeneric at =
  	  	        (* This is dead code right now since all generalizations
  	  	           occur at top-level and thus there are no nongeneric
  	  	           (lambda-bound) type variables whose genericization
  	  	           must be prevented *)
  	  	  	    let val genatt = get #generic at
  	  	  	    in if !genatt
  	  	  	          then (genatt := false;
  	  	  	          	    case utype at of
  	  	  	          	      TVAR _ => () |
  	  	  	          	      SIMPLE (_, tlist) => apply make_nongeneric tlist |
  	  	  	          	      DYN f => apply make_nongeneric (summands f))
  	  	  	          else ()
  	  	  	    end
  	   in (apply (fn (t1, t2) => (make_generic t1; make_generic t2)) C;
  	   	   make_generic t;
  	   	   TSCHEME (C, t))
  	   end

 fun instantiate (TSCHEME (C, t)) =
     let
     val insttypes = ref []
     fun inst at =
       if !(get #generic at) 
       then let val instance = get #instance at
       	    in case !instance of
          	     None => let val nt = new_typevar()
                 		 in instance := Some nt;
                     	    insttypes := at :: !insttypes;
                     	    (case utype at of
                       	        TVAR _ => ()
					  		  | SIMPLE (tt, tlist) => 
					  			  link (nt, make_type (tt, map inst tlist))
                      	      | DYN f => 
                         		  link (nt, make_dyn_type (map inst (summands f))));
                     	    nt
	                     end
	           | Some it => it
	        end
	   else at
     val C' = map (fn (t1, t2) => (inst t1, inst t2)) C
     val t' = inst t 
     in apply (fn at => get #instance at := None) (!insttypes);
        (C', t')
     end
   | instantiate (TYPE aty) = ([], aty)

  fun show_polytype (TSCHEME (C,t)) = 
  	  (map show_constraint C, show_type t) 
  	| show_polytype (TYPE t) = ([], show_type t)
  end

end;
