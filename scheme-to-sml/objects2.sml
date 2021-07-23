(*$SchemeTypes: SchemeGeneral UnionFind *)


structure Type =
  struct
  local open SchemeGeneral UnionFind Environment
  in

  (* TYPE TAGS *)

  datatype type_tag = FUNC | BOOL | PAIR | CHAR | SYMBOL |
                      STRING | NUMBER | VECTOR | 
                      LST | UNSPEC

  val type_tags = [FUNC, BOOL, PAIR, CHAR, SYMBOL, 
        	       STRING, NUMBER, VECTOR, LST, UNSPEC]
		
  datatype shade = WHITE | GREY | BLACK

  datatype PPtype = SIM of type_tag * PPtype list |
                    DYNAMIC of PPtype list |
                    TYVAR of int  |
                    MORE

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
             name: string Option ref }

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
            name = ref None }

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
  fun unit() = listtype (new_typevar())

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
          val init = List.foldr upd undef bindings
  	  in
  	  uref (DYN (List.foldr upd_simple init l), make_attrib())
  	  end

  fun summands f = 
      List.foldr (fn (tt, l) => 
               let val potsummand = f tt
               in case utype potsummand of
      		        SIMPLE _ => potsummand :: l
      		      | TVAR _ => l
      		      | DYN _ => error ("summands", "Illegal summand (dyn type)")
      		   end) nil type_tags

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
    in show (!print_depth) t
    end
  end

  fun string_of_type_tag (FUNC)   = "func"
    | string_of_type_tag (BOOL)   = "bool"
    | string_of_type_tag (PAIR)   = "pair"
    | string_of_type_tag (CHAR)   = "char"
    | string_of_type_tag (SYMBOL) = "symbol"
    | string_of_type_tag (STRING) = "string"
    | string_of_type_tag (NUMBER) = "number"
    | string_of_type_tag (VECTOR) = "vector"
    | string_of_type_tag (LST)    = "lst"
    | string_of_type_tag (UNSPEC) = "unspec"

  val letters = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
(*  fun string_of_tyvar (i) =
      if i < 26
      then (List.nth i letters)
      else (List.nth i (letters % 26) ^ Int.toString (letters / 26))
  end *)

  fun string_of_type (SIM(tag,[]))        = string_of_type_tag tag
    | string_of_type (SIM(tag,arg::args)) = string_of_type_tag tag ^
                                            "(" ^ List.foldl (fn (t, s) => s ^ ", " ^ string_of_type t) (string_of_type arg) args ^ ")"
    | string_of_type (DYNAMIC([]))        = "any" (* ??? it sems like DYNAMIC is meant to be a union *)
    | string_of_type (DYNAMIC(opt::opts)) = List.foldl (fn (t, s) => string_of_type t ^ s) (string_of_type opt) opts
    | string_of_type (TYVAR(i))           = "'a" ^ Int.toString i
    | string_of_type (MORE)               = "..."

  fun string_of_constraint (src,tgt) = string_of_type src ^ " ~> " ^ string_of_type tgt

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
          		      val ty = new_typevar()
          		      val topconstraints =
          		      	  if b then [(nstype, ntvar), (ty, ntvar)]
          		      	       else [(ntvar, nstype), (ntvar, ty)]
          		  in (get #equivptr nstype := Some (pair(nt, ty));
          		      (ntvar, APPEND [LIST topconstraints, C]))
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