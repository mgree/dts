(*$SchemeTypes: SchemeGeneral UnionFind *)

structure Type =
  struct
  local open General UnionFind 
  in

  (* TYPE TAGS *)
  
  datatype type_tag = FUNC | BOOL | NIL | PAIR | CHAR | SYMBOL |
        STRING | NUMBER | VECTOR | UNSPEC 

  val type_tags = [FUNC, BOOL, NIL, PAIR, CHAR, SYMBOL, 
        STRING, NUMBER, VECTOR, UNSPEC]
  
  datatype shade = WHITE | GREY | BLACK

  datatype PPtype = SIM of type_tag * PPtype list |
                    DYNAMIC of  PPtype list |
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
             generic: bool ref,
             preds: atype list ref, 
             succs: atype list ref,
             pos: bool ref, 
             neg: bool ref, 
             interpreted: bool ref,
             instance: atype Option ref, 
             instantiated: bool ref,
             color: shade ref }

  withtype atype = (utype * attributes) UF
  
  fun make_attrib () = 
      ATT { equivptr = ref None, 
            sccptr = ref None,
            generic = ref false,
            preds = ref nil, 
            succs = ref nil,
            pos = ref false, 
            neg = ref false, 
            interpreted = ref false, 
            instance = ref None,
            instantiated = ref false,
            color = ref WHITE }

  (* selection functions *)
  fun utype (aty: atype) = #1 (!!aty)
  fun attributes aty = (case !!aty of (_, ATT atts) => atts)
  fun get f aty = f (attributes aty)

  (* make new type variables *)
  local 
     val counter = ref 0 
  in
  fun new_typevar () = 
      make (TVAR (!counter), make_attrib()) before counter := !counter + 1
  end

  (* make a new simple type *)  
  fun make_type stype = make (SIMPLE stype, make_attrib())

  fun bool() = make_type (BOOL, [])
  fun char() = make_type (CHAR, [])
  fun string() = make_type (STRING, [])
  fun symbol() = make_type (SYMBOL, [])
  fun number() = make_type (NUMBER, [])
  fun vector t = make_type (VECTOR, [t])
  fun pair(t1,t2) = make_type (PAIR, [t1,t2])
  fun unit() = make_type (NIL, [])
  fun unspec() = make_type (UNSPEC, [])
  fun func(t1,t2) = make_type (FUNC, [t1,t2])

  (* make a new dynamic type *)
  fun make_dyn_type l = 
      let fun make_dyn_function [] = (fn _ => new_typevar())
            | make_dyn_function (a::r) =
                    (case utype a of
                           SIMPLE (tt, _) => 
                                 (fn tt' => if tt = tt' then a else make_dyn_function r tt')
                         | _ => error ("make_dyn_function", 
                                           "Only simple types allowed in make_dyn_function"))
      in make (DYN (make_dyn_function l), make_attrib()) 
      end

  fun summands f = map f type_tags
  
  (* find equiv representative of annotated type *)
  fun ecr t =
      let val eqptr = get #equivptr t
      in case !eqptr of
           None => t
         | Some t' => let val t'' = ecr t'
                      in (eqptr := Some t''; t'')
                      end
      end

  fun elink (p, q) =
      let val p' = ecr p
          val q' = ecr q
      in if p' = q'
	     then ()
         else get #equivptr p' := Some q'
      end

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
      | DYN f             => DYNAMIC(summands ((show (level-1)) o f))
    in show (!System.Print.printDepth) t
    end

  end
end
