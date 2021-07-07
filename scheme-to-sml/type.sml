structure Type =
  struct

  local open General UnionFind
  in

  (* TYPE TAGS *)
  
  datatype type_tag = FUNC | BOOL | NIL | PAIR | CHAR | SYMBOL |
        STRING | NUMBER | VECTOR | UNSPEC | TVAR | DYNAMIC

  val type_tags = [FUNC, BOOL, NIL, PAIR, CHAR, SYMBOL, 
        STRING, NUMBER, VECTOR, UNSPEC, TVAR]

(*  
  datatype PPtype = SIM of type_tag * PPtype list |
                    DYNAMIC of  PPtype list |
                    TYVAR of int  |
                    MORE
*)
 
  (* TYPE NODES AND COERCIONS *)

  datatype tnodeC =
    TNODE of { node_id: int,
	       type_tag: type_tag,
               children: tnode list,
	       succs: coercion list ref,
	       preds: coercion list ref,
               pos: bool ref,
	       neg: bool ref,
               instance: tnode ref }
  | UNDEF
  and coercionC =
    COERCE of { node_id: int,
	        dom_type: tnode, 
	        rng_type: tnode, 
		seen: bool ref,
	        instance: coercion ref }
  | IDENT of tnode
  | TAG of type_tag
  | UNTAG of type_tag
  | ERROR
  | UNDEFC 
  | NONEXEC
  withtype coercion = coercionC uref
  and      tnode = tnodeC uref

  (* MAKE TNODES *)  

  val undef_tnode = uref UNDEF

  local 
     val tnode_counter = ref 0 
     fun new_id () = (tnode_counter := !tnode_counter + 1; !tnode_counter)
  in
  fun mk_tnode (type_tag, children) = 
      uref (TNODE { node_id = new_id (),
		    type_tag = type_tag,
		    children = children,
		    succs = ref nil,
		    preds = ref nil,
		    pos = ref false,
		    neg = ref false,
		    instance = ref undef_tnode })
  end

  fun init_tnode tn =
      case !!tn of
	TNODE { succs = ss, preds = ps, 
	        pos = p, neg = n, instance = i, ... } => 
		(r := nil; ss := nil; ps := nil; 
	         p := false; n := false; i := undef_tnode)
      | UNDEF => ()

  val dynamic = mk_tnode (DYNAMIC, [])
  fun tvar() = mk_tnode (TVAR, [])
  fun bool = mk_tnode (BOOL, [])
  fun char() = mk_tnode (CHAR, [])
  fun string() = mk_tnode (STRING, [])
  fun symbol() = mk_tnode (SYMBOL, [])
  fun number() = mk_tnode (NUMBER, [])
  fun vector t = mk_tnode (VECTOR, [t])
  fun pair(t1,t2) = mk_tnode (PAIR, [t1,t2])
  fun unit() = mk_tnode (NIL, [])
  fun unspec() = mk_tnode (UNSPEC, [])
  fun func(t1,t2) = mk_tnode (FUNC, [t1,t2])

  (* OBSERVE TNODES *)

  exception NonTnode

  fun get f tnode = 
	case !!tnode of
	  TNODE r => f r
        | _ => raise NonTnode           

  val node_id = get #node_id
  val type_tag = get #type_tag
  val children = get #children
  val succs = get #succs
  val preds = get #preds
  val pos = get #pos
  val neg = get #neg
  val instance = get #instance


  (* DISPLAY TNODES *)
 
(*     	 	          
  fun show_type t =
    let fun show level t =
      if level = 0 then MORE else
      case utype t of
        SIMPLE(tt, tlist) => SIM(tt, map (show (level-1)) tlist)
      | TVAR i            => TYVAR i 
      | DYN f             => DYNAMIC(summands ((show (level-1)) o f))
    in show (!System.Print.printDepth) t
    end

*)

  (* MAKE COERCIONS *)

  val undef_coercion = uref UNDEFC
  val idc = uref o IDENT
  val tag = uref o TAG
  val untag = uref o UNTAG
  val error_coercion = uref ERROR
  val nonexec_coercion = uref NONEXEC

  local val coercion_counter = ref 0
        fun new_cid () = (coercion_counter := !coercion_counter + 1; !coercion_counter)
  in 
  fun mk_coercion (t1,t2) =
      if equal(t1,t2) 
	 then idc t1
      else uref (COERCE { node_id = new_cid(),
			  dom_type = t1,
			  rng_type = t2,
		          seen = ref bool
			  instance = ref undef_coercion })
  end

  fun init_coercion c =
      case !!c of
	COERCE { instance = i, seen = s, ... } => 
	  (i := undef_coercion; s := false)
      | _ => ()

  (* OBSERVE COERCIONS *)

  exception NonCoercion

  fun getC f c =
      case !!c of
        COERCE r => f r
      | _ => raise NonCoercion

  val dom_type = getC #dom_type
  val rng_type = getC #rng_type
  val cinstance = getC #instance
  val seen = getC #seen


  (* TYPE GRAPHS: G = (V, E, r), V = vertices, E = coercions, r = root;
                  Invariants: r in V, (v1, v2) in E => v1, v2 in V *)

  type tgraph = tnode list * coercion list 

  datatype tscheme = 
    SIMPLE of tnode 
  | POLY of tgraph * tnode

  fun instantiate (SIMPLE tn) = (nil, nil, tn)
    | instantiate (POLY ((V,E),r)) =
     let 
     val _ = app init_tnode V
     val V' = map (fn tn => 
		      if type_tag tn = TVAR
			 then let val tn' = tvar() 
               		      in (instance tn := tn'; tn')
		              end
	              else error ("instantiate", "Argument not a type variable")) V
     fun inst tn = 
	let val type_tag_tn = type_tag tn
            val children_tn = children tn
        in if type_tag_tn = TVAR
              then let val tn' = !(instance tn)
                   in case !!tn' of 
			UNDEF => tn
                      | TNODE _ => tn'
                   end
           else mk_tnode (type_tag_tn, map inst children_tn)
        end
		(* This definition of inst does _not_ work for cyclic types! *)
     val E' = map (fn c => mk_coercion (inst (dom_type c), inst (rng_type c))) E
     val r' = inst r
     in
     (V', E', r')
     end

  end
  end