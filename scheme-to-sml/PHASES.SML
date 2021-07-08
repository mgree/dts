  (* 1st stage: parse input stream into an exp annotated with new 
                coercion signatures *)

  local open Exp Coercion
  fun nothing _ = ()
  val new_atts = INIT { parameter = nothing,
                       variable = nothing,
                       exp = new_coercion_sig }
                                                 
  in
  val (parse_exp, parse_def, parse_program) = read new_atts;
  end;




  (* 2nd stage: resolve variable occurrences by annotating them
                with [a generic instance of] the type of the corresponding
	            binding occurrence *)

  exception StaticAnalysisError of string
  
  local   
  open SchemeGeneral Exp Environment Type PolyType
  
  fun noexp a Env = NOEXP a
  fun literal d Env = LITERAL d
  fun variable (a, (s: string, ())) Env = 
  	  let val (C, t) = instantiate (lookup s Env)
	      val ta = case C of
		   		 nil => TYPE t
		 	       | _ => TSCHEME (C, t)
	  in VARIABLE (a, (s, ta))
	  end
  fun call (a, e, l) Env = CALL (a, e Env, l Env)
  fun lambda (a, (st, fenv), e) Env = 
        LAMBDA (a, st, e (add (Env, fenv)))
  fun ifexp (a, e , e', e'') Env = 
        IF (a, e Env, e' Env, e'' Env)
  fun assign (a,(s:string, ()), e) Env = 
  	  case lookup s Env of
  	  	t as (TYPE _) => ASSIGN (a, (s, t), e Env)
  	  | TSCHEME _ => error ("make_attributes", 
                  	"Polymorphic values cannot be assigned to")

  fun pairarg (a,e,l) Env = 
        PAIRARG (a, e Env, l Env)
  fun nullarg a Env = NULLARG a
  fun avarpar (s, ()) = 
      let val nt = new_typevar()
      in
      (AVARPAR (s, nt), make_env (s, TYPE nt))
      end
  fun apairpar ((s:string, ()), (st, (env : (string, typescheme) env))) = 
      let val nt = new_typevar()
      in 
      if iskey s env
      	 then raise StaticAnalysisError "Duplicate parameter"
      else (APAIRPAR ((s, nt), st), extend (env, s, TYPE nt))
      end 
  fun anullpar () = (ANULLPAR, empty_env)
  val evar_hom = EHOM { noexp = noexp,
                   literal = literal,
                   variable = variable,
                   call = call,
                   lambda = lambda,
                   ifexp = ifexp,
                   assign = assign,
                   avarpar = avarpar,
                   apairpar = apairpar,
                   anullpar = anullpar,
                   pairarg = pairarg,
                   nullarg = nullarg}
  in
  fun make_attributes_exp e = apply_ehom evar_hom e

  fun make_attributes_def (DEFINE ((s, ()), e)) (Env : (string, typescheme) env) =
        let val nt = new_typevar()
            val Env' = extend(Env, s, TYPE nt)
        in DEFINE ((s, nt), make_attributes_exp e Env')
        end 

(* fun make_attributes program Env =
      case program of
        [] => []
     |  def::program' => let val (attdef, Env') =
                          make_attributes_def def Env
                      in
                          attdef::(make_attributes program' Env')
                      end
 *)                         

end;




 (* 3rd stage: unify types according to equational type rules,
	       resulting in a canonical well-annotation *)

  local   
  open Datum Exp Type PolyType Coercion UnionFind SchemeGeneral
  fun booldat (a,b) = hi_type a before link (lo_type a, bool())
  fun chardat (a,c) = hi_type a before link (lo_type a, char())
  fun stridat (a,c) = hi_type a before link (lo_type a, string())
  fun symbdat (a,s) = hi_type a before link (lo_type a, symbol())
  fun numbdat (a,s) = hi_type a before link (lo_type a, number())
  fun vectdat (a,l) = hi_type a before 
         let val nt = new_typevar()
         in apply (fn t => link (t, nt)) l; 
            link (lo_type a, vector nt)
         end
  fun pairdat (a,d1,d2) = hi_type a before link (lo_type a, pair(d1,d2))
  fun nildat a = hi_type a before link (lo_type a, unit())
  val dunify_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val dunify = apply_dhom dunify_hom

  fun noexp a = hi_type a before link (lo_type a, unspec())
  fun literal d = dunify d
  fun variable (a, (s, TYPE t)) = hi_type a before link (lo_type a, t)
    | variable (a, (s, TSCHEME (_, t))) = hi_type a before link (lo_type a, t)
  fun call (a,e,l) = hi_type a before link(e, func(l, lo_type a))
  fun lambda (a,f,e) = hi_type a before link(lo_type a, func(f,e)) 
  fun ifexp (a,e,e',e'') = hi_type a before (union (e', e'');
                                             union (lo_type a, e');
                                             link (e, bool()))
  fun assign _ = raise Unimplemented "assign, in unify"
  fun pairarg (a,e,l) = hi_type a before link (lo_type a, pair(e,l))
  fun nullarg a = hi_type a before link (lo_type a, unit())
  fun avarpar (s,t) = t
  fun apairpar ((s,t),f) = pair(t,f)
  fun anullpar () = unit()
  val unify_hom = EHOM { noexp = noexp,
                   literal = literal,
                   variable = variable,
                   call = call,
                   lambda = lambda,
                   ifexp = ifexp,
                   assign = assign,
                   avarpar = avarpar,
                   apairpar = apairpar,
                   anullpar = anullpar,
                   pairarg = pairarg,
                   nullarg = nullarg}

  in
  val unify_type_exp = apply_ehom unify_hom

  fun unify_type_def (DEFINE ((s,t), e)) = t before link(t, unify_type_exp e)

(*  fun unify_type program = apply unify_type_def program *)
  
  end; (* local open *)





(* 4th stage: Collect constraints from source program *)
   
  local
  open SchemeGeneral Datum Exp Constraint Type PolyType
  fun booldat (a,b) = leaf a
  fun chardat (a,c) = leaf a
  fun stridat (a,c) = leaf a
  fun symbdat (a,s) = leaf a
  fun numbdat (a,s) = leaf a
  fun vectdat (a,l) = APPEND (leaf a :: l)
  fun pairdat (a,d1,d2) = APPEND [leaf a, d1, d2]
  fun nildat a = leaf a
  val dconstraints_hom = DHOM { booldat = booldat,
                   chardat = chardat,
                   stridat = stridat,
                   symbdat = symbdat,
                   numbdat = numbdat,
                   vectdat = vectdat,
                   pairdat = pairdat,
                   nildat= nildat }
  val dconstraints = apply_dhom dconstraints_hom

  fun noexp a = leaf a
  val literal = dconstraints
  fun variable (a, (s, TYPE _)) = constraints a
    | variable (a, (s, TSCHEME (cl, _))) = APPEND [constraints a, LIST cl]
  fun call (a,e,l) = APPEND [leaf a, e, l]
  fun lambda (a,_,e) = APPEND [constraints a, e]
  fun ifexp (a,e,e',e'') = APPEND [leaf a, e, e', e'']
  fun assign (a,v,e) = APPEND [leaf a, e]
  fun pairarg (a,e,l) = APPEND [leaf a, e, l]
  fun nullarg a = leaf a
  fun avarpar _ = ()
  fun apairpar _ = ()
  fun anullpar _ = ()
  val constraints_hom = EHOM { noexp = noexp,
                   literal = literal,
                   variable = variable,
                   call = call,
                   lambda = lambda,
                   ifexp = ifexp,
                   assign = assign,
                           avarpar = avarpar,
                   apairpar = apairpar,
                   anullpar = anullpar,
                   pairarg = pairarg,
                   nullarg = nullarg}

  fun get_types_in_type at =
    let val collected = get #collected at
    in if !collected 
       then LIST []
       else (collected := true;
             case utype at of
  			   TVAR _ => leaf at
           	 | SIMPLE (tt, tlist) => APPEND (leaf at :: map get_types_in_type tlist)
           	 | DYN f => APPEND (leaf at :: map get_types_in_type (summands f)))
    end
  fun get_types_in_constraint (at1, at2) =
  	APPEND [get_types_in_type at1, get_types_in_type at2]
  in
  fun get_constraint_exp e = apply_ehom constraints_hom e

  fun get_constraint_def (DEFINE (tv, e)) = get_constraint_exp e
  
  fun get_types al =
      let fun gt (LIST l) = APPEND (map get_types_in_constraint l)
            | gt (APPEND l) = APPEND (map gt l)
          val alltypes = gt al
      in (foreach (fn t => get #collected t := false) alltypes; alltypes)
      end
    
(*  fun get_constraint program =
        case program of
           [] => anil
        |  def::program' => APPEND[get_constraint_def def, 
                                get_constraint program']
*)
                               
 end; (* local open *)



(* 5th stage: Perform unifications induced by closure condition of
              simple value flow graphs *)

  local
  open SchemeGeneral Type UnionFind Environment
  fun equiv (t1, t2): unit =
      let val t1' = ecr t1
          val t2' = ecr t2
      in if equal(t1', t2')
            then ()
         else case (utype t1', utype t2') of
                (DYN f, DYN f') => (elink (t1', t2'); 
                                    apply aliassimple (zip (summands f, summands f')))
              | (DYN f, SIMPLE (tt, tlist)) => 
                     (elink (t2', t1');
                      aliassimple (f tt, t2'))
              | (DYN _, TVAR _) => elink (t2', t1')
              | (SIMPLE (tt, _), DYN f) => 
                      (elink (t1', t2');
                       aliassimple (t1', f tt))
              | (SIMPLE (tt, tlist), SIMPLE (tt', tlist')) => 
                   if tt = tt' then
		              (elink (t1', t2');
                       apply aliasvar (zip (tlist, tlist')))
                   else let val rd = make_dyn_type [t1', t2']
                        in (elink (t1', rd);
                            elink (t2', rd))
                        end
              | (SIMPLE _, TVAR _) => elink (t2', t1')
              | (TVAR _, DYN _) => elink (t1', t2')
              | (TVAR _, SIMPLE _) => elink (t1', t2')
              | (TVAR _, TVAR _) => elink (t1', t2')
      end
  and aliassimple (t1, t2) =
      if equal (t1, t2)
      then ()
      else case (utype t1, utype t2) of
             (SIMPLE (tt, tlist), SIMPLE (tt', tlist')) =>
             	if tt = tt' 
             	then apply aliasvar (zip (tlist, tlist'))
             	else error ("aliassimple", "Unequal type tags -- impossible")
           | (SIMPLE _, TVAR _) => elink(t2, t1)
           | (TVAR _, SIMPLE _) => elink(t1, t2)
           | (TVAR _, TVAR _) => elink(t1, t2)
           | _ => error ("aliassimple", "No dyn types allowed")
  and aliasvar (t1, t2): unit =
  	  if equal (t1, t2)
  	  then ()
  	  else case (utype t1, utype t2) of
  	  	     (TVAR _, TVAR _) => let val t1' = ecr t1
  	  	     						 val t2' = ecr t2
  	  	     				     in if equal(t1,t1')
  	  	     				        then (link(t2,t1); equiv(t1',t2'))
  	  	     				        else (link(t1,t2); equiv(t1',t2'))
  	  	     				     end
  	  	    	(* NOT equiv (t1, t2); union (t1, t2) or
  	  	    		   union (t1, t2); equiv (t1, t2) !!! 
  	  	    	   Don't touch this code; it'll surely break if you do -- 
  	  	    	   guaranteed *)
  	  	   | (_, _) => error ("aliasvar", "Arguments must be type variables!")
  in  	  	   
  val induced_unify = foreach equiv
  end;
  


(* 6th stage: Set predecessor/successor fields in constraints *)

  local 
  open Type SchemeGeneral UnionFind
  fun pred_succ (l,h) =
      if equal (l, h) 
         then ()
      else let 
           val pred_h = get #preds h
	       val numpreds_h = get #numpreds h
           val succ_l = get #succs l
	       val numsuccs_l = get #numsuccs l
           in 
           (pred_h := l :: (!pred_h);
	        numpreds_h := !numpreds_h + 1;
            succ_l := h :: (!succ_l);
	        numsuccs_l := !numsuccs_l + 1)
           end
  in
  fun reset_pred_succs c =
      foreach (fn (t1,t2) => (get #preds t2 := nil; get #numpreds t2 := 0;
			                  get #succs t1 := nil; get #numsuccs t1 := 0)) c
  val pred_succs = foreach pred_succ
  fun find_sources nil = nil
    | find_sources (t::r) =
	if !(get #numpreds t) = 0 
	   then t :: find_sources r
        else find_sources r
  fun find_sinks nil = nil
    | find_sinks (t::r) =
	if !(get #numsuccs t) = 0 
	   then t :: find_sinks r
        else find_sinks r
  end;


(* 7th stage: eliminate cycles in simple value flow graph by collapsing
              them into a single node, resulting
	      in acyclic simple value flow graph [asvfg] *)

local
open SchemeGeneral Type UnionFind

fun visit (node, componentreps) =
   let val color = get #color node
   in
   case !color of
     WHITE => (color := GREY;
               let val succs = !(get #succs node)
                   val comps = List.foldr visit (node :: componentreps) succs
               in (color := BLACK; comps)
               end)
   | GREY => let val sccrepnode = sccrep node
                     fun split left nil = 
                                error ("visit", "No components")
                       | split left (right as (a::r)) = 
                                if equal(a,sccrepnode) then 
                                       (left, right)
                                    else split (a::left) r
                     val (l,r) = split nil componentreps
             in (apply (fn n => get #sccptr n := Some sccrepnode) l; r)
             end
   | BLACK => if !(get #color (sccrep node)) = GREY
              then visit (sccrep node, componentreps)
              else componentreps
   end
in
fun make_acyclic C =
    (foreach (fn (t1,t2) => (visit (t1, []))) C;
     foreach (fn (t1,t2) => (union (t1, sccrep t1);
     			             union (t2, sccrep t2))) C)
fun reset_shade x = foreach (fn t => get #color t := WHITE) x
end;

(* 8th stage: Propagate sources and sinks: types that can be reached
              from a variable source have their neg field set; 
	      those that reach a variable sink have their pos field set;
	      sources and sinks contain the set of simple types 
	      reaching resp. reached from particular type *)

local
open Environment Type SchemeGeneral
fun initialize_source at =
	    case (utype at) of
	          DYN _ => error ("initialize_source", 
	          				  "No dynamic types in constraints allowed")
            | SIMPLE (tt, _) => get #sources at := make_env (tt, at)
            | TVAR _ => get #neg at := true

fun initialize_sink at =
	    case (utype at) of
	      DYN _ => error ("initialize_sink", 
	                      "No dynamic types in constraints allowed")
            | SIMPLE (tt, _) => get #sinks at := make_env (tt, at)
            | TVAR _ => get #pos at := true

fun or_combine (env1, env2)
	   = List.foldr (fn (tt, env) => 
		if iskey tt env1 
		   then extend (env, tt, lookup tt env1)
                else if iskey tt env2
	             then extend (env, tt, lookup tt env2)
                else env)
		empty_env 
	        type_tags 

fun and_combine (env1, env2)
	   = List.foldr (fn (tt, env) => 
		if iskey tt env1 andalso iskey tt env2
		   then extend (env, tt, lookup tt env1)
	        else env)
		empty_env
	        type_tags 

fun propagate_sources nil = ()
  | propagate_sources (at :: r) =
      let val _ =
     	 	 if !(get #preds at) = nil
		     then initialize_source at
	         else (get #sources at := 
	         	    let 
	         	    val envs = map (fn pt => !(get #sources pt))
	         	    				   (!(get #preds at))
	         	    fun collect cumarg nil = cumarg
	         	      | collect cumarg (penv::r) = 
	         	    	  collect (or_combine (penv, cumarg)) r	
                    in collect empty_env envs
                    end;
	               get #neg at :=
			           List.exists (fn pt => !(get #neg pt)) (!(get #preds at)))
          val newsrcs =
	         List.foldr (fn (at, newsrcs) => 
			         let val numpreds = get #numpreds at
			         in (numpreds := !numpreds - 1;
			             if !numpreds = 0 
			             then at::newsrcs
                         else newsrcs)
		             end) 
		         r
		         (!(get #succs at)) 
     in propagate_sources newsrcs
     end
fun propagate_sinks nil = ()
  | propagate_sinks (at :: r) =
      let val _ =
		 	 if !(get #succs at) = nil
	 		 then initialize_sink at
	    	 else (get #sinks at := 
				     List.foldr (fn (pt, env) => or_combine (!(get #sinks pt), env))
			        	  empty_env
			    	          (!(get #succs at));
	           	   get #pos at :=
			      	 List.exists (fn st => !(get #pos st)) (!(get #succs at)))
          val newsnks =
	        List.foldr (fn (at, newsnks) => 
			        let val numsuccs = get #numsuccs at
	      			in (numsuccs := !numsuccs - 1;
					    if !numsuccs = 0 
			       		then at::newsnks
                        else newsnks)
		            end) 
		         r
		         (!(get #preds at)) 
       in propagate_sinks newsnks
       end


(* 9th stage: Interpret types as follows: 
	      pos/neg labeled type variables remain type variables;
	      they get unified with pos/neg labeled predecessors, however; 
	      Other types get interpreted by unifying them with 
	      intersection of their source type and their sink type *)

open UnionFind

fun contract e =
    (* deletes PAIR type from e if it also
       contains a LST type *)
    if iskey LST e 
    then delete PAIR e
    else e		 
	
fun interpret_type t =
    let val neg = !(get #neg t)
        val pos = !(get #pos t)
        val sources = !(get #sources t)
        val csources = contract sources
        val sinks = !(get #sinks t)
        val csinks = contract sinks
        val interpreted = get #interpreted t
        val preds = !(get #preds t)
        val succs = !(get #succs t)
    in if !interpreted 
       then ()
       else (interpreted := true;
             case utype t of
	           SIMPLE (tt, tlist) => apply interpret_type tlist
             | DYN f => apply interpret_type (summands f)
             | TVAR _ =>
                 (case (pos, neg) of
                   (true, true) =>
                   	(case (keys csources, keys csinks) of
                   	   (_::_::_, _::_::_) => 
                   	   	 link(t, make_dyn_type (values sources @ values sinks))
                   	 | (_::_::_, _) =>
                   	   apply (fn tpr =>
		                case utype tpr of
			              TVAR _ =>
			                if !(get #pos tpr) andalso !(get #neg tpr)
			                then union (t, tpr)
			                else ()
                        | SIMPLE _ => ()
			            | DYN _ => error ("interpret_type", 
					                      "Must not be a DYN type (forward)")) succs
					 | _ => 
                       apply (fn tpr =>
		                case utype tpr of
			              TVAR _ =>
			                if !(get #pos tpr) andalso !(get #neg tpr)
			                then union (t, tpr)
			                else ()
                        | SIMPLE _ => ()
			            | DYN _ => error ("interpret_type", 
					                      "Must not be a DYN type (backward)")) preds)
                 | (true, false) => 
                      (case keys csources of
		                 nil => error ("interpret_type", 
					                   "No sources -- impossible!")
	 	               | [tt] => link(t, lookup tt sources)
                       | _ => link(t, make_dyn_type (values sources)))
                 | (false, true) =>
                      (case keys csinks of
		                 nil => error ("interpret_type",
					                   "No sinks -- impossible!")
	                   | [tt] => link(t, lookup tt sinks)
                       | _ => link(t, make_dyn_type (values sinks)))
                 | (false, false) =>
                      let val ienv = and_combine (sources, sinks)
	                  in case keys ienv of
	                       nil => link(t, unspec())
		                 | [tt] => link(t, lookup tt ienv)
		                 | _ => link(t, make_dyn_type (values ienv))
  	                  end))
      end

local
val rec_counter = ref 0
in
fun new_name () =
    "REC" ^ Int.toString (!rec_counter) before rec_counter := !rec_counter + 1
end

fun mark_rec_type t =
    let val color = get #color t 
    in case !color of 
    	WHITE => (color := GREY;
				  (case utype t of
				  	TVAR _ => ()
				  | SIMPLE (_, tlist) => 
				  	   apply mark_rec_type tlist
				  | DYN f => 
				       apply mark_rec_type (summands f));
				  color := BLACK)
	  | GREY => let val name = get #name t
	            in case !name of
	  		  	     Some _ => ()
	  			   | None => name := Some (new_name())
	  			end
	  | BLACK => ()
    end
     
in
fun set_sources N =
    let val sources = find_sources (aflatten [N])
    in propagate_sources sources
    end
fun set_sinks N =
    let val sinks = find_sinks (aflatten [N])
    in propagate_sinks sinks
    end
val set_types = foreach interpret_type
fun mark_rec_types N = 
    (foreach (fn t => get #color t := WHITE) N;
    foreach mark_rec_type N)
end;


(* 10th stage: collect all coercion parameters *)

local
open UnionFind Type
in
fun coercion_parameters nil = nil
  | coercion_parameters ((s as (t1,t2)) :: r) =
  	  if equal(t1, t2)
  	  then coercion_parameters r
  	  else case utype t1 of
  	         TVAR _ => s :: coercion_parameters r
  	       | _ => (case utype t2 of
  	                TVAR _ => s :: (coercion_parameters r)
  	              | _ => coercion_parameters r)
end


(* 11th stage: initialize top level environment *)

local 
open Environment Type PolyType
fun tlist nil = unit()
  | tlist (a::ts) = pair(a, tlist ts)
fun funcl (l, t) = func (tlist l, t)
in
val topenv = list2env
    [ ("cons", let val nt1 = new_typevar()
    		       val nt2 = new_typevar()
    		   in close ([], funcl ([nt1, nt2], pair(nt1, nt2)))
    		   end),
      ("car", let val nt1 = new_typevar()
      		      val nt2 = new_typevar()
      		  in close ([], funcl ([pair(nt1,nt2)], nt1))
      		  end),
      ("cdr", let val nt1 = new_typevar()
                  val nt2 = new_typevar()
              in close ([], funcl ([pair(nt1,nt2)], nt2))
              end),
      ("null?", let val nt = new_typevar()
      		    in close ([], funcl ([listtype nt], bool()))
      		    end)]
end


(* Old type for null?:
				let val nt1 = new_typevar()
                    val nt2 = new_typevar()
                in close ([], func(tlist [make_dyn_type [unit(), 
                			   pair(nt1,nt2)]], bool()))
                end *)

