(*$SchemePolyTypes: SchemeCoercions KernelTypes *)

structure SchemePolyTypes =
  struct
  local open KernelTypes SchemeCoercions 
  in
  
  datatype typescheme =
  	TSCHEME of coercion_sig list * atype
  |	TYPE of atype

  fun close (Fvars, (C, t)) = 
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
  	  	  	    let val genatt = get #generic at
  	  	  	    in if !genatt
  	  	  	          then (genatt := false;
  	  	  	          	    case utype at of
  	  	  	          	      TVAR _ => () |
  	  	  	          	      SIMPLE (_, tlist) => apply make_nongeneric tlist |
  	  	  	          	      DYN f => apply make_generic (summands f))
  	  	  	          else ()
  	  	  	    end
  	   in (apply (fn (t1, t2) => (make_generic t1; make_generic t2)) C;
  	   	   make_generic t;
  	   	   apply make_nongeneric Fvars;
  	   	   TSCHEME (C, t))
  	   end

  fun unsome (Some x) = x 
    | unsome None = raise IllegalInput ("unsome", "Trying to unsome a none")

 fun instantiate (TSCHEME (C, t)) =
     let
     val insttypes = ref []
     fun inst aty =
      let val atts = attributes aty
      in if !(#generic atts)
         then if !(#instantiated atts)
              then unsome (!(#instance atts))
              else (#instantiated atts:= true;
                   insttypes := aty :: !insttypes;
                   case utype aty of
                     TVAR _ => 
                       let val nt = new_typevar ()
                       in nt before #instance atts := Some nt
                       end |
                     SIMPLE (tt, tlist) => 
                       let val it = make_type (tt, map inst tlist) 
                       in it before #instance atts := Some it
                       end |
                     DYN f => 
                       let val it = make_dyn_type (map (inst o f) type_tags)
                       in it before #instance atts := Some it
                       end)
          else aty
      end
     val C' = map (fn (t1, t2) => (inst t1, inst t2)) C
     val t' = inst t 
     in (apply (fn aty => let val atts = attributes aty
    			         in#instantiated atts := false
    			         end) (!insttypes);
        (C', t'))
     end
   | instantiate (TYPE aty) = ([], aty)
   
  end
end
  
