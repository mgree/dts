structure CollapseCycles =
  struct

  local open SchemeTypes
  in


  fun visit [] = []
    | visit (t::r) =
	case !(get #color t) of
	  WHITE => (* t hasn't been reached before *)
	    (get #color t := GREY; (* t has been reached, not completely processed *)
	     let val unprocessed = visit (!(get #succs t) @ r))   
	     in 


 fun visitall [] = (b, [])
    | visitall (t::r) = 
	let val (cycle_found, unprocessed) = visit t
	in if cycle_found
	      then if !(get #color t) = BLACK
		      then (false, unprocessed @ r)
	           else (get #color t := BLACK;
		         (true, unprocessed @ r))
	   else 	  

 
  fun visit t =
      let val color = get #color t
	  fun process tlist =
	      


	
          val cycle_found = ref false 
      in case !color of
	   WHITE => (color := GREY;
		     let val succs = get #succs t
		     in while not (!cycle_found) andalso !succs <> nil  do
			  let val t' = hd (!succs)
			  in (succs := tl (!succs);
			      cycle_found := visit t';
			      if !cycle_found 
				 then (succs := !succs @ !(get #succs t');
				       link (t', t))
			      else ())
                          end
	              end;
                      if !cycle_found
			 then if !color = BLACK
	                         then false
                              else (color := BLACK;
				    true)
	              else (color := BLACK;
			    false))
	  | GREY => (if color := BLACK;
		     true)
	  | BLACK => false
       end
    
  end
end
 
