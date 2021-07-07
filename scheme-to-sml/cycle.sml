(* eliminate cycles in constraints/simple value flow graph *)

(* Bottom-up SCC (Tarjan, Dijkstra) *)

fun sccs2 (vs, neighbors, number) =
    let val numbvs = length vs + 100
        fun finish n = (number n := numbvs; n)
        fun split(stack, x) =
            let exception Loop
                fun loop (acc, nil) = raise Loop
                  | loop (acc, x'::xs) = 
                        if !(number x') = !(number x) then (finish x'::acc, xs) 
                        else loop(finish x'::acc, xs)
            in loop (nil, stack)
            end
        local val counter = ref 0 
        in fun next() = (counter := !counter + 1; !counter)
        end
        fun dfs_visit (v, (parent_low_link_bound, state as (sccs, elems))) =
            if !(number v) = 0 (* WHITE *)
               then let val dfs_number = next()
                        val _ = number v := dfs_number
                        val (low_link, state' as (sccs', elems')) = 
                            revfold dfs_visit (neighbors v) (dfs_number, (sccs, v::elems))
                    in if low_link >= dfs_number then
                          let val (scc, elems'') = split (elems', v)
                          in (parent_low_link_bound, (scc :: sccs', elems''))
                          end
                       else (* low_link < dfs_number *)
                          (min(parent_low_link_bound, low_link), state')
                    end 
            else (* GREY or BLACK *)
               (min(parent_low_link_bound, !(number v)), state)
     in (app (fn v => number v := 0) vs; 
         #1 (#2 (revfold dfs_visit vs (0, ([], [])))))
     end;



fun collapse nil = ()
  | collapse [n] = ()
  | collapse (a1::(ar as a2::r)) = 
	(UnionFind.union (a1, a2); collapse ar)

fun collapseN nil = ()
  | collapseN (fst_component :: rem_components) = 
	(collapse fst_component; collapseN rem_components)


               