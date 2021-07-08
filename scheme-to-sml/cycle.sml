(* eliminate cycles in constraints/simple value flow graph *)

local
open General Type UnionFind

fun visit (node, componentreps) =
   let val color = get #color node
   in
   case !color of
     WHITE => (color := GREY;
               let val succs = !(get #succs node)
                   val comps = fold visit succs (node :: componentreps)
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
             in (apply (fn n => get #sccptr n := Some sccrepnode) l;
                     r)
             end
   | BLACK => if !(get #color (sccrep node)) = GREY
              then visit (sccrep node, componentreps)
              else componentreps
   end
in
val make_acyclic =
    foreach (fn (t1,t2) => (visit (t1, [])))
     
end




               