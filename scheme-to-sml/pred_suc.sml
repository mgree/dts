(* Set predecessor/successor fields in constraints *)

local 
open Type General UnionFind
fun pred_succ (l,h) =
      if equal (l, h) 
         then ()
      else let val pred_h = get #preds h
               val succ_l = get #succs l
           in (pred_h := l :: (!pred_h);
               succ_l := h :: (!succ_l))
           end
in
val pred_succs = foreach pred_succ
end
