(*$UnionFind: UNIONFIND *)

structure SimpleUnionFind: UNIONFIND =

(* UNIONFIND DATA STRUCTURE WITH PATH COMPRESSION

Created by: Fritz Henglein, DIKU, University of Copenhagen (henglein@diku.dk)
Date:       31 Dec 1994

Maintenance: Author

RCS LOG

*)

struct

  exception UnionFind of string

  datatype 'a urefC = 
        ECR of 'a
      | PTR of 'a uref
  withtype 'a uref = 'a urefC ref

  fun find (p as ref (ECR _)) = p
    | find (p as ref (PTR p')) =
         let val p'' = find p'
         in (p := PTR p''; p'')
         end

  fun uref x = ref (ECR x)

  fun !! p = 
      case !(find p) of
        ECR x => x
      | _ => raise UnionFind "!!"
      
  fun equal (p, p') = (find p = find p')

  fun ::= (p, x) = 
      let val p' = find p
      in p' := ECR x
      end
  infix ::=  

  fun link (p, q) = 
      let val p' = find p
          val q' = find q
      in if p' = q' 
            then ()
         else p' := PTR q'
      end
 
  val union = link

  fun union_with f (p, q) = 
      let val p' = find p
          val q' = find q
      in if p' = q'
            then case !q' of
                   ECR vq => (q' := ECR (f(vq,vq)))
                 | _ => raise UnionFind "union_with (equal args)"
         else case (!p', !q') of
                (ECR vp, ECR vq) => 
                	(q' := ECR (f(vp,vq)); p' := PTR q')
              | _ => raise UnionFind "union_with (unequal args)"
      end

end
