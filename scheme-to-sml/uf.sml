(*$UnionFind: UNIONFIND *)

structure UnionFind: UNIONFIND =

(* UNIONFIND DATA STRUCTURE WITH PATH COMPRESSION AND RANKED UNION

Created by: Fritz Henglein, DIKU, University of Copenhagen (henglein@diku.dk)
Date:       29 Dec 1994

Maintenance: Author

RCS LOG

*)

struct

  exception UnionFind of string

  datatype 'a urefC = 
        ECR of 'a * int
      | PTR of 'a uref
  withtype 'a uref = 'a urefC ref

  fun find (p as ref (ECR _)) = p
    | find (p as ref (PTR p')) =
         let val p'' = find p'
         in (p := PTR p''; p'')
         end

  fun uref x = ref (ECR (x, 0))

  fun !! p = 
      case !(find p) of
        ECR (x, _) => x
      | _ => raise UnionFind "!!"
      
  fun equal (p, p') = (find p = find p')

  fun op::= (p, x) = 
      let val p' = find p
      in case !p' of
           ECR (_, r) => (p' := ECR (x, r))
         | _ => raise UnionFind "::="
      end

  fun link (p, q) =
      let val p' = find p
          val q' = find q
      in if p' = q'
	    then ()
         else p' := PTR q
      end

  fun union_with f (p, q) = 
          let val p' = find p
              val q' = find q
          in (case (!p', !q') of
               (ECR (pc, pr), ECR (qc, qr)) =>
                  if p' = q' then
                     p' := ECR (f(pc,pc), pr)
                  else if pr = qr then 
                     (q' := ECR (f(pc,qc), qr+1);
                      p' := PTR q')
                  else if pr < qr then 
                     (q' := ECR (f(pc,qc), qr);
                      p' := PTR q')
                  else (* pr > qr *)
                     (p' := ECR (f(pc,qc), pr);
                      q':= PTR p')
               | _ => raise UnionFind "union")
          end

  fun union (p, q) = 
          let val p' = find p
              val q' = find q
          in if p' = q' 
                then ()
             else (case (!p', !q') of
                     (ECR (pc, pr), ECR (qc, qr)) =>
                        if pr = qr 
                           then (q' := ECR (qc, qr+1);
                                 p' := PTR q')
                        else if pr < qr
                                then p' := PTR q'
                             else q':= PTR p'
                    | _ => raise UnionFind "union")
          end

end
