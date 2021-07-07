(*$UnionFind: UNIONFIND *)

structure UnionFind: UNIONFIND =

(* UNIONFIND DATA STRUCTURE WITH PATH HALVING AND RANKED UNION

Created by: Fritz Henglein, DIKU, University of Copenhagen (henglein@diku.dk)
Date:       29 Dec 1994

Maintenance: Author

Comment:     This implementation of union/find was found to be the most
	     efficient combination of implementations of the basic
	     union and find operations

RCS LOG

*)

struct

  exception UnionFind of string

  datatype 'a urefC = 
        ECR of 'a * int
      | PTR of 'a uref
  withtype 'a uref = 'a urefC ref


  fun find (p as ref (ECR _)) = p
    | find (p as ref (PTR (p' as ref (ECR _)))) = p'
    | find (p as ref (PTR (p' as ref (PTR p'')))) =
	(p := PTR p''; find p'')

  fun uref x = ref (ECR (x, 0))

  fun !! p = 
      case !(find p) of
        ECR (x, _) => x
      | _ => raise UnionFind "!!"
      
  fun equal (p, p') = (find p = find p')

  fun ::= (p, x) = 
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
                     (p' := PTR q'; 
                      q' := ECR (f(pc,qc), qr+1))
                  else if pr < qr then 
                     (p' := PTR q'; 
                      q' := ECR (f(pc,qc), qr))                     
                  else (* pr > qr *)
                     (q':= PTR p';
                      p' := ECR (f(pc,qc), pr))
               | _ => raise UnionFind "union_with")
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

