(*$UnionFind: UNIONFIND *)

structure UnionFind: UNIONFIND =

(* UNIONFIND DATA STRUCTURE WITH PATH COMPRESSION AND RANKED UNION

Created by: Fritz Henglein, DIKU, University of Copenhagen (henglein@diku.dk)
Date:       29 Dec 1994

Maintenance: Author

RCS LOG

*)

struct

  exception UnionFind of string

  datatype 'a UFC = 
        ECR of 'a * int
      | PTR of 'a UF
  withtype 'a UF = 'a UFC ref

  fun find (p as ref (ECR _)) = p
    | find (p as ref (PTR p')) =
         let val p'' = find p'
         in (p := PTR p''; p'')
         end

  fun make x = ref (ECR (x, 0))

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
  infix ::=  

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
