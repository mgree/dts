(*$UnionFind: UNIONFIND *)

structure SimpleUnionFind: UNIONFIND =

(* UNIONFIND DATA STRUCTURE WITH PATH COMPRESSION

Created by: Fritz Henglein, DIKU, University of Copenhagen (henglein@diku.dk)
Date:       31 Dec 1994

Maintenance: Author

RCS LOG

*)

struct

  exception UnionFind of string

  datatype 'a UFC = 
        ECR of 'a
      | PTR of 'a UF
  withtype 'a UF = 'a UFC ref

  fun find (p as ref (ECR _)) = p
    | find (p as ref (PTR p')) =
         let val p'' = find p'
         in (p := PTR p''; p'')
         end

  fun make x = ref (ECR x)

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

  fun union (p, q) = 
      let val p' = find p
          val q' = find q
      in if p' = q' 
            then ()
         else p' := PTR q'
      end

end
