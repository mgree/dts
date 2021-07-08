(*$SchemeGeneral: SCHEMEGENERAL *)

structure SchemeGeneral (*: SCHEMEGENERAL *) = 
  struct

  open TextIO

  datatype 'a Option = None | Some of 'a
 
  datatype ('a, 'b) Result = OK of 'a | Fail of 'b
  type natural = int

  exception Unimplemented of string
  exception IllegalInput of string * string 
  exception EOF
  exception EXIT

  fun error (s1,s2) = 
  	(output(stdErr, "Error in " ^ s1 ^ ": " ^ s2); 
	 raise IllegalInput (s1,s2))

  fun debug(x,y) = (output(stdErr,"DEBUG:\n");
                    output(stdErr, x^"  "^y^"\n"))

  fun unsome (Some x) = x 
    | unsome None = error ("unsome", "Trying to unsome a none")

  fun fst (x, _) = x
  fun snd (_, y) = y

  fun apply f [] = ()
    | apply f (a::r) = (f a; apply f r)

  fun zip ([], []) = []
    | zip (a::r, a'::r') = (a,a')::zip (r,r')
    | zip (_, _) = error ("zip", "lists of different lengths")
    
  fun unzip l =
      let fun uz res nil = res
            | uz (fsts,snds) ((a1,a2)::r) = uz (a1::fsts, a2::snds) r
          val (fsts,snds) = uz (nil,nil) l
      in (rev fsts, rev snds)
      end

  fun flatten ls = List.foldr op@ nil ls

  fun member x [] = false
    | member x (y::tl) = if x=y then true else (member x tl)

  fun forall f nil = true
        | forall f (a::l) = f a andalso forall f l
        
  datatype 'a applist =   LIST of 'a list 
                        | APPEND of 'a applist list

  fun leaf a = LIST [a]
  val anil = LIST []

  fun foreach f (LIST l) = apply f l
    | foreach f (APPEND l) = apply (foreach f) l

  fun aflatten nil = nil
    | aflatten (LIST nil :: r) = aflatten r
    | aflatten (LIST (a::r) :: l) = a :: aflatten (LIST r :: l)
    | aflatten (APPEND nil :: r) = aflatten r
    | aflatten (APPEND (a :: r) :: l) = aflatten (a :: APPEND r :: l)
    
  datatype 'a aotree =   EMPTYAO
                       | SINGLE of 'a
                       | OR of 'a aotree * 'a aotree
                       | AND of 'a aotree * 'a aotree

  fun aoflatten T =
      let fun f l EMPTYAO = l
            | f l (SINGLE x) = x :: l
            | f l (OR (T1, T2)) = f (f l T1) T2
            | f l (AND (T1, T2)) = f (f l T1) T2
      in f [] T
      end

  end;

structure Environment =
  struct
  local open SchemeGeneral
  in

  type (''a, 'b) env = (''a * 'b) list

  val empty_env = []

  fun make_env y = [y]
  
  fun list2env l = l

  fun add (e,e') = e' @ e

  fun extend (e,s,v) = (s,v) :: e
  
  fun delete k nil = nil
    | delete k ((b as (k',_))::r) =
    	if k = k' 
    	then delete k r
    	else (b:: delete k r)

  exception Lookup 
  fun lookup s [] = raise Lookup
    | lookup s ((s',v)::r) = 
         if s = s' then v else lookup s r

  fun iskey s e = (let val v = lookup s e in true end) handle Lookup => false
  
  fun keys e = 
      let fun keysacc sofar nil = sofar
            | keysacc sofar ((k,_)::r) =
            	if member k sofar
            	then keysacc sofar r
            	else keysacc (k::sofar) r 
      in keysacc nil e
      end 
  
  fun values e = map (fn k => lookup k e) (keys e)

  end
  end;

(*$UnionFind: UNIONFIND *)

structure UnionFind (* : UNIONFIND *) =

(* UNIONFIND DATA STRUCTURE WITH PATH COMPRESSION AND RANKED UNION

Created by: Fritz Henglein, DIKU, University of Copenhagen (henglein@diku.dk)
Date:       29 Dec 1994

Maintenance: Author

RCS LOG

*)

struct

  exception UnionFind of string

  datatype 'a UF = 
        ECR of 'a * int
      | PTR of 'a uref
  withtype 'a uref = 'a UF ref

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

  infix ::=
   
  fun combine f (p, q) =
      if equal(p,q) 
      then ()
      else let val pc = !!p
               val qc = !!q
           in (union (p, q);
               p ::= f(pc,qc))
           end

end;


























