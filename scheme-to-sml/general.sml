(*$SchemeGeneral: SCHEMEGENERAL *)

structure General (*: SCHEMEGENERAL *) = 
  struct

  datatype 'a Option = None | Some of 'a
  datatype ('a, 'b) Result = OK of 'a | Fail of 'b
  type natural = int

  exception Unimplemented of string
  exception IllegalInput of string * string 
  exception EOF
  exception EXIT

  fun error (s1,s2) = 
  	(print("Error in " ^ s1 ^ ": " ^ s2); raise IllegalInput (s1,s2))

  fun debug(x,y) = (print("DEBUG:\n");
                    print(x^"  "^y^"\n"))


  fun apply f [] = ()
    | apply f (a::r) = (f a; apply f r)

  fun zip ([], []) = []
    | zip (a::r, a'::r') = (a,a')::zip (r,r')
    | zip (_, _) = error ("zip", "lists of different lengths")

  fun foldappend ls = List.foldr op@ ls nil

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

  end

