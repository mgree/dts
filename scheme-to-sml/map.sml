structure Map =
  struct

  type (''a, 'b) map = (''a * 'b) list

  val empty = []

  fun make y = [y]

  fun add (e,e') = e' @ e

  fun extend (e,s,v) = (s,v) :: e

  exception Lookup 
  fun lookup s [] = raise Lookup
    | lookup s ((s',v)::r) = 
         if s = s' then v else lookup s r

  fun indom s e = (true before lookup s e) handle Lookup => false
  
  val domain = map (#1: 'a * 'b -> 'a)
 
  val range = map (#2: 'a * 'b -> 'b)

  fun delete s [] = []
    | delete s ((f as (s',_))::r) = 
	if s = s' then delete s r
        else f :: delete s r

  end



 