structure Environment =
  struct

  type 'a env = (string * 'a) list

  val empty_env = []

  fun make_env y = [y]

  fun add (e,e') = e' @ e

  fun extend (e,s,v) = (s,v) :: e

  exception Lookup 
  fun lookup s [] = raise Lookup
    | lookup s ((s',v)::r) = 
         if s = s' then v else lookup s r

  end



 