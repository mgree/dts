  datatype 'a Option = None | Some of 'a
  datatype ('a, 'b) Result = OK of 'a | Fail of 'b
  type natural = int

  exception Unimplemented of string
  exception IllegalInput of string * string 
  exception EOF
  exception EXIT

  fun apply f [] = ()
    | apply f (a::r) = (f a; apply f r)

  fun zip ([], []) = []
    | zip (a::r, a'::r') = (a,a')::zip (r,r')
    | zip (_, _) = raise IllegalInput ("zip", "lists of different lengths")

  fun foldappend ls = fold op@ ls nil

  fun member x [] = false
    | member x (y::tl) = if x=y then true else (member x tl)

