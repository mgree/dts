structure String: STRING =
  struct
  open Error Object

  fun make_string (k, c: char) = 
      if k < 0 then 
	  raise InputError ("make-string", NUMBER_TAG k)
      else
	  let fun ms k = if k = 0 then nil else ref c :: ms (k-1) in
      MUTABLE (ms k)
      end
  fun string l = MUTABLE (map ref l)

  fun string_length (FIXED s) = length (explode s) |
      string_length (MUTABLE l) = length l
  fun string_ref (FIXED s, k) = nth (explode s, k) |
      string_ref (MUTABLE l, k) = 
       !(nth(l,k)) handle Nth => 
	  raise InputError ("string-ref", NUMBER_TAG k)
  fun string_set (s' as (FIXED s), k: number, c: char) = 
          raise InputError ("immutable string in string-set", STRING_TAG s') |
      string_set (MUTABLE l, k, c) = 
          (nth(l,k) := c) handle Nth => 
	      raise InputError ("negative index in string-set", NUMBER_TAG k)
  fun string_eq (FIXED s, FIXED s') = (s=s') |
      string_eq (MUTABLE nil, MUTABLE nil) = true |
      string_eq (MUTABLE (a::r), MUTABLE (a'::r')) = (!a = !a') andalso
      		string_eq (MUTABLE r, MUTABLE r') |
      string_eq (_,_) = false
  fun string_ci_eq (s, s') = raise Unimplemented "string_ci"
  fun string_lt (s, s') = raise Unimplemented "string_lt"
  fun string_gt (s, s') = raise Unimplemented "string_gt"
  fun string_le (s, s') = raise Unimplemented "string_le"
  fun string_ge (s, s') = raise Unimplemented "string_ge"
  fun string_ci_lt (s, s') = raise Unimplemented "string_ci_lt"
  fun string_ci_gt (s, s') = raise Unimplemented "string_ci_gt"
  fun string_ci_le (s, s') = raise Unimplemented "string_ci_le"
  fun string_ci_ge (s, s') = raise Unimplemented "string_ci_ge"
  fun substring (s, k, l) = raise Unimplemented "substring"
  local
  fun str_ap nil = nil |
      str_ap (FIXED s :: r) = map ref (explode s) @ str_ap r |
      str_ap (MUTABLE l :: r) = l @ str_ap r 
  in 
  fun string_append nil = MUTABLE nil |
      string_append [s] = s |
      string_append l = MUTABLE (str_ap l)
  end

  end
