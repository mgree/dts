structure Environment: ENVIRONMENT =
  struct

  type env = string -> object

  exception Lookup of string
  fun empty_env k = raise Lookup k
  fun add env (k,v) string = if string=k then v else env string
  fun push env binds =
    let fun pb (nil, fvars, env) = env |
            pb ((k,b)::r, fvars, env) 
	       let fun member nil = false
	             | member (a::b) = (a=k) orelse member b
               i
               if member fvar
		  then raise Lookup 
               else pb (r, k::fvars, add env (k,b
               en
    in pb (binds, nil, env
    e
  fun delete env k k' = if k = k' then raise Lookup k else env k'

  fun lookup env k = en
  e
