structure Environment: ENVIRONMENT =
  struct

  type env = string -> object

  exception Lookup of string
  fun empty_env k = raise Lookup k
  fun add env (k,v) string = if string=k then v else env string
  fun push env binds =
    let fun pb (nil, fvars, env) = env |
            pb ((k,b)::r, fvars, env) = 
	       let fun member nil = false
	             | member (a::b) = (a=k) orelse member b
               in 
               if member fvars 
		  then raise Lookup k
               else pb (r, k::fvars, add env (k,b))
               end
    in pb (binds, nil, env)
    end

  fun delete env k k' = if k = k' then raise Lookup k else env k'

  fun lookup env k = env k

  end
