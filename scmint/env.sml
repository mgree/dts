(*$SCHEMEENVIRONMENT *)

signature ENVIRONMENT =
sig

(* ENVIRONMENT

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for environments

*)

(* TYPES *)

eqtype key
type value
type env

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

(* CONSTRUCTORS *)

val empty_env: env
val add: env -> key * value -> env
val push: env -> (key * value) list -> env
val delete: env -> key -> env

(* OBSERVERS *)

exception Lookup of key
val lookup: env -> key -> value

end


(*$SchemeEnvironment: SCHEMEENVIRONMENT SchemeDynamic SchemeGeneral *)

structure Environment: ENVIRONMENT =
  struct
  local open General Dynamic 
  in

  type key = string
  type value = dynamic
  type env = key -> value

  exception Lookup of key
  fun empty_env k = raise Lookup k
  fun add env (k,v) key = if key=k then v else env key
  fun push env binds =
    let fun pb (nil, fvars, env) = env |
            pb ((k,b)::r, fvars, env) = 
               if member k fvars 
		  then raise Lookup k
               else pb (r, k::fvars, add env (k,b))
    in pb (binds, nil, env)
    end

  fun delete env k k' = if k = k' then raise Lookup k else env k'

  fun lookup env k = env k

  end
  end
