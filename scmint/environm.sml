(*$SCHEMEENVIRONMENT *)

signature SCHEMEENVIRONMENT =
sig

(* ENVIRONMENT

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for environments, including top-level environment

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

val init_top_env: unit -> unit
val top_env: env ref
val empty_env: env
val add: key * value * env -> env
val add_top: key * value -> unit
val push: (key * value) list * env -> env
val push_top: (key * value) list -> unit

(* OBSERVERS *)

val lookup: key * env -> value
val lookup_top: key -> value

end


(*$SchemeEnvironment: SCHEMEENVIRONMENT SchemeDynamic SchemeGeneral *)

structure SchemeEnvironment: SCHEMEENVIRONMENT =
  struct
  local open SchemeGeneral SchemeDynamic 
  in

  type key = string
  type value = dynamic
  type env = key -> value

  exception Lookup
  fun empty_env k = raise Lookup
  fun add(k,v,env) key = if key=k then v else env key
  fun push(binds, env) =
    let fun pb (nil, fvars, env) = env |
            pb ((k,b)::r, fvars, env) = 
	      let fun mem (v, nil) = false |
	              mem (v, a::r) = (a=v) orelse mem (v,r)
              in if mem (k, fvars) 
		     then raise IllegalInput ("Identifier doubly declared", k)
                 else pb (r, k::fvars, add(k,b,env))
              end
    in pb (binds, nil, env)
    end
 
  val top_env: env ref = ref empty_env 
  fun init_top_env () = top_env := empty_env
  fun add_top (k,v) = (top_env := add(k,v,!top_env))
  fun push_top binds = (top_env := push(binds, !top_env))

  fun lookup_top key = (!top_env key 
      handle Lookup => raise IllegalInput ("Identifier not found in environment", key))
  fun lookup (key, f: env) = f key handle Lookup => lookup_top key

  end
  end
