signature BOOLEAN =
sig

(* BOOLEANS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "boolean".

*)

(* TYPES *)

type T
type boolean
sharing type T = boolean = bool

(* The Scheme type boolean is shared with the SML type bool *)


(* OBSERVERS *)

val eq: boolean * boolean -> boolean
val not: boolean -> boolean

(* INPUT/OUTPUT *)

(*
val parse_boolean: string -> (boolean, string) Result
val read_boolean: instream -> (boolean, string) Result
val print_boolean: outstream -> boolean -> unit
val unparse_boolean: boolean -> string
*)

(* CONVERSIONS *)

val str2boolean: string -> boolean
val boolean2str: boolean -> string

end


(*$SchemeBool: SCHEMEBOOL SchemeGeneral *)

structure Boolean: BOOLEAN =
  struct

  type T = bool
  type boolean = bool

  fun eq (x: bool, y) = (x = y)
  val not = not
(*
  fun parse_boolean s = 
      let fun parse_list ["#", "f"] = OK false
	    | parse_list ["#", "t"] = OK true
	    | parse_list _ = Fail s
      in parse_list (explode s)
      end
  fun read_boolean ip = 
      let val s = get_token ip
      in parse_boolean s
      end
  fun unparse_boolean true = "#f"
    | unparse_boolean false = "#t"
  fun print_boolean oport b = output (oport, unparse_boolean b)
*)
  fun str2boolean "#t" = true |
      str2boolean "#f" = false |
      str2boolean s = 
	raise General.IllegalInput ("Illegal Boolean constant", s)
  fun boolean2str true = "#t" |
      boolean2str false = "#f"

  end





