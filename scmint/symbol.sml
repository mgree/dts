signature SYMBOL =
sig

(* SYMBOLS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "symbol".

*)

(* TYPES *)

type T
type symbol
sharing type T = symbol
  
(* STANDARD PROCEDURES *)

val eq: symbol * symbol -> bool

(* INPUT/OUTPUT *)
(*
val read_symbol: instream -> (symbol, string) Result
val parse_symbol: string -> (symbol, string) Result
val print_symbol: outstream -> symbol -> unit
val unparse_symbol: symbol -> string
*)

(* CONVERSIONS *)

val str2symbol: string -> symbol
val symbol2str: symbol -> string

end


(*$SchemeSymbol: SCHEMESYMBOL *)

structure Symbol: SYMBOL =
  struct

  type T = string
  type symbol = string

  fun eq (s: symbol, s') = (s = s')
(*
  fun parse_symbol (s as "+") = OK s
    | parse_symbol (s as "-") = OK s
    | parse_symbol (s as "...") = OK s
    | parse_symbol s =
      if exists (fn x => hd (explode s) = x) 
	  ["#", ";", "\"", ")", "(", "`", "'", ".", ",",
	   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
	  then Fail s
      else OK s
  fun read_symbol ip = parse_symbol (get_token ip)
  fun unparse_symbol s = s
  fun print_symbol op s = output (op, s)
*)
  fun str2symbol (s: string) = s
  fun symbol2str (s: symbol) = s

  end 
