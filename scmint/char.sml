signature CHARACTER =
sig

(* CHARS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "character".

*)

(* TYPES *)

type T
type char
sharing type char = T


(* STANDARD PROCEDURES *)

val eq: char * char -> bool
val lt: char * char -> bool
val gt: char * char -> bool
val le: char * char -> bool
val ge: char * char -> bool
val ci_eq: char * char -> bool
val ci_lt: char * char -> bool
val ci_gt: char * char -> bool
val ci_le: char * char -> bool
val ci_ge: char * char -> bool
val is_char_alphabetic: char -> bool
val is_char_numeric: char -> bool
val is_char_whitespace: char -> bool
val is_char_upper_case: char -> bool
val is_char_lower_case: char -> bool
val char_upcase: char -> char
val char_downcase: char -> char

(* CONVERSIONS *)

val char2integer: char -> integer
val integer2char: integer -> char
val str2char: string -> char
val char2str: char -> string

(* INPUT/OUTPUT *)

(*
val read: instream -> char
val print: outstream -> char -> unit
*)

end


(*$SchemeChar: SCHEMECHAR SchemeGeneral  *)

structure Character: CHARACTER =
  struct 
  open General

  type char = string
  type integer = int

  fun eq (c: char, c') = (c=c') 
  fun lt (c: char, c') = (c < c')
  fun gt (c: char, c') = (c > c')
  fun le (c: char, c') = (c <= c')
  fun ge (c: char, c') = (c >= c')
  fun ci_eq (c, c') = raise Unimplemented "ci_eq"
  fun ci_lt (c, c') = raise Unimplemented "ci_lt"
  fun ci_gt (c, c') = raise Unimplemented "ci_gt"
  fun ci_le (c, c') = raise Unimplemented "ci_le"
  fun ci_ge (c, c') = raise Unimplemented "ci_ge"
  fun is_char_alphabetic c = (("a" <= c andalso c <= "z") 
			      orelse
  			      ("A" <= c andalso c <= "Z"))
  fun is_char_numeric c = ("0" <= c andalso c <= "9")
  fun is_char_whitespace c = (c = "\n" orelse c = "\t" orelse c = " ")
  fun is_char_upper_case c = ("A" <= c andalso c <= "Z")
  fun is_char_lower_case c = ("a" <= c andalso c <= "z")
  val char2integer = ord
  val integer2char = chr
  fun char_upcase c = if ("a" <= c andalso c <= "z") then
                         chr (ord c + ord "A" - ord "a")
		      else c
  fun char_downcase c = if ("A" <= c andalso c <= "Z") then
                         chr (ord c + ord "a" - ord "A")
		      else c

  fun str2char "newline" = "\n" |
      str2char "tab" = "\t" |
      str2char "space" = " " |
      str2char s = 
         if size s = 1 then s else raise IllegalInput ("Illegal character", s)
  fun char2str "\n" = "#\\newline" |
      char2str "\t" = "#\\tab" |
      char2str " " = "#\\space" |
      char2str c = "#\\" ^ c

  end
