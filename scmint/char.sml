(*$SCHEMECHAR *)

signature SCHEMECHAR =
sig

(* CHARS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "character".

*)

(* TYPES *)

type char
type integer
sharing type integer = int and type char = string

(* NOTE: sharing char and string is too strong.  This is here to enable 
sharing of char with one-character SML strings, used in the implementation
of Scheme strings *)

(* STANDARD PROCEDURES *)

val char_eq: char * char -> bool
val char_lt: char * char -> bool
val char_gt: char * char -> bool
val char_le: char * char -> bool
val char_ge: char * char -> bool
val char_ci_eq: char * char -> bool
val char_ci_lt: char * char -> bool
val char_ci_gt: char * char -> bool
val char_ci_le: char * char -> bool
val char_ci_ge: char * char -> bool
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

(* INPUT/OUTPUT *)

(*
val read: instream -> char
val print: outstream -> char -> unit
*)

val str2char: string -> char
val char2str: char -> string

end


(*$SchemeChar: SCHEMECHAR SchemeGeneral  *)

structure SchemeChar: SCHEMECHAR =
  struct 
  local open SchemeGeneral in

  type char = string
  type integer = int

  fun char_eq (c: char, c') = (c=c') 
  fun char_lt (c: char, c') = (c < c')
  fun char_gt (c: char, c') = (c > c')
  fun char_le (c: char, c') = (c <= c')
  fun char_ge (c: char, c') = (c >= c')
  fun char_ci_eq (c, c') = raise Unimplemented "char_ci_eq"
  fun char_ci_lt (c, c') = raise Unimplemented "char_ci_lt"
  fun char_ci_gt (c, c') = raise Unimplemented "char_ci_gt"
  fun char_ci_le (c, c') = raise Unimplemented "char_ci_le"
  fun char_ci_ge (c, c') = raise Unimplemented "char_ci_ge"
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
  end
