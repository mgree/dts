signature STRING =
sig

(* STRINGS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "string".

*)

(* TYPES *)

type T
type sstring
sharing type T = sstring
type char
type natural
type number
type symbol
type 'a slist

(* CONSTRUCTORS *)

val make_string: natural * char -> sstring
val string: char list -> sstring

(* STANDARD PROCEDURES *)

val string_length: sstring -> natural
val string_ref: sstring * natural -> char
val string_set: sstring * natural * char -> unit
val string_eq: sstring * sstring -> bool
val string_ci_eq: sstring * sstring -> bool
val string_lt: sstring * sstring -> bool
val string_gt: sstring * sstring -> bool
val string_le: sstring * sstring -> bool
val string_ge: sstring * sstring -> bool
val string_ci_lt: sstring * sstring -> bool
val string_ci_gt: sstring * sstring -> bool
val string_ci_le: sstring * sstring -> bool
val string_ci_ge: sstring * sstring -> bool
val substring: sstring * natural * natural -> sstring
val string_append: sstring list -> sstring
val string_copy: sstring -> sstring
val string_fill: sstring * char -> unit

(* CONVERSIONS *)

val string2list: sstring -> char slist
val list2string: char slist -> sstring
val symbol2string: symbol -> sstring
val string2symbol: sstring -> symbol
val number2string: number * natural -> sstring
val string2number: sstring * natural -> number

(* INPUT/OUTPUT *)

(*
val read: instream -> sstring
val print: outstream -> sstring -> unit
*)

val str2sstring: string -> sstring
val sstring2str: sstring -> string

end


(*$SchemeString: SCHEMESTRING SchemeGeneral SchemeChar SchemeNumber
                  SchemeSymbol SchemeList *)

structure String: STRING =
  struct
  local open General Character Number Symbol List in

  datatype sstring = FIXED of string |
  		     VARSTR of char ref list

  type char = char
  type natural = int
  type number = number
  type symbol = symbol
  type 'a slist = 'a slist

  fun str2sstring s = VARSTR (map ref (explode s))
  fun sstring2str (FIXED s) = s |
      sstring2str (VARSTR l) = implode (map ! l)

  fun make_string (k, c: char) = 
      if k < 0 then 
	  raise IllegalInput ("Negative index in make-string", makestring k)
      else
	  let fun ms k = if k = 0 then nil else ref c :: ms (k-1) in
      VARSTR (ms k)
      end
  fun string l = VARSTR (map ref l)

  fun string_length (FIXED s) = length (explode s) |
      string_length (VARSTR l) = length l
  fun string_ref (FIXED s, k) = nth (explode s, k) |
      string_ref (VARSTR l, k) = 
      (!(nth(l,k)) handle Nth => 
	  raise IllegalInput ("Negative index in string-ref", makestring k))
  fun string_set (FIXED s, k: number, c: char) = 
          raise IllegalInput ("Nonupdatable string input to string-set", s) |
      string_set (VARSTR l, k, c) = 
          (nth(l,k) := c) handle Nth => 
	      raise IllegalInput ("Negative index in string-set", makestring k)
  fun string_eq (FIXED s, FIXED s') = (s=s') |
      string_eq (VARSTR nil, VARSTR nil) = true |
      string_eq (VARSTR (a::r), VARSTR (a'::r')) = char_eq(!a,!a') andalso
      		string_eq (VARSTR r, VARSTR r') |
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
      str_ap (VARSTR l :: r) = l @ str_ap r 
  in 
  fun string_append nil = VARSTR nil |
      string_append [s] = s |
      string_append l = VARSTR (str_ap l)
  end
  val string2list = explode o sstring2str
  val list2string = str2sstring o implode  
  fun string_copy (FIXED s) = VARSTR (map ref (explode s)) |
      string_copy (VARSTR l) = VARSTR (map (fn c => ref (! c)) l)
  fun string_fill (s, c) = raise Unimplemented "string_fill"

  fun symbol2string (s: symbol) = FIXED s
  fun string2symbol (FIXED s) = s |
      string2symbol (VARSTR l) = implode (map (fn x => ! x) l)
  
  fun number2string (n: number, b) =
      let fun numb2numblist (n) = 
	  (* assume n, b nonnegative *)
	  if n < b then [n] else (n mod b) :: numb2numblist (n div b)
          fun char 0 = "0" |
	      char 1 = "1" |
	      char 2 = "2" |
	      char 3 = "3" |
	      char 4 = "4" |
	      char 5 = "5" |
	      char 6 = "6" |
	      char 7 = "7" |
	      char 8 = "8" |
	      char 9 = "9" |
	      char 10 = "a" |
	      char 11 = "b" |
	      char 12 = "c" |
	      char 13 = "d" |
	      char 14 = "e" |
	      char 15 = "f" |
	      char n = raise IllegalInput ("Illegal input to char (this is impossible!)", makestring n)
      in if b = 10 orelse b = 2 orelse b = 8 orelse b = 16 
	     then if n >= 0 
		      then VARSTR (map (ref o char) (rev (numb2numblist n)))
		  else VARSTR
		      (ref "-" :: map (ref o char) (rev (numb2numblist n)))
	 else raise IllegalInput ("Illegal base in number->string", makestring b)
      end
  fun string2number (ss, b) =
      let fun ord "0" = 0 |
	      ord "1" = 1 |
	      ord "2" = 2 |
	      ord "3" = 3 |
	      ord "4" = 4 |
	      ord "5" = 5 |
	      ord "6" = 6 |
	      ord "7" = 7 |
	      ord "8" = 8 |
	      ord "9" = 9 |
	      ord "a" = 10 |
	      ord "b" = 11 |
	      ord "c" = 12 |
	      ord "d" = 13 |
	      ord "e" = 14 |
	      ord "f" = 15 |
	      ord c = raise IllegalInput ("Illegal digit/letter in string->number (this is impossible!)", c)
	  fun str2numb (nil, b) = 0 |
	      str2numb ((c::r), b) = 
	        let val ordc = ord c 
		in if ordc < b 
		       then ordc + b * str2numb (r, b)
		   else raise IllegalInput ("Illegal character in input to string->number", sstring2str ss)
		end
	  fun str2numb_wp ("#"::"b"::l) = str2numb(l, 2) |
	      str2numb_wp ("#"::"o"::l) = str2numb(l, 8) |
	      str2numb_wp ("#"::"d"::l) = str2numb(l, 10) |
	      str2numb_wp ("#"::"x"::l) = str2numb(l, 16) |
	      str2numb_wp l = str2numb (l, b)
	  fun dispatch (FIXED s) = str2numb_wp (explode s) |
	      dispatch (VARSTR l) = str2numb_wp (map ! l)
      in if b = 10 orelse b = 2 orelse b = 8 orelse b = 16 
	     then dispatch ss
         else raise IllegalInput ("Illegal base in string->number", makestring b)
      end

  end
  end
