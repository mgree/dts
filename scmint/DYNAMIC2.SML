structure  = 

  structure Boolean: BOOLEAN =
  struct

  type T = bool
  type boolean = bool

  fun eq (x: bool, y) = (x = y)
  val not = not

  end

  structure Pair: PAIR =
  struct

  type ('a, 'b) pair = 'a * 'b

  fun cons (p: ('a, 'b) pair) = p
  infix ||
  fun (f || g) (x, y) = (f x, g y)

  fun car (x, _) = x
  fun cdr (_, y) = y
  
  fun set_car ((x, _), z) = (x := z)
  fun set_cdr ((_, y), z) = (y := z)
  
  fun caar x = car (car x)
  fun cadr x = car (cdr x)
  fun cdar x = cdr (car x)
  fun cddr x = cdr (cdr x)
  fun caaar x = car (caar x)
  fun caadr x = car (cadr x)
  fun cadar x = car (cdar x)
  fun caddr x = car (cddr x)
  fun cdaar x = cdr (caar x)
  fun cdadr x = cdr (cadr x)
  fun cddar x = cdr (cdar x)
  fun cdddr x = cdr (cddr x)
  fun caaaar x = car (caaar x)
  fun caaadr x = car (caadr x)
  fun caadar x = car (cadar x)
  fun caaddr x = car (caddr x)
  fun cadaar x = car (cdaar x)
  fun cadadr x = car (cdadr x)
  fun caddar x = car (cddar x)
  fun cadddr x = car (cdddr x)
  fun cdaaar x = cdr (caaar x)
  fun cdaadr x = cdr (caadr x)
  fun cdadar x = cdr (cadar x)
  fun cdaddr x = cdr (caddr x)
  fun cddaar x = cdr (cdaar x)
  fun cddadr x = cdr (cdadr x)
  fun cdddar x = cdr (cddar x)
  fun cddddr x = cdr (cdddr x)

  fun eq (eq1, eq2) ((x1,y1), (x2,y2)) = 
      eq1 (x1,x2) andalso eq2 (y1,y2)
  end

  structure List: LIST =
  struct 
  open General

  type 'a slist = 'a list 

  fun list (l: 'a list) = l

  val is_empty = null

  val length = length
  fun append nil = nil |
      append [a] = a |
      append (nil::r) = append r |
      append ((a::l)::r) = a :: append(l::r)
  val reverse = rev
  fun list_tail (l, n) =
      let fun list_tail_checked (l, 0) = l |
	      list_tail_checked (a::r, n) = list_tail_checked (r, n-1) |
              list_tail_checked (nil, _) = 
		raise IllegalInput ("Empty list argument in list-tail", "")
      in if n < 0 
	    then raise IllegalInput ("Negative index in list-tail", makestring n)
	 else list_tail_checked (l, n)
      end
  fun list_ref (l, n) =
      let fun list_ref_checked (a::r, 0) = a |
	      list_ref_checked (a::r, n) = list_ref_checked (r, n-1) |
	      list_ref_checked (nil, _) = raise IllegalInput ("Empty list argument in list-ref", "")
      in if n < 0 then raise IllegalInput ("Negative index in list-ref", makestring n)
	 else list_ref_checked (l, n)
      end
  fun mem eq (obj, l as a :: r) = if eq (obj, a) then l else mem eq (obj, r) |
      mem eq (obj, nil) = nil
  fun ass eq (obj, (p as (a, b)) :: r) = if eq (obj, a) then Some p else
  					 ass eq (obj, r) |
      ass eq (obj, nil) = None

  fun eq eq' (nil, nil) =  true |
      eq eq' (a::r, a'::r') = eq' (a,a') andalso list_eq eq' (r,r') |
      eq eq' _ = false

  fun slist2list (l: 'a slist) = l
  end 

  structure Symbol: SYMBOL =
  struct

  type T = string
  type symbol = string

  fun eq (s: symbol, s') = (s = s')
  fun str2symbol (s: string) = s
  fun symbol2str (s: symbol) = s

  end 

  structure Number: NUMBER =
  struct 
  open General

  type number = int
  
  fun is_complex n = false
  fun is_real n = false
  fun is_rational n = false
  fun is_integer n = true
  fun is_exact n = true
  fun is_inexact n = false

  fun eq (nil: number list) = true |
      eq [a] = true |
      eq [a,b] = (a = b) |
      eq (a::b::r) = (a = b) andalso eq (b::r)
  fun lt (nil: number list) = true |
      lt [a] = true |
      lt [a,b] = (a < b) |
      lt (a::b::r) = (a < b) andalso lt (b::r)
  fun gt (nil: number list) = true |
      gt [a] = true |
      gt [a,b] = (a > b) |
      gt (a::b::r) = (a > b) andalso gt (b::r)
  fun le (nil: number list) = true |
      le [a] = true |
      le [a,b] = (a <= b) |
      le (a::b::r) = (a <= b) andalso le (b::r)
  fun ge (nil: number list) = true |
      ge [a] = true |
      ge [a,b] = (a >= b) |
      ge (a::b::r) = (a >= b) andalso ge (b::r)

  fun is_zero n = (n=0)
  fun is_positive n = (n>0)
  fun is_negative n = (n<0)
  fun is_odd n = ((n mod 2) = 1)
  fun is_even n = ((n mod 2) = 0)

  fun max (nil: number list) = raise IllegalInput ("Maximum of empty set is undefined", "") |
      max [a] = a |
      max (a::r) = let val maxr = max r in
      		   if a < maxr then maxr else a
		   end

  fun min (nil: number list) = raise IllegalInput ("Minimum of empty set is undefined", "") |
      min [a] = a |
      min (a::r) = let val minr = min r in 
                   if a > minr then minr else a
		   end
  
  fun plus (nil: number list) = 0 | (* This is not IEEE/R4 Scheme! *)
      plus [n] = n |
      plus [m,n] = m + n |
      plus (m::n::r) = m + n + plus r
  fun mult (nil: number list) = 1 | (* This is not IEEE/R4 Scheme! *)
      mult [n] = n |
      mult [m,n] = m * n |
      mult (m::n::r) = m * n * mult r
  fun minus (n, nil: number list) = ~n |
      minus (n, l) =
         let fun minus_list (m, (nil: number list)) = m |
	         minus_list (m, [a]) = m - a |
		 minus_list (m, (a::b::r)) = minus_list (m-a-b, r)
         in minus_list (n, l)
	 end
  fun divide (n, l: number list) = raise Unimplemented "divide"
  val abs = (abs: int -> int)

  fun quotient (m, n: number) = 
      m div n handle _ => raise IllegalInput ("Division by zero in quotient", "")
  fun remainder (m, n: number) = 
      if n = 0 
	  then raise IllegalInput ("Division by zero in remainder", "")
      else if (m >= 0 andalso n > 0) orelse (m < 0 andalso n < 0)
	  then m mod n
      else (* (m < 0 andalso n > 0) orelse (m >=0 andalso n < 0) *)
	  (m mod n) - n
  fun modulo (m, n: number) = 
      m mod n handle _ => raise IllegalInput ("Division by zero in modulo", "")

  fun gcd (nil: number list) = raise IllegalInput ("Gcd of empty set is undefined", "") |
      gcd [n] = abs n |
      gcd [m,n] = raise Unimplemented "gcd" |
      gcd (m::n::r) = raise Unimplemented "gcd"
  fun lcm (nil: number list) = raise IllegalInput ("Lcm of empty set is undefined", "") |
      lcm [n] = abs n |
      lcm [m,n] = raise Unimplemented "lcm" |
      lcm (m::n::r) = raise Unimplemented "lcm"

  fun floor (x: number) = x
  fun ceiling (x: number) = x
  fun truncate (x: number) = x
  fun round (x: number) = x
  fun str2number s =
      let fun xord "0" = 0 |
	      xord "1" = 1 |
	      xord "2" = 2 |
	      xord "3" = 3 |
	      xord "4" = 4 |
	      xord "5" = 5 |
	      xord "6" = 6 |
	      xord "7" = 7 |
	      xord "8" = 8 |
	      xord "9" = 9 |
	      xord "a" = 10 |
	      xord "b" = 11 |
	      xord "c" = 12 |
	      xord "d" = 13 |
	      xord "e" = 14 |
	      xord "f" = 15 |
	      xord c = raise IllegalInput ("Illegal digit in xord (this is impossible!)", c)
	  fun str2numb (nil, b) = 0 |
	      str2numb ((c::r), b) = 
	        let val xordc = xord c 
		in if xordc < b 
		       then xordc + b * str2numb (r, b)
		   else raise IllegalInput ("Illegal digit/letter in number representation", s)
		end
	  fun str2numb_wp ("#"::"b"::l) = str2numb(l, 2) |
	      str2numb_wp ("#"::"o"::l) = str2numb(l, 8) |
	      str2numb_wp ("#"::"d"::l) = str2numb(l, 10) |
	      str2numb_wp ("#"::"x"::l) = str2numb(l, 16) |
	      str2numb_wp l = str2numb (l, 10)
	  fun str2numb_ws ("+"::l) = str2numb_wp (rev l) |
	      str2numb_ws ("-"::l) = ~(str2numb_wp (rev l)) | 
	      str2numb_ws l = str2numb_wp (rev l)
      in str2numb_ws (explode s)
      end
  fun number2str n =
      let val b = 10
          fun numb2numblist (nl, n) = 
	  (* assume n nonnegative, nl accumulating parameter  *)
	      if n < b then n::nl 
	      else numb2numblist ((n mod b)::nl, n div b)
          fun char n = 
	      if 0 <= n andalso n < b then chr (n + (ord "0"))
	      else raise IllegalInput ("Illegal digit in number (this is impossible!)", makestring n)
      in if n >= 0 then implode (map char (numb2numblist ([], n)))
	 else implode ("-" :: map char (numb2numblist ([], n)))
      end
  end 

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

  structure Vector: VECTOR =
  struct 
  local open General List in

  type 'a vector = 'a list
  type natural = int
  type 'a slist = 'a slist
  
  fun make_vector (k, fill) = 
      if k < 0 then 
	  raise IllegalInput ("Negative index in make-vector", makestring k)
      else if k = 0 then nil 
	   else (* k > 0 *)   fill :: make_vector (k-1, fill)
  fun vector (l: 'a list) = l
  val vector_map = map

  val vector_length = length
  val vector_ref = nth
  fun vector_set (v, k, obj) = (nth(v,k) := obj)

  fun vector2list (v: 'a vector) = v
  fun list2vector (l: 'a list) = l

  fun vector_fill (nil, fill) = () |
      vector_fill (a::r, fill) = (a := fill ; vector_fill (r,fill))

  fun vector_eq eq (nil, nil) = true |
      vector_eq eq (a::r, a'::r') = eq (a,a') andalso vector_eq eq (r,r') |
      vector_eq eq (_,_) = false
  end
  end

  structure Control: CONTROL = 
  struct 
  local open General List in

  type ('a, 'b) procedure = 'a -> 'b
  type 'a slist = 'a slist

  infixr -->
  fun (g --> h) f x = h (f (g x))

  fun apply (p, l) = p l
  fun map1 (p, l) = map p l
  fun map2 (p, nil, nil) = nil |
      map2 (p, a::l, a'::l') = p(a,a') :: map2 (p, l, l') |
      map2 _ = raise IllegalInput ("Lists of unequal lengths in map2", "")
  fun mapn (p, nil) = raise IllegalInput ("No argument list given in map", "") | 
      mapn (p, [l]) = map1 (fn x => p [x], l) |
      mapn (p, [l,l']) = map2 (fn (x,y) => p [x,y], l, l') |
      mapn (p, (l::l'::rl)) = raise Unimplemented "mapn"
  fun for_each1 (p, l) = app p l
  fun for_each2 (p, nil, nil) = () |
      for_each2 (p, a::l, a'::l') = (p(a,a') ; for_each2 (p, l, l')) |
      for_each2 _ = raise IllegalInput ("Lists of unequal lengths in for-each2", "")
  fun for_each (p, nil) = raise IllegalInput ("No argument list given in for-each", "") |
      for_each (p, [l]) = for_each1 (fn x => p [x], l) |
      for_each (p, [l,l']) = for_each2 (fn (x,y) => p [x,y], l, l') |
      for_each (p, (l::l'::rl)) = raise Unimplemented "for_each"
  fun delay v () = v
  fun force v = v ()
  fun callcc _ = raise Unimplemented "callcc"
  fun throw _ = raise Unimplemented "throw"
  fun call_with_current_continuation p = callcc (p o throw) 
  end
  end

  structure InputOutput: INPUTOUTPUT =
  struct
  local open General Character String Control in

  type input_port = instream
  type output_port = outstream

  type char = char
  type 'a Option = 'a Option

  val open_input_file = open_in 
  val open_output_file = open_out

  fun call_with_input_file (s, p) = p (open_in s)
  fun call_with_output_file (s, p) = p (open_out s)

  fun current_input_port () = std_in (* this is wrong!! *)
  fun current_output_port () = std_out (* this is wrong!! *)
  
  fun with_input_from_file (s, p) = raise Unimplemented "with_input_from_file"
  fun with_output_to_file (s, p) = raise Unimplemented "with_output_to_file"

  val close_input_port = close_in
  val close_output_port = close_out

  fun read_char ip = 
      let val ic = input (ip, 1)
      in if ic = "" then None else Some ic
      end 

  fun peek_char ip = 
      let val ic = lookahead ip
      in if ic = "" then None else Some ic
      end
  
  fun is_eof_object None = true
    | is_eof_object (Some _) = false

  fun is_char_ready ip = raise Unimplemented "is_char_ready"

  fun newline oport = output (oport, "\n")
  fun write_char (c, oport) = output (oport, c)

  fun transcript_on ss = raise Unimplemented "transcript_on"
  fun transcript_off () = raise Unimplemented "transcript_off"
  end
  end 

  structure Dynamic: DYNAMIC =
  struct
  local 
      open General Boolean Symbol Character Vector Pair Number String List 
	   Control InputOutput 

  datatype dyn = 
      BOOL of Boolean.T |
      CHAR of Char.T |
      STRING of String.T |
      SYMBOL of Symbol.T |
      NUMBER of Number.T |
      VECTOR of dynamic Vector.T |
      PAIR of (dynamic, dynamic) Pair.T |
      PROCEDURE of (dynamic list, dynamic) Control.T |
      LIST of dynamic List.T |
      INPORT of IO.inport |
      OUTPORT of IO.outport |
      UNSPECIFIED
  withtype dynamic = dyn ref
  type T = dynamic

  fun BOOL_TAG b = ref (BOOL b)
  fun SYMBOL_TAG s = ref (SYMBOL s)
  fun CHAR_TAG c = ref (CHAR c)
  fun VECTOR_TAG v = ref (VECTOR v)
  fun PAIR_TAG p = ref (PAIR p)
  fun NUMBER_TAG n = ref (NUMBER n)
  fun STRING_TAG s = ref (STRING s)          
  fun PROCEDURE_TAG p = ref (PROCEDURE p)

  fun LIST_TAG l = ref (LIST l)
  fun INPORT_TAG iport = ref (INPORT iport)
  fun OUTPORT_TAG oport = ref (OUTPORT oport)
  fun UNSPECIFIED_TAG () = ref UNSPECIFIED

  val unspecified = ref UNSPECIFIED

  datatype TError = 
	BOOLERROR | 
	CHARERROR |
	STRINGERROR |
	SYMBOLERROR |
	NUMBERERROR |
	VECTORERROR |
	PAIRERROR |
	PROCEDUREERROR |
	LISTERROR |
	INPORTERROR |
	OUTPORTERROR |
		
  exception TypeError of TError * string * dynamic

  fun BOOL_UNTAG s (ref (BOOL b)) = b |
      BOOL_UNTAG s d = raise TypeError (BOOLERROR, s, d)
  fun SYMBOL_UNTAG x (ref (SYMBOL s)) = s |
      SYMBOL_UNTAG x d = raise TypeError (SYMBOLERROR, s, d)
  fun CHAR_UNTAG s (ref (CHAR c)) = c |
      CHAR_UNTAG s d = raise TypeError (CHARERROR, s, d)
  fun VECTOR_UNTAG s (ref (VECTOR v)) = v |
      VECTOR_UNTAG s d = raise TypeError (VECTORERROR, s, d)
  fun PAIR_UNTAG s (ref (PAIR p)) = p |
      PAIR_UNTAG s (ref (LIST (a::r))) = (a, ref (LIST r)) |
      PAIR_UNTAG s d = raise TypeError (PAIRERROR, s, d)
  fun NUMBER_UNTAG s (ref (NUMBER n)) = n |
      NUMBER_UNTAG s d = raise TypeError (NUMBERERROR, d)
  fun STRING_UNTAG x (ref (STRING s)) = s |
      STRING_UNTAG x d = raise TypeError (STRINGERROR, s, d)
  fun PROCEDURE_UNTAG s (ref (PROCEDURE p)) = p |
      PROCEDURE_UNTAG s d = raise TypeError (PROCEDUREERROR, s, d)

  fun LIST_UNTAG s (ref (LIST l)) = l |
      LIST_UNTAG s (ref (PAIR (l,r))) = l :: LIST_UNTAG s r |
      LIST_UNTAG s d = raise TypeError (LISTERROR, s, d)
  fun INPORT_UNTAG s (ref (INPORT iport)) = iport |
      INPORT_UNTAG s d = raise TypeError (INPORTERROR, d)
  fun OUTPORT_UNTAG s (ref (OUTPORT oport)) = oport |
      OUTPORT_UNTAG s d = raise TypeError (OUTPORTERROR, s, d)

  fun is_boolean (ref (BOOL b)) = true |
      is_boolean _ = false
  fun is_symbol (ref (SYMBOL s)) = true |
      is_symbol _ = false
  fun is_char (ref (CHAR c)) = true |
      is_char _ = false
  fun is_vector (ref (VECTOR v)) = true |
      is_vector _ = false
  fun is_pair (ref (PAIR p)) = true |
      is_pair (ref (LIST (x::y))) = true |
      is_pair _ = false
  fun is_number (ref (NUMBER n)) = true |
      is_number _ = false
  fun is_string (ref (STRING s)) = true |
      is_string _ = false
  fun is_procedure (ref (PROCEDURE p)) = true |
      is_procedure _ = false

  fun is_list (ref (LIST l)) = true |
      is_list (ref (PAIR (a, r))) = is_list r |
      is_list _ = false
  fun is_null (ref (LIST nil)) = true |
      is_null _ = false
  fun is_INPORT (ref (INPORT _)) = true |
      is_INPORT _ = false
  fun is_OUTPORT (ref (OUTPORT _)) = true |
      is_OUTPORT _ = false

  fun dynamic2bool (ref (BOOL b)) = b |
      dynamic2bool _ = true

  fun is_eq (ref (BOOL b), ref (BOOL b')) = boolean_eq (b, b') |
      is_eq (ref (SYMBOL s), ref (SYMBOL s')) = symbol_eq (s, s') |
      is_eq (ref (CHAR c), ref (CHAR c')) = char_eq (c, c') |
      is_eq (d as ref (VECTOR v), d' as ref (VECTOR v')) = (d=d') |
      is_eq (d as ref (PAIR p), d' as ref (PAIR p')) = (d=d') |
      is_eq (d as ref (NUMBER n), d' as ref (NUMBER n')) = (d=d') |
      is_eq (d as ref (STRING s), d' as ref (STRING s')) = (d=d') |
      is_eq (d as ref (PROCEDURE p), d' as ref (PROCEDURE p')) = (d=d') |
      is_eq (ref (LIST nil), ref (LIST nil)) = true |
      is_eq (d as ref (LIST l), d' as ref (LIST l')) = (d=d') |
      is_eq _ = false
  fun is_eqv (ref (BOOL b), ref (BOOL b')) = boolean_eq (b, b') |
      is_eqv (ref (SYMBOL s), ref (SYMBOL s')) = symbol_eq (s, s') |
      is_eqv (ref (CHAR c), ref (CHAR c')) = char_eq (c, c') |
      is_eqv (d as ref (VECTOR v), d' as ref (VECTOR v')) = (d=d') |
      is_eqv (d as ref (PAIR p), d' as ref (PAIR p')) = (d=d') |
      is_eqv (ref (NUMBER n), ref (NUMBER n')) = number_eq [n, n'] |
      is_eqv (d as ref (STRING s), d' as ref (STRING s')) = (d=d') |
      is_eqv (d as ref (PROCEDURE p), d' as ref (PROCEDURE p')) = (d=d') |
      is_eqv (ref (LIST nil), ref (LIST nil)) = true |
      is_eqv (d as ref (LIST l), d' as ref (LIST l')) = (d=d') |
      is_eqv _ = false
  fun is_equal (ref (BOOL b), ref (BOOL b')) = boolean_eq (b, b') |
      is_equal (ref (SYMBOL s), ref (SYMBOL s')) = symbol_eq (s, s') |
      is_equal (ref (CHAR c), ref (CHAR c')) = char_eq (c, c') |
      is_equal (ref (VECTOR v), ref (VECTOR v')) = 
      	vector_eq is_equal (v, v') |
      is_equal (ref (PAIR p1), ref (PAIR p2)) = 
        pair_eq (is_equal, is_equal) (p1, p2) |
      is_equal (ref (NUMBER n), ref (NUMBER n')) = number_eq [n, n'] |
      is_equal (ref (STRING s), ref (STRING s')) = string_eq (s, s') |
      is_equal (d as ref (PROCEDURE p), d' as ref (PROCEDURE p')) = (d=d') |
      is_equal (ref (LIST nil), ref (LIST nil)) = true |
      is_equal (ref (LIST l), ref (LIST l')) = list_eq is_equal (l, l') |
      is_equal (ref (PAIR (l,r)), ref (LIST (l'::r'))) =
            is_equal (l,l') andalso is_equal (r, ref (LIST r')) |
      is_equal (ref (LIST (l::r)), ref (PAIR (l',r'))) =
            is_equal (l,l') andalso is_equal (ref (LIST r), r') |
      is_equal _ = false
  
  datatype token =
      Identifier of symbol
    | BoolSym of boolean
    | NumbSym of number
    | CharSym of char
    | QuotationMark
    | LeftParen
    | RightParen
    | VectorSym
    | QuoteSym
    | QuasiquoteSym
    | UnquoteSym
    | UnqSplSym
    | DotSym
    | SemiColon
    | EndOfInput
  
  fun read ip = 
      let val read_so_far = ref ""
	  fun read_error msg =
	      (input_line ip;
	       raise IllegalInput (msg, !read_so_far))
	  fun get_next_char() =
	      let val next_char = input(ip,1)
	      in  
		 (read_so_far := !read_so_far ^ next_char;	next_char)
	      end
	  fun get_identifier() =
	      let val next_char = lookahead ip 
	      in case next_char of
		  "" => ""
		| " " => ""
		| "\t" => ""
		| "\n" => ""
		| "(" => ""
		| ")" => ""
		| "\"" => ""
		| ";" => ""
		| _ => (get_next_char() ^ get_identifier())
	      end
	  fun get_string() =
	      let val c = get_next_char() 
	      in case c of
		  "\"" => ""
		| "\\" => get_next_char() ^ get_string() 
		| _ => c ^ get_string()
	      end
	  fun get_line() = input_line ip
          fun is_token_limit c =  (is_char_whitespace c) orelse (c = ")")
	  fun get_next_token() =
	      let val next_char = get_next_char()
	      in case next_char of
		  " " => get_next_token()
		| "\t" => get_next_token()
		| "\n" => get_next_token()
		| ";" => SemiColon
		| "\"" => QuotationMark
		| "(" => LeftParen
		| ")" => RightParen
		| "`" => QuasiquoteSym
		| "'" => QuoteSym
		| "." => DotSym
		| "," => if lookahead ip = "@" then (get_next_char(); UnqSplSym)
			 else UnquoteSym
		| "#" => (case get_next_char() of
			      "(" => VectorSym
			    | "t" => BoolSym true
			    | "f" => BoolSym false
			    | "\\" => let val c = get_next_char()
				          val rem_chars = get_identifier()
				      in if rem_chars = ""
					     then CharSym c
					 else read_error "Illegal character constant"
				      end
			    | c => read_error "Illegal constant")
		| "+" => if is_token_limit (lookahead ip)
			     then Identifier (str2symbol "+") 
			 else NumbSym (str2number (get_identifier()))
		| "-" => if is_token_limit (lookahead ip) 
			     then Identifier (str2symbol "-")
			 else NumbSym (str2number ("-" ^ get_identifier()))
		| "0" => NumbSym (str2number ("0" ^ get_identifier()))
		| "1" => NumbSym (str2number ("1" ^ get_identifier()))
		| "2" => NumbSym (str2number ("2" ^ get_identifier()))
		| "3" => NumbSym (str2number ("3" ^ get_identifier()))
		| "4" => NumbSym (str2number ("4" ^ get_identifier()))
		| "5" => NumbSym (str2number ("5" ^ get_identifier()))
		| "6" => NumbSym (str2number ("6" ^ get_identifier()))
		| "7" => NumbSym (str2number ("7" ^ get_identifier()))
		| "8" => NumbSym (str2number ("8" ^ get_identifier()))
		| "9" => NumbSym (str2number ("9" ^ get_identifier()))
		| c => Identifier (str2symbol (c ^ get_identifier()))
	      end
	  fun read_datum(tok) =
	      (* parses the input stream with tok prepended as the 
	       first token *)
	      case tok of
		  Identifier s => SYMBOL_TAG s
		| BoolSym b => BOOL_TAG b 
		| NumbSym n => NUMBER_TAG n
		| CharSym c => CHAR_TAG c
		| QuotationMark => STRING_TAG (str2sstring (get_string()))
		| LeftParen => read_list()
		| VectorSym => VECTOR_TAG (read_vector())
		| QuoteSym => LIST_TAG [SYMBOL_TAG (str2symbol "quote"),
					read_datum (get_next_token())]
		| QuasiquoteSym =>
		      LIST_TAG [SYMBOL_TAG (str2symbol "quasiquote"),
				read_datum (get_next_token())]
		| UnquoteSym =>
		      LIST_TAG [SYMBOL_TAG (str2symbol "unquote"),
				read_datum (get_next_token())]
		| UnqSplSym =>
		      LIST_TAG [SYMBOL_TAG (str2symbol "unquote-splicing"),
				read_datum (get_next_token())]
		| SemiColon => (get_line(); read_datum (get_next_token()))
		| _ => read_error "Illegal input"
	  and read_list() =
	      let val tok = get_next_token()
	      in if tok = RightParen then
		  LIST_TAG nil
		 else PAIR_TAG (read_datum(tok), read_list_rem())
	      end
	  and read_list_rem() =
	      let val tok = get_next_token()
	      in case tok of
		  RightParen => LIST_TAG nil
		| DotSym => let val pd = read_datum(get_next_token())
			    in if get_next_token() = RightParen
				   then pd 
			       else read_error "Illegal input, ) expected"
			    end
		| _ => PAIR_TAG (read_datum(tok), read_list_rem())
	      end
	  and read_vector() =
	      let val tok = get_next_token()
	      in case tok of 
		  RightParen => nil
		| _ => read_datum(tok) :: read_vector()
	      end
      in
          let val tok = get_next_token() in
              if tok = EndOfInput then raise EOF
              else read_datum(tok)
          end
      end

  fun strlist2string [] = ""
  |   strlist2string [x] = x
  |   strlist2string (x::tl) = x^" "^(strlist2string tl)

  fun write (dval,oport) = 
            let
                   (* This implementation will write e.g. "he\j" as "he\j". *)
                   (* Result is not specified in std since                  *)
                   (* "he\j" is not a legal string.                         *)
               fun str_rep str = "\""^str^"\""
               fun char_rep1 "\n" = "#\\newline"
               |   char_rep1 " "  = "#\\space"
               |   char_rep1 c    = "#\\"^c
               fun is_empty_list x = if is_list x then is_null x else false
               fun rep dval =
                       let val err = fn _ =>  "()"
                               (* At the moment nothing is illegal argument *)
                               (* to write. Otherwise we would have :       *)
                               (* raise IllegalInput ("Illegal argument to  *)
                               (* write", "")                               *)
                           in
                               typecase {bool = bool_rep,
                                         symbol = symbol_rep,
                                         char = char_rep,
                                         vector = vector_rep,
                                         pair = pair_rep,
                                         number = number_rep,
                                         string = string_rep,
                                         procedure = procedure_rep,
                                         list = list_rep,
                                         INPORT = err,
                                         OUTPORT = err,
                                         otherwise = err}  dval
                           end
               and bool_rep b = boolean2str b
               and symbol_rep s = symbol2str s
               and char_rep c = char_rep1 (char2str c)
               and vector_rep v  = "#("^(strlist2string(map rep v))^")"
               and pair_rep (d1,d2) = "("^(rep d1)^(pair_rep1 d2)^")"
               and number_rep n = number2str n
               and string_rep ss = str_rep (sstring2str ss)
               and list_rep l = "("^(strlist2string(map rep l))^")"
               and pair_rep1 d = 
                             if is_empty_list d then ""
                             else 
                                if is_pair d then 
                                   let val (p1,p2) = PAIR_UNTAG "" d
                                   in
                                       " "^(rep p1)^(pair_rep1 p2)
                                   end
                                else
                                   " . "^(rep d)

                                (* Not required by std. but nice to have *)
               and procedure_rep _ = "#<PROCEDURE>"
            in
               (* This is not entirely correct, since output may not be    *)
               (* defined on an oport. But since we do not wish to use     *)
               (* the write_char function of InputOutput we use this *)
               (* function for the moment. We probably ought to have a     *)
               (* function  like write_char which writes a string ?        *)

               output(oport,rep dval) 
            end

fun display (dval,oport) = 
            let fun disp dval =
                        let val err = fn _ => 
                         raise IllegalInput ("Illegal argument to display", "")
                        in
                            typecase {bool = bool_disp,
                                      symbol = symbol_disp,
                                      char = char_disp,
                                      vector = vector_disp,
                                      pair = pair_disp,
                                      number = number_disp,
                                      string = string_disp,
                                      procedure = err,
                                      list = list_disp,
                                      INPORT = err,
                                      OUTPORT = err,
                                      otherwise = err}  dval
                        end
               and bool_disp b = boolean2str b
               and symbol_disp s = symbol2str s
               and char_disp c   = char2str c
               and vector_disp v  = "#("^(strlist2string(map disp v))^")"
               and pair_disp (d1,d2) = "("^(disp d1)^(pair_disp1 d2)^")"
               and number_disp n = number2str n
               and string_disp ss = sstring2str ss
               and list_disp l = "("^(strlist2string(map disp l))^")"
               and pair_disp1 d = 
                              if is_null d then ""
                              else 
                                 if is_pair d then 
                                    let val (p1,p2) = PAIR_UNTAG "" d
                                    in
                                        " "^(disp p1)^(pair_disp1 p2)
                                    end
                                 else
                                    " . "^(disp d)
           in
               (* This is not entirely correct, since output may not be   *)
               (* defined on an oport.                                    *)
               (* We probably ought to have a function                    *)
               (* like write_char which writes a string ?                 *)
               (* See also comment on Write.write.                  *)

               output(oport,disp dval)
           end

  end
  end

end
