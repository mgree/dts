(*$SCHEMENUMBER *)

signature SCHEMENUMBER =
sig

(* NUMBERS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Standard procedures for Scheme type "number".
Caveat: SML integers are used exclusively for numbers!

*)

(* TYPES *)

type number

(* STANDARD PROCEDURES *)

val is_complex: number -> bool
val is_real: number -> bool
val is_rational: number -> bool
val is_integer: number -> bool
val is_exact: number -> bool
val is_inexact: number -> bool

val number_eq: number list -> bool
val number_lt: number list -> bool
val number_gt: number list -> bool
val number_le: number list -> bool
val number_ge: number list -> bool

val is_zero: number -> bool
val is_positive: number -> bool
val is_negative: number -> bool
val is_odd: number -> bool
val is_even: number -> bool

val max: number list -> number
val min: number list -> number

val plus: number list -> number
val mult: number list -> number
val minus: number * number list -> number
val divide: number * number list -> number
val abs: number -> number

val quotient: number * number -> number
val remainder: number * number -> number
val modulo: number * number -> number

val gcd: number list -> number
val lcm: number list -> number

val floor: number -> number
val ceiling: number -> number
val truncate: number -> number
val round: number -> number

(* INPUT/OUTPUT *)

(*
val read_number: instream -> (number, string) Result
val parse_number: string -> (number, string) Result
val print_number: outstream -> number -> unit
val unparse_number: number -> string
*)

val str2number: string -> number
val number2str: number -> string

end


(*$SchemeNumber: SCHEMENUMBER SchemeGeneral *)

structure SchemeNumber: SCHEMENUMBER =
  struct 
  local open SchemeGeneral in

  type number = int
  
  fun is_complex n = false
  fun is_real n = false
  fun is_rational n = false
  fun is_integer n = true
  fun is_exact n = true
  fun is_inexact n = false

  fun number_eq (nil: number list) = true |
      number_eq [a] = true |
      number_eq [a,b] = (a = b) |
      number_eq (a::b::r) = (a = b) andalso number_eq (b::r)
  fun number_lt (nil: number list) = true |
      number_lt [a] = true |
      number_lt [a,b] = (a < b) |
      number_lt (a::b::r) = (a < b) andalso number_lt (b::r)
  fun number_gt (nil: number list) = true |
      number_gt [a] = true |
      number_gt [a,b] = (a > b) |
      number_gt (a::b::r) = (a > b) andalso number_gt (b::r)
  fun number_le (nil: number list) = true |
      number_le [a] = true |
      number_le [a,b] = (a <= b) |
      number_le (a::b::r) = (a <= b) andalso number_le (b::r)
  fun number_ge (nil: number list) = true |
      number_ge [a] = true |
      number_ge [a,b] = (a >= b) |
      number_ge (a::b::r) = (a >= b) andalso number_ge (b::r)

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
(*
  fun parse_number s =
      let fun parse_base_list b nil = OK 0 (* NOT DONE! *)
	  fun parse_radix_list ("#"::"b"::sl) = parse_base_list 2 sl
	    | parse_radix_list ("#"::"o"::sl) = parse_base_list 8 sl
	    | parse_radix_list ("#"::"d"::sl) = parse_base_list 10 sl
	    | parse_radix_list ("#"::"x"::sl) = parse_base_list 16 sl
	    | parse_radix_list sl = parse_base_list 10 sl
	  fun parse_number_list ("#"::"i"::sl) = INEXACT (parse_radix_list sl)
	    | parse_number_list ("#"::"e"::sl) = parse_radix_list sl
	    | parse_number_list ("#"::"b"::"#"::"i"::sl) =
	      INEXACT (parse_base_list 2 sl)
	    | parse_number_list ("#"::"b"::"#"::"e"::sl) =
	      parse_base_list 2 sl
	    | parse_number_list ("#"::"o"::"#"::"i"::sl) =
	      INEXACT (parse_base_list 8 sl)
	    | parse_number_list ("#"::"o"::"#"::"e"::sl) =
	      parse_base_list 8 sl
	    | parse_number_list ("#"::"d"::"#"::"i"::sl) =
	      INEXACT (parse_base_list 10 sl)
	    | parse_number_list ("#"::"d"::"#"::"e"::sl) =
	      parse_base_list 10 sl
	    | parse_number_list ("#"::"x"::"#"::"i"::sl) =
	      INEXACT (parse_base_list 16 sl)
	    | parse_number_list ("#"::"x"::"#"::"e"::sl) =
	      parse_base_list 16 sl
	    | parse_number_list sl = parse_radix_list sl
      in parse_number_list (explode s)
      end
*)
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
  end 
