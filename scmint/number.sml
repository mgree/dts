structure Number: NUMBER =
  struc
  open Object Error
  fun id x = 
  fun is_complex n = tru
  fun is_real n = tru
  fun is_rational n = tr
  fun is_integer n = tr
  fun is_exact n = true
  fun is_inexact n = fals
  fun eq (z1:int, z2, l) 
      if z1 = z
         then let fun eqz nil = tru
	            | eqz (a::b) = (a = z1) andalso eqz 
              in eqz l
              en
      else fals
  fun lt (x1:int, x2, l) 
      if x1 < x2
         then let fun ltp (x, nil) = true
                    | ltp (x, (a::b)) = (x < a) andalso ltp (a, b)
              in ltp (x2, l)
              en
      else fals
  fun gt (x1:int, x2, l) =
      if x1 > x
         then let fun gtp (x, nil) = tru
                    | gtp (x, (a::b)) = x > a andalso gtp (a, b
              in gtp (x2, l)
              end
      else false
  fun le (x1:int, x2, l) 
      if x1 <= x2
         then let fun lep (x, nil) = tru
                    | lep (x, (a::b)) = x <= a andalso lep (a, b)
              in lep (x2, l)
              end
      else fal
  fun ge (x1:int, x2, l) =
      if x1 >= x2
         then let fun gep (x, nil) = true
                    | gep (x, (a::b)) = x >= a andalso gep (a, b)
              in gep (x2, l
              en
      else fals
  fun is_zero n = (n=0
  fun is_positive n = (n>0
  fun is_negative n = (n<0)
  fun is_odd n = ((n mod 2) = 1)
  fun is_even n = ((n mod 2) = 
  fun max (x:int, nil) = x
    | max (x, (a::b)) = if x < a then max (a, b) else max (x, b
  fun min (x:int, nil) = 
    | min (x, (a::b)) = if x > a then min (a, b) else min (x, b)
  loca
  fun accumulate (f, r) nil = 
    | accumulate (f, r) (a::b) = accumulate (f, f(r,a)) b
  
  val plus = accumulate (op+, 0
  val mult = accumulate (op*, 1)
  en
  fun minus (z1:int, z2) = z1 - z
  fun divide (n, n') = 
	n div n' handle _ => raise InputError ("Division by zero", UNSPECIFIED_TAG())
  val abs = (abs: int -> int
  fun quotient (m, n: number) = 
      m div n handle _ => raise InputError ("Division by zero", UNSPECIFIED_TAG()
  fun remainder (m, n: number) = 
      (if (m >= 0 andalso n > 0) orelse (m < 0 andalso n < 0)
	  then m mod n
      else (* (m < 0 andalso n > 0) orelse (m >=0 andalso n < 0) *
	  (m mod n) - n
      handle _ =>
	raise InputError ("Division by zero", UNSPECIFIED_TAG())
  fun modulo (m, n: number) 
      m mod n 
      handle _ =
	raise InputError ("Division by zero", UNSPECIFIED_TAG()
  fun gcd nil = 0 
      gcd [n] = abs n 
      gcd _ = raise Unimplemented "gc
  fun lcm nil = 1 |
      lcm [n] = abs n |
      lcm _ = raise Unimplemented "lc
  val numerator = i
  fun denominator r = 1
  val floor = i
  val ceiling = id
  val truncate = i
  val round = id
  fun rationalize (x1, x2) = raise Unimplemented "rationaliz
  fun exp z = raise Unimplemented "exp" 
  fun log z = raise Unimplemented "log" 
  fun sin z = raise Unimplemented "sin
  fun cos z = raise Unimplemented "cos
  fun tan z = raise Unimplemented "tan" 
  fun asin z = raise Unimplemented "asin
  fun acos z = raise Unimplemented "acos
  fun atan z = raise Unimplemented "atan
  fun atanr (x1, x2) = raise Unimplemented "atanr
  fun sqrt z = raise Unimplemented "sqrt" 
  fun expt (z1, z2) = raise Unimplemented "exp
  fun make_rectangular (x1, x2) = raise Unimplemented "make-rectangular" 
  fun make_polar (x1, x2) = raise Unimplemented "make-polar" 
  fun real_part z = raise Unimplemented "real-part" 
  fun imag_part z = raise Unimplemented "imag-part
  fun magnitude z = raise Unimplemented "magnitude" 
  fun angle z = raise Unimplemented "angle
  fun exact2inexact z = 
	raise InputError ("Cannot produce inexact numbers", NUMBER_TAG z)
  val inexact2exact = i
  fun NATURAL2RADIX 2 = 2
    | NATURAL2RADIX 4 = 4
    | NATURAL2RADIX 8 = 8
    | NATURAL2RADIX 10 = 10
    | NATURAL2RADIX 16 = 1
    | NATURAL2RADIX n = raise TypeError ("radix", NUMBER_TAG n)

  fun number2string (n: number, b: radix) 
      let fun numb2numblist (n
	      if n < b then [n] else (n mod b) :: numb2numblist (n div b)
          fun char 0 = "0" |
	      char 1 = "1" |
	      char 2 = "2" 
	      char 3 = "3" 
	      char 4 = "4" 
	      char 5 = "5" 
	      char 6 = "6" 
	      char 7 = "7" 
	      char 8 = "8
	      char 9 = "9" 
	      char 10 = "a" 
	      char 11 = "b" |
	      char 12 = "c
	      char 13 = "d
	      char 14 = "e" 
	      char 15 = "f
	      char n = raise Impossible "number->string
      in if n >= 
	    then implode (map char (rev (numb2numblist n))
         else implode ("-" :: map char (rev (numb2numblist (~n))
      en
  fun string2number (s: string, b: radix) =
      let exception Fail
          fun ord "0" = 
	      ord "1" = 1 
	      ord "2" = 
	      ord "3" = 3 |
	      ord "4" = 4 |
	      ord "5" = 
	      ord "6" = 
	      ord "7" = 
	      ord "8" = 8 |
	      ord "9" = 
	      ord "a" = 10 
	      ord "b" = 11 |
	      ord "c" = 12 
	      ord "d" = 13 
	      ord "e" = 14 
	      ord "f" = 1
	      ord c = raise Fai
	  fun str2numb (nil, b) = 0 
	      str2numb ((c::r), b) = 
	        let val ordc = ord 
		in if ordc < 
		       then ordc + b * str2numb (r, 
		   else raise Fai
		en
          fun str2numb_s ("+"::l, n) = str2numb (rev l, n
	      str2numb_s ("-"::l, n) = ~(str2numb (rev l, n)
	      str2numb_s (l, n) = str2numb (rev l, n)
	  fun str2numb_wp ("#"::"b"::l) = str2numb_s(l, 2) 
	      str2numb_wp ("#"::"o"::l) = str2numb_s(l, 8) |
	      str2numb_wp ("#"::"d"::l) = str2numb_s(l, 10) 
	      str2numb_wp ("#"::"x"::l) = str2numb_s(l, 16) 
	      str2numb_wp l = str2numb_s (l, b)
      in Some (str2numb_wp (explode s)) handle Fail => No
      e
    fun str2number s 
	case string2number (s, 10) o
	  Some n => 
        | None => raise Impossible "str2number"
    fun number2str n = number2string (n, 10)
  end 
