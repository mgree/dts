structure Char: CHAR =
  struct 
  open Error

  val unspec_char = "\t"
  val eof_char = ""

  fun char_eq (c: char, c') = (c=c') 
  fun char_lt (c: char, c') = (c < c')
  fun char_gt (c: char, c') = (c > c')
  fun char_le (c: char, c') = (c <= c')
  fun char_ge (c: char, c') = (c >= c')
  fun char_ci_eq (c, c') = raise Unimplemented "ci_eq"
  fun char_ci_lt (c, c') = raise Unimplemented "ci_lt"
  fun char_ci_gt (c, c') = raise Unimplemented "ci_gt"
  fun char_ci_le (c, c') = raise Unimplemented "ci_le"
  fun char_ci_ge (c, c') = raise Unimplemented "ci_ge"
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

  end
