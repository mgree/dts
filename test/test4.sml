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
	      ord c = raise IllegalCharacter c
	  fun str2numb (nil, b) = 0 |
	      str2numb ((c::r), b) = 
	        let val ordc = ord c 
		in if ordc < b 
		       then ordc + b * str2numb (r, b)
		   else raise IllegalCharacter c
		end
	  fun str2numb_wp ("#"::"b"::l) = str2numb(l, 2) |
	      str2numb_wp ("#"::"o"::l) = str2numb(l, 8) |
	      str2numb_wp ("#"::"d"::l) = str2numb(l, 10) |
	      str2numb_wp ("#"::"x"::l) = str2numb(l, 16) |
	      str2numb_wp l = str2numb (l, b)
	  fun dispatch (FIXED s) => str2numb_wp (explode s) |
	      dispatch (VARSTR l) => str2numb_wp (map (fn x => !x) l)
      in 0
      end
  
