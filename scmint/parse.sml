type char = string
    
datatype Token = 
    EndOfInput
  | SemiColon
  | QuotationMark
  | LeftParen
  | RightParen
  | QuasiquoteSym
  | QuoteSym
  | DotSym
  | UnqSplSym
  | UnquoteSym
  | VectorSym
  | BoolSym of bool
  | CharSym of char
  | NumbSym of number * string
  | Identifier of string
    
fun read_char ip = input(ip,1)
val peek_char = lookahead
    
fun read_identifier ip =
    let val next_char = peek_char ip
    in case next_char of
	"" => ""
      | " " => ""
      | "\t" => ""
      | "\n" => ""
      | "(" => ""
      | ")" => ""
      | "\"" => ""
      | ";" => ""
      | _ => read_char ip ^ read_identifier ip
    end

val read_line = input_line
   
datatype exactness = UNSPEC | EXACT | INEXACT

fun str2number s =
    let val sl = ref (explode s)
	exception NaN
	fun nextchar() = 
	    if !sl = nil then ""
	    else let val res = hd !sl in sl := tl (!sl); res end
	fun peekchar() =
	    if !sl = nil then ""
	    else hd !sl
	val exact = ref UNSPEC
	val base = ref 10
	fun number() = complex(prefix())
	and prefix() =
	    let val c = nextchar()  
	    in if c = "#" 
		   then ((case nextchar() of
			     "i" => (exact := INEXACT; radix())
			   | "e" => (exact := EXACT; radix())
			   | "b" => (base := 2; exactness())
			   | "o" => (base := 8; exactness())
			   | "d" => (base := 10; exactness())
			   | "x" => (base := 16; exactness())
			   | _ => raise NaN);
		       nextchar())
	       else c 
	    end
	and exactness() = 
	    let val c = nextchar()
	    in if c = "#" 
		   then ((case nextchar() of
			     "i" => exact := INEXACT
			   | "e" => exact := EXACT
			   | _ => raise NaN);
			 nextchar())
	       else c
	    end
	and radix() =
	    let val c = nextchar()
            in if c = "#"
		   then ((case nextchar() of
			     "b" => radix := 2
			   | "o" => radix := 8
			   | "d" => radix := 10
			   | "x" => radix := 16
			   | _ => raise NaN);
			 nextchar())
	       else c
	    end
	and complex c =
	    case c of
		"+" => let val c' = nextchar()
                       in if c' = "i" 
			      then if nextchar() = ""
				       then COMPLEX (0.0,1.0)
				   else raise NaN
			  else let val r = ureal "+" c'
				   val c'' = nextchar()
			       in case c'' of 
				   "" => r
				 | "@" => raise Unimplemented 
				       "Polar coordinate recognition"
				 | "+" => let val c''' = nextchar()
					  in if c''' = "i" and peekchar() = ""
					      then COMPLEX (toreal r, 1.0)
					     else let val r' = ureal "+" c'''
						  in if nextchar() = "i" and nextchar() = ""
							 then COMPLEX (toreal r, toreal r')
						     else raise NaN
						  end
					  end
				 | "-" => let val c''' = nextchar()
					  in if c''' = "i" and peekchar() = ""
					      then COMPLEX (toreal r, ~1.0)
					     else let val r' = ureal "-" c'''
						  in if nextchar() = "i" and nextchar() = ""
							 then COMPLEX (toreal r, toreal r')
						     else raise NaN
						  end
					  end
				 | "i" => if nextchar() = ""
					      then COMPLEX (0.0, toreal r)
					  else raise NaN
				 | _ => raise NaN
			       end
                       end
	      | "-" => let val c' = nextchar()
                       in if c' = "i" 
			      then if nextchar() = ""
				       then COMPLEX (0.0,~1.0)
				   else raise NaN
			  else let val r = ureal "-" c'
				   val c'' = nextchar()
			       in case c'' of 
				   "" => r
				 | "@" => raise Unimplemented 
				       "Polar coordinate recognition"
				 | "+" => let val c''' = nextchar()
					  in if c''' = "i" and peekchar() = ""
					      then COMPLEX (toreal r, 1.0)
					     else let val r' = ureal "+" c'''
						  in if nextchar() = "i" and nextchar() = ""
							 then COMPLEX (toreal r, toreal r')
						     else raise NaN
						  end
					  end
				 | "-" => let val c''' = nextchar()
					  in if c''' = "i" and peekchar() = ""
					      then COMPLEX (toreal r, ~1.0)
					     else let val r' = ureal "-" c'''
						  in if nextchar() = "i" and nextchar() = ""
							 then COMPLEX (toreal r, toreal r')
						     else raise NaN
						  end
					  end
				 | "i" => if nextchar() = ""
					      then COMPLEX (0.0, toreal r)
					  else raise NaN
				 | _ => raise NaN
			       end
                       end
	      | _ => let val r = ureal "+" c
			 val c'' = nextchar()
		     in case c'' of 
			 "" => r
		       | "@" => raise Unimplemented "Polar coordinate recognition"
		       | "+" => let val c''' = nextchar()
				in if c''' = "i" and peekchar() = ""
				       then COMPLEX (toreal r, 1.0)
				   else let val r' = ureal "+" c'''
					in if nextchar() = "i" and nextchar() = ""
					       then COMPLEX (toreal r, toreal r')
					   else raise NaN
					end
				end
		       | "-" => let val c''' = nextchar()
				in if c''' = "i" and peekchar() = ""
				       then COMPLEX (toreal r, ~1.0)
				   else let val r' = ureal "-" c'''
					in if nextchar() = "i" and nextchar() = ""
					       then COMPLEX (toreal r, toreal r')
					   else raise NaN
					end
				end
		       | "i" => if nextchar() = ""
				    then COMPLEX (0.0, toreal r)
				else raise NaN
		       | _ => raise NaN
		     end
	and ureal sign c =
	    if c = "." then raise Unimplemented "Decimal constants"
	    else let val v = uinteger c
		     val c' = nextchar()
		 in case c' of
		     "" => if sign = "+" then
			 INTEGER v 
			   else INTEGER ~v
		   |  "." => raise Unimplemented "Decimal constants"
		   | "/" => let val v' = uinteger (nextchar())
			    in if nextchar() = "" 
				then if sign = "+" then
				    REAL (real v / real v')
				     else REAL ~(real v / real v')
			       else raise NaN
			    end 
		   | "e" => raise Unimplemented "Exponent constants"
		   | "s" => raise Unimplemented "Exponent constants"
		   | "f" => raise Unimplemented "Exponent constants"
		   | "d" => raise Unimplemented "Exponent constants"
		   | "l" => raise Unimplemented "Exponent constants"
		   | _ => raise NaN
		 end
	and uinteger c = 0
    in 5
    end

fun read_token ip =
    let val next_char = read_char ip
    in case next_char of
	"" => EndOfInput
      | " " => read_token ip
      | "\t" => read_token ip
      | "\n" => read_token ip
      | ";" => SemiColon
      | "\"" => QuotationMark
      | "(" => LeftParen
      | ")" => RightParen
      | "`" => QuasiquoteSym
      | "'" => QuoteSym
      | "." => DotSym
      | "," => if peek_char ip = "@" then (read_char ip; UnqSplSym)
	       else UnquoteSym
      | "#" => (case read_char ip of
		    "(" => VectorSym
		  | "t" => BoolSym true
		  | "f" => BoolSym false
		  | "\\" => let val c' = read_char ip
				val rem_chars = read_identifier ip
			    in (* if c' = "" then raise IllegalInput 
				("Unexpected end-of-file", "#\\")
				else *)
				if rem_chars = ""
				    then CharSym c'
				else (case c' ^ rem_chars of
					  "space" => CharSym " "
					| "newline" => CharSym "\n"
					| "tab" => CharSym "\t"
					| _ =>  raise IllegalInput 
					      ("Illegal #\\ object", "#\\" ^ c' ^ rem_chars))
			    end
		  | c => let val s = "#" ^ c ^ read_identifier ip
			 in if number_rep s
				then Number s
			    else raise IllegalInput
				("Illegal # object", s)
			 end)
      | c => let val s = c ^ read_identifier ip
	     in case str2number s of
		 None => Identifier s
	       | Some n => Number (n, s)
	     end
    end





