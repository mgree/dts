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
    
val read_so_far = ref ""
val current_input_stream = ref std_in

fun get_next_char() =
    let val next_char = input(ip,1)
    in if next_char = "" 
	   then raise IllegalInput ("Unexpected end-of-file", !read_so_far) 
       else (read_so_far := !read_so_far ^ next_char;	next_char)
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
			       else raise IllegalInput 
				   ("Illegal character constant", "#\\" ^ c ^ rem_chars)
			    end
		  | c => raise IllegalInput ("Illegal constant", "#" ^ c ^ get_line()))
      | "+" => if is_char_whitespace (lookahead ip)
		   then Identifier (str2symbol "+") 
	       else NumbSym (str2number (get_identifier()))
      | "-" => if is_char_whitespace (lookahead ip) 
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
fun get_next_char() =
    let val next_char = input(ip,1)
    in if next_char = "" 
	   then raise IllegalInput ("Unexpected end-of-file", !read_so_far) 
       else (read_so_far := !read_so_far ^ next_char;	next_char)
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
			       else raise IllegalInput 
				   ("Illegal character constant", "#\\" ^ c ^ rem_chars)
			    end
		  | c => raise IllegalInput ("Illegal constant", "#" ^ c ^ get_line()))
      | "+" => if is_char_whitespace (lookahead ip)
		   then Identifier (str2symbol "+") 
	       else NumbSym (str2number (get_identifier()))
      | "-" => if is_char_whitespace (lookahead ip) 
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

fun read ip = 
    let val read_so_far = ref ""
	fun parse_datum(tok) =
	    (* parses the input stream with tok prepended as the 
	     first token *)
	    case tok of
		Identifier s => SYMBOL_TAG s
	      | BoolSym b => BOOL_TAG b 
	      | NumbSym n => NUMBER_TAG n
	      | CharSym c => CHAR_TAG c
		    handle IllegalCharacter _ => parse_error "Illegal character")
			 | QuotationMark => STRING_TAG (str2sstring (get_string()))
			 | LeftParen => parse_list()
			 | VectorSym => VECTOR_TAG (parse_vector())
			 | QuoteSym => LIST_TAG [SYMBOL_TAG (str2symbol "quote"),
						 parse_datum (get_next_token())]
			 | QuasiquoteSym =>
			       LIST_TAG [SYMBOL_TAG (str2symbol "quasiquote"),
					 parse_datum (get_next_token())]
			 | UnquoteSym =>
			       LIST_TAG [SYMBOL_TAG (str2symbol "unquote"),
					 parse_datum (get_next_token())]
			 | UnqSplSym =>
			       LIST_TAG [SYMBOL_TAG (str2symbol "unquote-splicing"),
					 parse_datum (get_next_token())]
			 | SemiColon => (get_line(); parse_datum (get_next_token()))
			 | _ => parse_error "Illegal input"
and parse_list() =
    let val tok = get_next_token()
    in if tok = RightParen then
	LIST_TAG nil
       else PAIR_TAG (parse_datum(tok), parse_list_rem())
    end
and parse_list_rem() =
    let val tok = get_next_token()
    in case tok of
	RightParen => LIST_TAG nil
      | DotSym => let val pd = parse_datum(get_next_token())
		  in if get_next_token() = RightParen
			 then pd 
		     else parse_error "Illegal input: ) expected"
		  end
      | _ => PAIR_TAG (parse_datum(tok), parse_list_rem())
    end
and parse_vector() =
    let val tok = get_next_token()
    in case tok of 
	RightParen => nil
      | _ => parse_datum(tok) :: parse_vector()
    end
    in parse_datum(get_next_token())
    end
