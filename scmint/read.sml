datatype token =
    Identifier of string
  | BoolSym of string
  | NumbSym of string
  | CharSym of string
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
  | IllegalToken of string

val read_so_far = ref ""

val ip = std_in

exception InputError of string

fun parse_error s = 
   let val t = input_line ip
   in raise InputError (s ^ ": " ^ !read_so_far ^ "\n" ^ t ^ " discarded")
   end

fun get_next_char() =
    let val next_char = input(ip,1)
    in if next_char = "" 
	   then parse_error "Unexpected end of input"
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
      | "\\" => let val d = get_next_char() 
		in case d of 
		    "\"" => "\""
                  | "\\" => "\\"
		  | _ => parse_error "Illegal string"
		end
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
		  | "\\" => CharSym (get_identifier())
		  | c => BoolSym ("#" ^ c ^ get_identifier()))
      | "+" => if is_char_whitespace (lookahead ip) then
	    Identifier "+" 
	    else NumbSym ("+" ^ get_identifier())
      | "-" => if is_char_whitespace (lookahead ip) then
	    Identifier "-"
	    else NumbSym ("-" ^ get_identifier())
      | "0" => NumbSym ("0" ^ get_identifier())
      | "1" => NumbSym ("1" ^ get_identifier())
      | "2" => NumbSym ("2" ^ get_identifier())
      | "3" => NumbSym ("3" ^ get_identifier())
      | "4" => NumbSym ("4" ^ get_identifier())
      | "5" => NumbSym ("5" ^ get_identifier())
      | "6" => NumbSym ("6" ^ get_identifier())
      | "7" => NumbSym ("7" ^ get_identifier())
      | "8" => NumbSym ("8" ^ get_identifier())
      | "9" => NumbSym ("9" ^ get_identifier())
      | "@" => IllegalToken next_char
      | "[" => IllegalToken next_char
      | "]" => IllegalToken next_char
      | "{" => IllegalToken next_char
      | "}" => IllegalToken next_char
      | "|" => IllegalToken next_char
      | "\\" => IllegalToken next_char
      | c => Identifier (c^ get_identifier())
    end

fun parse_datum(tok) =
    (* parses the input stream with tok prepended as the first token *)
    case tok of
	Identifier s => SYMBOL_TAG (str2symbol s)
      | BoolSym s => BOOL_TAG (str2bool s)
      | NumbSym s => NUMBER_TAG (str2number s)
      | CharSym s => CHAR_TAG (str2char s)
      | QuotationMark => STRING_TAG (str2sstring (get_string()))
      | LeftParen => parse_list()
      | VectorSym => VECTOR_TAG (parse_vector())
      | QuoteSym => LIST_TAG [SYMBOL_TAG (str2symbol "quote"),
			      parse_datum (get_next_token())]
      | QuasiquoteSym => LIST_TAG [SYMBOL_TAG (str2symbol "quasiquote"),
				   parse_datum (get_next_token())]
      | UnquoteSym => LIST_TAG [SYMBOL_TAG (str2symbol "unquote"),
				parse_datum (get_next_token())]
      | UnqSplSym => LIST_TAG [SYMBOL_TAG (str2symbol "unquote-splicing"),
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
and read() = (read_so_far := ""; parse_datum(get_next_token()))

