structure IO: IO =
  struct
  open Error Object

  val input_port_setting = ref std_in
  val output_port_setting = ref std_out
  fun set_input_port iport = (input_port_setting := iport)
  fun set_output_port oport = (output_port_setting := oport)

  fun call_with_input_file (s, p) = p [INPUT_PORT_TAG (open_in s)]
  fun call_with_output_file (s, p) = p [OUTPUT_PORT_TAG (open_out s)]
  fun current_input_port () = !input_port_setting
  fun current_output_port () = !output_port_setting
  val open_input_file = open_in 
  val open_output_file = open_out
  val close_input_port = close_in
  val close_output_port = close_out
  fun read_char ip = input (ip, 1)
  val read_char_current = read_char o current_input_port
  val peek_char = lookahead
  val peek_char_current = peek_char o current_input_port
  fun newline oport = output (oport, "\n")
  val newline_current = newline o current_output_port
  fun write_char (c, oport) = output (oport, c)
  fun write_char_current c = write_char (c, current_output_port())

  local 
  open Char Symbol Number SVector

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
  fun str2number s = string2number (s, 10)
  fun number2str n = number2string (n, 10)
  fun char2str "\n" = "#\\newline" |
      char2str "\t" = "#\\tab" |
      char2str " " = "#\\space" |
      char2str c = "#\\" ^ c
  fun boolean2str true = "#t" |
      boolean2str false = "#f"
  fun typecase {bool = bool_fcn,
		symbol = symbol_fcn,
		char = char_fcn,
		vector = vector_fcn,
		pair = pair_fcn,
		number = number_fcn,
		string = string_fcn,
		procedure = procedure_fcn,
		list = list_fcn,
		input_port = input_port_fcn,
		output_port = output_port_fcn,
		otherwise = otherwise_fcn} dval =
      case dval of
	  ref (BOOL b) => bool_fcn b
        | ref (SYMBOL s) => symbol_fcn s
	| ref (CHAR c) => char_fcn c
	| ref (VECTOR v) => vector_fcn v
	| ref (PAIR p) => pair_fcn p
	| ref (NUMBER n) => number_fcn n
	| ref (STRING s) => string_fcn s
	| ref (PROCEDURE p) => procedure_fcn p
	| ref (LIST l) => list_fcn l
	| ref (INPUT_PORT iport) => input_port_fcn iport
	| ref (OUTPUT_PORT oport) => output_port_fcn oport
	| dval => otherwise_fcn dval
  in
  fun read ip = 
      let val read_so_far = ref ""
	  fun read_error msg =
	      (input_line ip;
	       raise IOError (msg, !read_so_far))
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
			     then Identifier (string2symbol "+") 
			 else NumbSym (str2number (get_identifier()))
		| "-" => if is_token_limit (lookahead ip) 
			     then Identifier (string2symbol "-")
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
		| c => Identifier (string2symbol (c ^ get_identifier()))
	      end
	  fun read_datum(tok) =
	      (* parses the input stream with tok prepended as the 
	       first token *)
	      case tok of
		  Identifier s => SYMBOL_TAG s
		| BoolSym b => BOOL_TAG b 
		| NumbSym n => NUMBER_TAG n
		| CharSym c => CHAR_TAG c
		| QuotationMark => STRING_TAG (FIXED (get_string()))
		| LeftParen => read_list()
		| VectorSym => VECTOR_TAG (vector (read_vector()))
		| QuoteSym => LIST_TAG [SYMBOL_TAG (string2symbol "quote"),
					read_datum (get_next_token())]
		| QuasiquoteSym =>
		      LIST_TAG [SYMBOL_TAG (string2symbol "quasiquote"),
				read_datum (get_next_token())]
		| UnquoteSym =>
		      LIST_TAG [SYMBOL_TAG (string2symbol "unquote"),
				read_datum (get_next_token())]
		| UnqSplSym =>
		      LIST_TAG [SYMBOL_TAG (string2symbol "unquote-splicing"),
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
              if tok = EndOfInput then CHAR_TAG eof_char
              else read_datum(tok)
          end
      end
  val read_current = read o current_input_port

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
                               typecase {bool = boolean2str,
                                         symbol = symbol2string,
                                         char = char2str,
                                         vector = vector_rep,
                                         pair = pair_rep,
                                         number = number_rep,
                                         string = string_rep,
                                         procedure = procedure_rep,
                                         list = list_rep,
                                         input_port = err,
                                         output_port = err,
                                         otherwise = err}  dval
                           end
               and bool_rep b = boolean2str b
               and symbol_rep s = symbol2string s
               and vector_rep v  = "#("^(strlist2string(map rep (vector2list v)))^")"
               and pair_rep (d1,d2) = "("^(rep d1)^(pair_rep1 d2)^")"
               and number_rep n = number2str n
               and string_rep ss = str_rep (mstring2string ss)
               and list_rep l = "("^(strlist2string(map rep l))^")"
               and pair_rep1 d = 
                             if is_empty_list d then ""
                             else 
                                if is_pair d then 
                                   let val (p1,p2) = PAIR_UNTAG d
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
  fun write_current v = write (v, current_output_port())
  
  fun display (dval,oport) = 
            let fun disp dval =
                        let val err = fn _ => 
                         raise InputError ("Illegal argument to display", dval)
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
                                      input_port = err,
                                      output_port = err,
                                      otherwise = err}  dval
                        end
               and bool_disp b = boolean2str b
               and symbol_disp s = symbol2string s
               and char_disp c   = char2str c
               and vector_disp v  = "#("^(strlist2string(map disp (vector2list v)))^")"
               and pair_disp (d1,d2) = "("^(disp d1)^(pair_disp1 d2)^")"
               and number_disp n = number2str n
               and string_disp ss = mstring2string ss
               and list_disp l = "("^(strlist2string(map disp l))^")"
               and pair_disp1 d = 
                              if is_null d then ""
                              else 
                                 if is_pair d then 
                                    let val (p1,p2) = PAIR_UNTAG d
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
    fun display_current v = display (v, current_output_port())
    end
  end 
