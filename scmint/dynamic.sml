(*$SCHEMEDYNAMIC *)

signature DYNAMIC =
sig

(* DYNAMIC

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

General Scheme (dynamic) objects and generic routines

*)

(* TYPES *)

type dynamic

type boolean
type symbol
type char
type 'a vector
type ('a, 'b) pair
type number
type sstring
type ('a, 'b) procedure

type 'a slist
type input_port
type output_port


(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

val read: input_port -> dynamic
val write: dynamic * output_port -> unit
val display: dynamic * output_port -> unit

(* CONSTRUCTORS *)

val BOOL_TAG: boolean -> dynamic
val SYMBOL_TAG: symbol -> dynamic
val CHAR_TAG: char -> dynamic
val VECTOR_TAG: dynamic vector -> dynamic
val PAIR_TAG: (dynamic, dynamic) pair -> dynamic
val NUMBER_TAG: number -> dynamic
val STRING_TAG: sstring -> dynamic
val PROCEDURE_TAG: (dynamic list, dynamic) procedure -> dynamic

val LIST_TAG: dynamic slist -> dynamic
val INPUT_PORT_TAG: input_port -> dynamic
val OUTPUT_PORT_TAG: output_port -> dynamic
val UNSPECIFIED_TAG: unit -> dynamic

(* DESTRUCTORS *)

exception TypeError of string * dynamic

val BOOL_UNTAG: dynamic -> boolean
val SYMBOL_UNTAG: dynamic -> symbol
val CHAR_UNTAG: dynamic -> char
val VECTOR_UNTAG: dynamic -> dynamic vector
val PAIR_UNTAG: dynamic -> (dynamic, dynamic) pair
val NUMBER_UNTAG: dynamic -> number
val STRING_UNTAG: dynamic -> sstring
val PROCEDURE_UNTAG: dynamic -> (dynamic list, dynamic) procedure

val LIST_UNTAG: dynamic -> dynamic slist
val INPUT_PORT_UNTAG: dynamic -> input_port
val OUTPUT_PORT_UNTAG: dynamic -> output_port

val typecase:
	{bool: (boolean -> 'a), 
	 symbol: (symbol -> 'a),
	 char: (char -> 'a), 
	 vector: (dynamic vector -> 'a), 
	 pair: ((dynamic, dynamic) pair -> 'a), 
	 number: (number -> 'a), 
	 string: (sstring -> 'a),
	 procedure: (dynamic list, dynamic) procedure -> 'a, 
	 list: dynamic list -> 'a, 
	 input_port: input_port -> 'a, 
	 output_port: output_port -> 'a, 
	 otherwise: dynamic -> 'a} 
	-> dynamic -> 'a



val bool_case        : (boolean -> 'a) -> (dynamic -> 'a) -> 
                       (dynamic -> 'a)
val symbol_case      : (symbol -> 'a) -> (dynamic -> 'a) -> 
                       (dynamic -> 'a)
val char_case        : (char -> 'a) -> (dynamic -> 'a) -> 
                       (dynamic -> 'a)
val vector_case      : (dynamic vector -> 'a) -> (dynamic -> 'a) -> 
                       (dynamic -> 'a)
val pair_case        : ((dynamic, dynamic) pair -> 'a) -> (dynamic -> 'a) -> 
                       (dynamic -> 'a)
val number_case      : (number -> 'a) -> (dynamic -> 'a) -> 
                       (dynamic -> 'a)
val string_case      : (sstring -> 'a) -> (dynamic -> 'a) -> 
                       (dynamic -> 'a)
val procedure_case   : ((dynamic list, dynamic) procedure -> 'a) -> 
                        (dynamic -> 'a) ->  (dynamic -> 'a)
val list_case        : (dynamic slist -> 'a) -> (dynamic -> 'a) -> 
                       (dynamic -> 'a)
val input_port_case  : (input_port -> 'a) -> (dynamic -> 'a) -> 
                       (dynamic -> 'a)
val output_port_case : (output_port -> 'a) -> (dynamic -> 'a) -> 
                       (dynamic -> 'a)



(* TYPE TESTING PROCEDURES *)

val is_boolean: dynamic -> boolean
val is_symbol: dynamic -> boolean
val is_char: dynamic -> boolean
val is_vector: dynamic -> boolean
val is_pair: dynamic -> boolean
val is_number: dynamic -> boolean
val is_string: dynamic -> boolean
val is_procedure: dynamic -> boolean

val is_list: dynamic -> boolean
val is_null: dynamic -> boolean
val is_input_port: dynamic -> boolean
val is_output_port: dynamic -> boolean

(* CONVERSIONS *)

val dynamic2bool: dynamic -> boolean

(* EQUALITY PREDICATES *)

val is_eq: dynamic * dynamic -> boolean
val is_eqv: dynamic * dynamic -> boolean
val is_equal: dynamic * dynamic -> boolean

end



(*$SchemeDynamic: SCHEMEDYNAMIC SchemeGeneral SchemeBool SchemeSymbol 
        SchemeChar
	SchemeVector SchemePair SchemeList SchemeProcedure SchemeNumber
	SchemeInputOutput SchemeString *)

structure Dynamic: DYNAMIC =
  struct
  local 
      open General Boolean Symbol Character Vector Pair Number String List 
	   Control InputOutput 
  in
  type boolean = boolean
  type symbol = symbol
  type char = char
  type 'a vector = 'a vector
  type ('a, 'b) pair = ('a, 'b) pair
  type number = number
  type sstring = sstring
  type ('a, 'b) procedure = ('a, 'b) procedure
  type 'a slist = 'a slist
  type input_port = input_port
  type output_port = output_port
      
  datatype dyn = 
      BOOL of boolean | 
      CHAR of char |
      STRING of sstring |
      SYMBOL of symbol |
      NUMBER of number |
      VECTOR of dynamic vector |
      PAIR of (dynamic, dynamic) pair |
      PROCEDURE of (dynamic list, dynamic) procedure |
      LIST of dynamic slist |
      INPUT_PORT of input_port |
      OUTPUT_PORT of output_port |
      UNSPECIFIED
  withtype dynamic = dyn ref

  fun BOOL_TAG b = ref (BOOL b)
  fun SYMBOL_TAG s = ref (SYMBOL s)
  fun CHAR_TAG c = ref (CHAR c)
  fun VECTOR_TAG v = ref (VECTOR v)
  fun PAIR_TAG p = ref (PAIR p)
  fun NUMBER_TAG n = ref (NUMBER n)
  fun STRING_TAG s = ref (STRING s)          
  fun PROCEDURE_TAG p = ref (PROCEDURE p)

  fun LIST_TAG l = ref (LIST l)
  fun INPUT_PORT_TAG iport = ref (INPUT_PORT iport)
  fun OUTPUT_PORT_TAG oport = ref (OUTPUT_PORT oport)
  fun UNSPECIFIED_TAG () = ref UNSPECIFIED

  val unspecified = ref UNSPECIFIED

  exception TypeError of string * dynamic

  fun BOOL_UNTAG (ref (BOOL b)) = b |
      BOOL_UNTAG d = raise TypeError ("Not a Boolean", d)
  fun SYMBOL_UNTAG (ref (SYMBOL s)) = s |
      SYMBOL_UNTAG d = raise TypeError ("Not a symbol", d)
  fun CHAR_UNTAG (ref (CHAR c)) = c |
      CHAR_UNTAG d = raise TypeError ("Not a character", d)
  fun VECTOR_UNTAG (ref (VECTOR v)) = v |
      VECTOR_UNTAG d = raise TypeError ("Not a vector", d)
  fun PAIR_UNTAG (ref (PAIR p)) = p |
      PAIR_UNTAG (ref (LIST (a::r))) = (a, ref (LIST r)) |
      PAIR_UNTAG d = raise TypeError ("Not a pair", d)
  fun NUMBER_UNTAG (ref (NUMBER n)) = n |
      NUMBER_UNTAG d = raise TypeError ("Not a number", d)
  fun STRING_UNTAG (ref (STRING s)) = s |
      STRING_UNTAG d = raise TypeError ("Not a string", d)
  fun PROCEDURE_UNTAG (ref (PROCEDURE p)) = p |
      PROCEDURE_UNTAG d = raise TypeError ("Not a procedure", d)

  fun LIST_UNTAG (ref (LIST l)) = l |
      LIST_UNTAG (ref (PAIR (l,r))) = l :: LIST_UNTAG r |
      LIST_UNTAG d = raise TypeError ("Not a list", d)
  fun INPUT_PORT_UNTAG (ref (INPUT_PORT iport)) = iport |
      INPUT_PORT_UNTAG d = raise TypeError ("Not an input port", d)
  fun OUTPUT_PORT_UNTAG (ref (OUTPUT_PORT oport)) = oport |
      OUTPUT_PORT_UNTAG d = raise TypeError ("Not an output port", d)

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

    
  fun bool_case bool_fcn else_fcn dval =
      (case dval of
	   (ref (BOOL b)) => bool_fcn b
	 |  dval => else_fcn dval)

  fun symbol_case symbol_fcn else_fcn dval =
      (case dval of
	   (ref (SYMBOL s)) => symbol_fcn s
	 |  dval => else_fcn dval)
    
  fun char_case char_fcn else_fcn dval =
      (case dval of
	   (ref (CHAR c)) => char_fcn c
	 |  dval => else_fcn dval)
    
  fun vector_case vector_fcn else_fcn dval =
      (case dval of
	   (ref (VECTOR v)) => vector_fcn v
	 |  dval => else_fcn dval)
    
  fun pair_case pair_fcn else_fcn dval =
      (case dval of
	   (ref (PAIR p)) => pair_fcn p
	 |  dval => else_fcn dval)
    
  fun number_case number_fcn else_fcn dval =
      (case dval of
	   (ref (NUMBER n)) => number_fcn n
	 |  dval => else_fcn dval)

  fun string_case string_fcn else_fcn dval =
      (case dval of
	   (ref (STRING s)) => string_fcn s
	 |  dval => else_fcn dval)
    
  fun procedure_case procedure_fcn else_fcn dval =
      (case dval of
	   (ref (PROCEDURE p)) => procedure_fcn p
	 |  dval => else_fcn dval)
	   
  fun list_case list_fcn else_fcn dval =
      (case dval of
	   (ref (LIST l)) => list_fcn l
	 |  dval => else_fcn dval)
	   
  fun input_port_case input_port_fcn else_fcn dval =
      (case dval of
	   (ref (INPUT_PORT iport)) => input_port_fcn iport
	 |  dval => else_fcn dval)
    
  fun output_port_case output_port_fcn else_fcn dval =
      (case dval of
	   (ref (OUTPUT_PORT oport)) => output_port_fcn oport
	 |  dval => else_fcn dval)

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
  fun is_input_port (ref (INPUT_PORT _)) = true |
      is_input_port _ = false
  fun is_output_port (ref (OUTPUT_PORT _)) = true |
      is_output_port _ = false

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
                                         input_port = err,
                                         output_port = err,
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
                                      input_port = err,
                                      output_port = err,
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




  end
  end
