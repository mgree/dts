structure Datum (*: SCHEMEDATUM *) =
  struct

  datatype 'a udatum =
      BOOLDAT of bool |
      CHARDAT of string |
      STRIDAT of string |
      SYMBDAT of string |
      NUMBDAT of string |
      VECTDAT of 'a datum list |
      PAIRDAT of 'a datum * 'a datum |   
      NILDAT 
  withtype 'a datum = 'a udatum * 'a

  fun dattrib (ud, a) = a


  datatype token =
      Identifier of string
    | BoolSym of bool
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
    | EndOfInput

  exception ReadError of string * string
  exception EOF

  fun read_datum init ip = 
      let val read_so_far = ref ""
          fun read_error msg =
              (input_line ip;
               raise ReadError (msg, !read_so_far))
          fun get_next_char() =
              let val next_char = input(ip,1)
              in
                 (read_so_far := !read_so_far ^ next_char; next_char)
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
          fun is_delimiter c = case c of
                                    ")" => true
                               |    "(" => true
                               |    "\"" => true
                               |    ";" => true
                               |    "\n" => true
                               |    "\t" => true
                               |    " " => true
                               |     _  => false
          fun is_digit c = case c of
                                "0" => true
                              | "1" => true
                              | "2" => true
                              | "3" => true
                              | "4" => true
                              | "5" => true
                              | "6" => true
                              | "7" => true
                              | "8" => true
                              | "9" => true
                              | _ => false
          fun get_next_token() =
              let val next_char = get_next_char()
              in case next_char of
                   ""  => EndOfInput
                |  " " => get_next_token()
                | "\t" => get_next_token()
                | "\n" => get_next_token()
                | ";" => (get_line(); get_next_token()) (* SemiColon *)
                | "\"" => QuotationMark
                | "(" => LeftParen
                | ")" => RightParen
                | "`" => QuasiquoteSym
                | "'" => QuoteSym
                | "." => if is_delimiter (lookahead ip) then DotSym
                         else Identifier ("."^get_identifier())
                               
                            
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
                            | c => (Identifier (c ^ get_identifier())))  (* Non-standard identifier *)
                | "+" => let val lh = lookahead ip in
                             if lh = "." orelse is_digit lh then NumbSym (get_identifier())
                             else Identifier ("+"^get_identifier()) (* Non-standard identifier *)
                         end
                | "-" => let val lh = lookahead ip in
                             if lh = "." orelse is_digit lh then NumbSym (get_identifier())
                             else Identifier ("-"^get_identifier())  (* Non-standard identifier *)
                         end
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
                | c => Identifier (c ^ get_identifier())
              end
 
          fun listdat [] = (NILDAT, init())
            | listdat (d::r) = (PAIRDAT (d, listdat r), init())

          fun make_datum DC s = (DC s, init())
          fun parse_datum(tok) =
              (* parses the input stream with tok prepended as the 
               first token *)
              case tok of
                  Identifier s => make_datum SYMBDAT s
                | BoolSym b => make_datum BOOLDAT b 
                | NumbSym s => make_datum NUMBDAT s
                | CharSym c => make_datum CHARDAT c
                | QuotationMark => make_datum STRIDAT (get_string())
                | LeftParen => parse_list()
                | VectorSym => (VECTDAT (parse_vector()), init())
                | QuoteSym => 
                      listdat [make_datum SYMBDAT "quote", 
                               parse_datum (get_next_token())]
                | QuasiquoteSym =>
                      listdat [make_datum SYMBDAT "quasiquote", 
                               parse_datum (get_next_token())]
                | UnquoteSym => 
                      listdat [make_datum SYMBDAT "unquote", 
                               parse_datum (get_next_token())]
                | UnqSplSym =>
                      listdat [make_datum SYMBDAT "unquote-splicing", 
                               parse_datum (get_next_token())]
                | _ => read_error "Illegal input"
          and parse_list() =
              let val tok = get_next_token()
              in if tok = RightParen 
                    then (NILDAT, init())
                 else (PAIRDAT (parse_datum(tok), parse_list_rem()), init())
              end
          and parse_list_rem() =
              let val tok = get_next_token()
              in case tok of
                  RightParen => (NILDAT, init())
                | DotSym => let val pd = parse_datum(get_next_token())
                            in if get_next_token() = RightParen
                                   then pd
                               else read_error "Illegal input: ) expected"
                            end
                | _ => (PAIRDAT (parse_datum(tok), parse_list_rem()), init())
              end
          and parse_vector() =
              let val tok = get_next_token()
              in case tok of 
                  RightParen => nil
                | _ => parse_datum(tok) :: parse_vector()
              end
      in 
         let val tok = get_next_token() in
             case tok of 
                  EndOfInput => raise EOF
             |    _ => parse_datum(tok) (* before input_line ip *)
         end
      end

end
