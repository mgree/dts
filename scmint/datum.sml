structure Datum: DATUM =
  struct
  open Error
  datatype token =
      Identifier of string
    | BoolSym of bool
    | NumbSym of string
    | CharSym of string
    | QuotationMar
    | LeftParen
    | RightParen
    | VectorS
    | QuoteSy
    | QuasiquoteSym
    | UnquoteS
    | UnqSplSym
    | DotSy
    | EndOfInput

  fun read_datum i
      let val read_so_far = ref ""
          fun read_error msg 
              (input_line ip;
               raise IOError (msg, !read_so_far)
          fun get_next_char() 
              let val next_char = input(ip,1
              in
                 (read_so_far := !read_so_far ^ next_char; next_char)
              e
          fun get_identifier() 
              let val next_char = lookahead i
              in case next_char 
                  "" => "
                | " " => "
                | "\t" => 
                | "\n" => 
                | "(" => 
                | ")" => ""
                | "\"" => 
                | ";" => "
                | _ => (get_next_char() ^ get_identifier()
              en
          fun get_string() 
              let val c = get_next_char() 
              in case c o
                  "\"" => "
                | "\\" => get_next_char() ^ get_string(
                | _ => c ^ get_string()
              end
          fun get_line() = input_line ip
          fun is_delimiter c = case c 
                                    ")" => true
                               |    "(" => true
                               |    "\"" => true
                               |    ";" => tru
                               |    "\n" => tru
                               |    "\t" => tru
                               |    " " => tru
                               |     _  => fals
          fun is_digit c = case c 
                                "0" => tr
                              | "1" => true
                              | "2" => tr
                              | "3" => tr
                              | "4" => tr
                              | "5" => true
                              | "6" => tru
                              | "7" => tr
                              | "8" => tr
                              | "9" => true
                              | _ => fals
          fun get_next_token(
              let val next_char = get_next_char(
              in case next_char o
                   ""  => EndOfInp
                |  " " => get_next_token(
                | "\t" => get_next_token(
                | "\n" => get_next_token(
                | ";" => (get_line(); get_next_token()) (* SemiColon *)
                | "\"" => QuotationMar
                | "(" => LeftParen
                | ")" => RightParen
                | "`" => QuasiquoteSym
                | "'" => QuoteSym
                | "." => if is_delimiter (lookahead ip) then DotSy
                         else Identifier ("."^get_identifier())
                | "," => if lookahead ip = "@" then (get_next_char(); UnqSplSy
                         else UnquoteS
                | "#" => (case get_next_char() 
                              "(" => VectorS
                            | "t" => BoolSym tr
                            | "f" => BoolSym fals
                            | "\\" => CharSym (get_next_char() ^ get_identifier(
                            | c => (Identifier (c ^ get_identifier())))  (* Non-standard identifier *
                | "+" => let val lh = lookahead ip in
                             if lh = "." orelse is_digit lh then NumbSym (get_identifier(
                             else Identifier ("+"^get_identifier()) (* Non-standard identifier *
                         en
                | "-" => let val lh = lookahead ip 
                             if lh = "." orelse is_digit lh then NumbSym (get_identifier())
                             else Identifier ("-"^get_identifier())  (* Non-standard identifier 
                         end
                | "0" => NumbSym ("0" ^ get_identifier()
                | "1" => NumbSym ("1" ^ get_identifier()
                | "2" => NumbSym ("2" ^ get_identifier()
                | "3" => NumbSym ("3" ^ get_identifier()
                | "4" => NumbSym ("4" ^ get_identifier(
                | "5" => NumbSym ("5" ^ get_identifier())
                | "6" => NumbSym ("6" ^ get_identifier())
                | "7" => NumbSym ("7" ^ get_identifier())
                | "8" => NumbSym ("8" ^ get_identifier()
                | "9" => NumbSym ("9" ^ get_identifier(
                | c => Identifier (c ^ get_identifier()
              en
          fun listdat [] = NILD
            | listdat (d::r) = PAIRDAT (d, listdat r
          fun make_datum DC s = DC (s
          fun parse_datum(tok) 
              (* parses the input stream with tok prepended as t
               first token 
              case tok 
                  Identifier s => make_datum SYMBDAT 
                | BoolSym b => make_datum BOOLDAT 
                | NumbSym s => make_datum NUMBDAT 
                | CharSym c => make_datum CHARDAT 
                | QuotationMark => make_datum STRIDAT (get_string()
                | LeftParen => parse_list
                | VectorSym => VECTDAT (parse_vector(
                | QuoteSym => 
                      listdat [make_datum SYMBDAT "quote
                               parse_datum (get_next_token()
                | QuasiquoteSym 
                      listdat [make_datum SYMBDAT "quasiquote
                               parse_datum (get_next_token())
                | UnquoteSym 
                      listdat [make_datum SYMBDAT "unquote
                               parse_datum (get_next_token()
                | UnqSplSym =
                      listdat [make_datum SYMBDAT "unquote-splicing
                               parse_datum (get_next_token())
                | _ => read_error "Illegal inpu
          and parse_list() 
              let val tok = get_next_token
              in if tok = RightPare
                    then NILD
                 else PAIRDAT (parse_datum(tok), parse_list_rem()
              e
          and parse_list_rem(
              let val tok = get_next_token
              in case tok of
                  RightParen => NILDAT 
                | DotSym => let val pd = parse_datum(get_next_token())
                            in if get_next_token() = RightPar
                                   then 
                               else read_error "Illegal input: ) expected
                            e
                | _ => PAIRDAT (parse_datum(tok), parse_list_rem())
              en
          and parse_vector(
              let val tok = get_next_token
              in case tok o
                  RightParen => nil
                | _ => parse_datum(tok) :: parse_vector
              end
      i
         let val tok = get_next_token() in
             case tok o
                  EndOfInput => raise EndOfFil
             |    _ => parse_datum(tok) (* before input_line ip *
         en
      en
  en
