structure SchemeDatum: SCHEMEDATUM =
  struct

  datatype ('a, 'd) datum_hom =
      DHOM of { booldat: 'a -> bool -> 'd,
                chardat: 'a -> string -> 'd,
                stridat: 'a -> string -> 'd,
                symbdat: 'a -> string -> 'd,
                numbdat: 'a -> string -> 'd,
                vectdat: 'a -> 'd list -> 'd,
                pairdat: 'a -> 'd * 'd -> 'd,
                nildat: 'a -> 'd 
              }

  datatype 'a datum =
      BOOLDAT of bool |
      CHARDAT of string |
      STRIDAT of string |
      SYMBDAT of string |
      NUMBDAT of string |
      VECTDAT of 'a anndatum list |
      PAIRDAT of 'a anndatum * 'a anndatum |   
      NILDAT

  and 'a anndatum =
      DATUM of 'a datum * 'a

  fun apply_dhom (DHOM H: ('a, 'd) datum_hom) (d: 'a anndatum): 'd = 
      let fun apply (DATUM (d,a): 'a anndatum) =
              case d of
                BOOLDAT b => #booldat H a b
              | CHARDAT c => #chardat H a c
              | STRIDAT s => #stridat H a s
              | SYMBDAT s => #symbdat H a s
              | NUMBDAT s => #numbdat H a s
              | VECTDAT dl => #vectdat H a (map apply dl)
              | PAIRDAT (d1, d2) => #pairdat H a (apply d1, apply d2)
              | NILDAT => #nildat H a
      in apply d
      end

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
 
          fun make_datum DC x = DATUM (DC x, init())
          fun make_nil () = DATUM (NILDAT, init())

          fun listdat [] = make_nil ()
            | listdat (a::r) = make_datum PAIRDAT (a, listdat r)

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
                | VectorSym => make_datum VECTDAT (parse_vector())
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
                    then make_nil ()
                 else make_datum PAIRDAT (parse_datum(tok), parse_list_rem())
              end
          and parse_list_rem() =
              let val tok = get_next_token()
              in case tok of
                  RightParen => make_nil ()
                | DotSym => let val pd = parse_datum(get_next_token())
                            in if get_next_token() = RightParen
                                   then pd
                               else read_error "Illegal input: ) expected"
                            end
                | _ => make_datum PAIRDAT (parse_datum(tok), parse_list_rem())
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