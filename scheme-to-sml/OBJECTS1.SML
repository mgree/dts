structure Datum =
  struct

  datatype ('da, 'd) datum_hom =
      DHOM of { booldat: 'da * bool -> 'd,
                chardat: 'da * string -> 'd,
                stridat: 'da * string -> 'd,
                symbdat: 'da * string -> 'd,
                numbdat: 'da * string -> 'd,
                vectdat: 'da * 'd list -> 'd,
                pairdat: 'da * 'd * 'd -> 'd,
                nildat: 'da -> 'd 
              }

  datatype 'a datum =
      BOOLDAT of 'a * bool |
      CHARDAT of 'a * string |
      STRIDAT of 'a * string |
      SYMBDAT of 'a * string |
      NUMBDAT of 'a * string |
      VECTDAT of 'a * 'a datum list |
      PAIRDAT of 'a * 'a datum * 'a datum |   
      NILDAT of 'a

  fun dattrib (BOOLDAT (a,_)) = a
    | dattrib (CHARDAT (a,_)) = a
    | dattrib (STRIDAT (a,_)) = a
    | dattrib (SYMBDAT (a,_)) = a
    | dattrib (NUMBDAT (a,_)) = a
    | dattrib (VECTDAT (a,_)) = a
    | dattrib (PAIRDAT (a,_,_)) = a
    | dattrib (NILDAT a) = a
   
  fun apply_dhom (DHOM D) = 
      let fun dapply (BOOLDAT x) = #booldat D x
            | dapply (CHARDAT x) = #chardat D x
            | dapply (STRIDAT x) = #stridat D x
            | dapply (SYMBDAT x) = #symbdat D x
            | dapply (NUMBDAT x) = #numbdat D x
            | dapply (VECTDAT (a,dl)) = #vectdat D (a, map dapply dl)
            | dapply (PAIRDAT (a, d1, d2)) = 
                        #pairdat D (a, dapply d1, dapply d2)
            | dapply (NILDAT a) = #nildat D a 
      in dapply 
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
 
          fun listdat [] = NILDAT (init())
            | listdat (d::r) = PAIRDAT (init(), d, listdat r)

          fun make_datum DC s = DC (init(), s)
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
                | VectorSym => VECTDAT (init(), parse_vector())
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
                    then NILDAT (init())
                 else PAIRDAT (init(), parse_datum(tok), parse_list_rem())
              end
          and parse_list_rem() =
              let val tok = get_next_token()
              in case tok of
                  RightParen => NILDAT (init())
                | DotSym => let val pd = parse_datum(get_next_token())
                            in if get_next_token() = RightParen
                                   then pd
                               else read_error "Illegal input: ) expected"
                            end
                | _ => PAIRDAT (init(),parse_datum(tok), parse_list_rem())
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

end;

structure Exp =
  struct 
  local open Datum SchemeGeneral
  in

  type 'v var = string * 'v
  
  datatype ('a, 'd, 'e, 'l, 'f, 't, 'v) exp_hom =
    EHOM of { noexp: 'a -> 'e,
              literal: 'd -> 'e,
              variable: 'a * 'v var -> 'e,
              call: 'a * 'e * 'l -> 'e,
              lambda: 'a * 'f * 'e -> 'e,
              ifexp: 'a * 'e * 'e * 'e -> 'e,
              assign: 'a * 'v var * 'e -> 'e,
              pairarg: 'a * 'e * 'l -> 'l,
              nullarg: 'a -> 'l,
              avarpar: 't var -> 'f,
              apairpar: 't var * 'f -> 'f,
              anullpar: unit -> 'f }

  datatype ('a, 't, 'v) exp =
    NOEXP of 'a |
    LITERAL of 'a datum |
    VARIABLE of 'a * 'v var |
    CALL of 'a * ('a, 't, 'v) exp * ('a, 't, 'v) args |
    LAMBDA of 'a * 't formals * ('a, 't, 'v) exp |
    IF of 'a * ('a, 't, 'v) exp * ('a, 't, 'v) exp * ('a, 't, 'v) exp |  
    ASSIGN of 'a * 'v var * ('a, 't, 'v) exp 
 

  

  and ('a, 't, 'v) args =
    PAIRARG of 'a * ('a, 't, 'v) exp * ('a, 't, 'v) args |
    NULLARG of 'a

  and 't formals =
    AVARPAR of 't var |
    APAIRPAR of 't var * 't formals |
    ANULLPAR

  datatype ('a, 't, 'v) def = 
    DEFINE of ('t var) * ('a, 't, 'v) exp 
  
  type ('a, 't, 'v) program = ('a, 't, 'v) def list 

  fun eattrib (NOEXP a) = a
    | eattrib (LITERAL d) = dattrib d
    | eattrib (VARIABLE (a,v)) = a
    | eattrib (CALL (a,_,_)) = a
    | eattrib (LAMBDA (a,_,_)) = a
    | eattrib (IF(a,_,_,_)) = a
    | eattrib (ASSIGN(a,_,_)) = a 

  fun apply_ehom (EHOM E) =
      let fun eapply (NOEXP a) = #noexp E a
            | eapply (LITERAL d) = #literal E d
            | eapply (VARIABLE (a,v)) = #variable E (a,v) 
            | eapply (CALL (a, e, l)) = #call E (a, eapply e, lapply l)
            | eapply (LAMBDA (a, f, e)) = #lambda E (a, fapply f, eapply e)
            | eapply (IF (a, e1, e2, e3)) = #ifexp E (a, eapply e1, eapply e2, eapply e3)
            | eapply (ASSIGN (a, v, e)) = #assign E (a, v, eapply e)
          and fapply (AVARPAR v) = #avarpar E v
            | fapply (APAIRPAR (v, f')) = #apairpar E (v, fapply f')
            | fapply ANULLPAR = #anullpar E () 
          and lapply (PAIRARG (a, e, l')) = #pairarg E (a, eapply e, lapply l')
            | lapply (NULLARG a) = #nullarg E a 
      in eapply
      end

  exception ParseError of string
  fun parseError s = (output(std_out, "Parse error: " ^ s); 
  					  raise ParseError s)
 
  val keywords = ["lambda", "if", "set!", "quote", "define"]
        
  fun dat2ast (parinit, varinit) = 
      let fun dat2program (NILDAT _) = nil
            | dat2program (PAIRDAT (_, d1, d2)) =
            	dat2def d1 :: dat2program d2
            | dat2program _ = parseError "Illegal program"
      	  and dat2def (PAIRDAT(_, SYMBDAT(_, "define"),
                       PAIRDAT(_, d1, PAIRDAT (_, d2, NILDAT _)))) =
                 DEFINE (dat2par d1, dat2exp d2)  
            | dat2def _ = parseError ("Illegal definition")               
      	  and dat2exp (PAIRDAT (a,d1,d2)) =
              (case d1 of
                 SYMBDAT (_, "lambda") => dat2lambda (a,d2)
               | SYMBDAT (_, "if") => dat2if (a,d2)
               | SYMBDAT (_, "set!") => dat2assign (a,d2)
               | SYMBDAT (_, "quote") => dat2quote (a,d2)
               | _ => CALL (a, dat2exp d1, dat2args d2))
            | dat2exp (SYMBDAT (a,s)) = VARIABLE (a, (s, varinit (a,s)))
            | dat2exp d = LITERAL d
          and dat2lambda (a, (PAIRDAT (_, f, (PAIRDAT (_, e, NILDAT _))))) =
                 LAMBDA (a, dat2formals f, dat2exp e)
            | dat2lambda _ = parseError ("Illegal lambda expression")
          and dat2formals (SYMBDAT (a,s)) = 
          	     if member s keywords
          	     	then parseError ("Illegal parameter (keyword)")
          	     else AVARPAR (s, parinit (a,s))
            | dat2formals (PAIRDAT (a,d1,d2)) = 
                 APAIRPAR (dat2par d1, dat2formals d2)
            | dat2formals (NILDAT a) = ANULLPAR
            | dat2formals _ = parseError ("Illegal formals")
          and dat2par (SYMBDAT (a,s)) =
                if member s keywords
                    then parseError ("Illegal parameter (keyword)")
                 else (s, parinit (a,s))
            | dat2par _ = 
                parseError ("Illegal parameter (not an identifier)")
          and dat2if (a, (PAIRDAT (_, d1, PAIRDAT (_, d2, d3)))) =
                IF (a, dat2exp d1, dat2exp d2, dat2else d3)
            | dat2if _ = parseError ("Illegal if expression")
          and dat2else (PAIRDAT (_, d, NILDAT _)) =
                 dat2exp d
            | dat2else (NILDAT a) = NOEXP a
            | dat2else _ = parseError ("Illegal if expression (else)")
          and dat2assign (a, (PAIRDAT (_,d1, PAIRDAT (_,d2, NILDAT _)))) =
                 ASSIGN (a, dat2var d1, dat2exp d2)
            | dat2assign _ = parseError ("Illegal assignment")

          and dat2quote (a, PAIRDAT (_,d1, NILDAT _)) = 
                 LITERAL d1 
            | dat2quote _ = parseError ("Illegal quotation")
          and dat2var (SYMBDAT (a,s)) = 
                 if member s keywords 
                    then parseError ("Illegal variable (keyword)")
                 else (s, varinit (a,s))
            | dat2var _ = parseError ("Illegal variable (not an identifier)")
          and dat2args (PAIRDAT (a, d1, d2)) =
                PAIRARG (a, dat2exp d1, dat2args d2)
            | dat2args (NILDAT a) = NULLARG a
            | dat2args _ = parseError ("Illegal argument list")
      in
      (dat2exp, dat2def, dat2program)
      end

  datatype ('a, 't, 'v) attinit =
      INIT of { parameter: 'a * string -> 't,
                variable: 'a * string -> 'v,
                exp: unit -> 'a}
                                    
  fun read (INIT J) = 
    let val (dat2exp, dat2def, dat2program) = 
                 dat2ast (#parameter J, #variable J) 
        val in2dat = read_datum (#exp J)
    in (dat2exp o in2dat, dat2def o in2dat, dat2program o in2dat)
    end

  end
end;
























