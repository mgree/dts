signature KERNELEXP =
  sig

  datatype ('a, 'c, 'd, 'e, 'f, 'l, 'v) exp_hom =
    EHOM of { noexp: 'a -> 'e,
              literal: 'a -> 'd -> 'e,
              variable: 'a -> 'v * 'c -> 'e,
              call: 'a -> 'e * 'l -> 'e,
              lambda: 'a -> 'f * 'e -> 'e,
              ifexp: 'a -> 'e * 'e * 'e -> 'e,
              assign: 'a -> 'v * 'e -> 'e,
              pairarg: 'a -> 'e * 'l -> 'l,
              nullarg: 'a -> 'l,
              avarpar: 'a -> 'v -> 'f,
              apairpar: 'a -> 'v * 'f -> 'f,
              anullpar: 'a -> 'f
            }

  type 'a anndatum

  datatype 'b variable =
    VAR of string * 'b

  datatype ('a, 'b, 'c) exp =
    NOEXP |
    LITERAL of 'a anndatum |
    VARIABLE of 'b variable * 'c |
    CALL of ('a, 'b, 'c) annexp * ('a, 'b, 'c) annargs |
    LAMBDA of ('a, 'b) annformals * ('a, 'b, 'c) annexp |
    IF of ('a, 'b, 'c) annexp * ('a, 'b, 'c) annexp * ('a, 'b, 'c) annexp |  
    ASSIGN of 'b variable * ('a, 'b, 'c) annexp

  and ('a, 'b, 'c) annexp =
    EXP of ('a, 'b, 'c) exp * 'a

  and ('a, 'b, 'c) args =
    PAIRARG of ('a, 'b, 'c) annexp * ('a, 'b, 'c) annargs |
    NULLARG 

  and ('a, 'b, 'c) annargs =
    ARGS of ('a, 'b, 'c) args * 'a

  and ('a, 'b) formals =
    AVARPAR of 'b variable |
    APAIRPAR of 'b variable * ('a, 'b) annformals |
    ANULLPAR

  and ('a, 'b) annformals =
    FORMALS of ('a, 'b) formals * 'a

  val apply_ehom: ('a, 'c, 'a anndatum, 'e, 'f, 'l, 'b variable) exp_hom -> 
                  ('a, 'b, 'c) annexp -> 'e

  exception ParseError of string

  val dat2exp: ((unit -> 'a) * (string -> 'b) * (unit -> 'c)) -> 
                'a anndatum -> ('a, 'b, 'c) annexp

  val read_exp: ((unit -> 'a) * (string -> 'b) * (unit -> 'c)) -> instream ->
                ('a, 'b, 'c) annexp

  end


