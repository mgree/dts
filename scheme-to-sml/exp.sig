signature KERNELEXP =
  sig

  datatype ('a, 'd, 'e, 'f, 'l, 'v) exp_hom =
    EHOM of { noexp: 'a -> 'e,
              literal: 'a -> 'd -> 'e,
              variable: 'a -> 'v -> 'e,
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

  datatype ('a, 'b) exp =
    NOEXP |
    LITERAL of 'a anndatum |
    VARIABLE of 'b variable |
    CALL of ('a, 'b) annexp * ('a, 'b) annargs |
    LAMBDA of ('a, 'b) annformals * ('a, 'b) annexp |
    IF of ('a, 'b) annexp * ('a, 'b) annexp * ('a, 'b) annexp |  
    ASSIGN of 'b variable * ('a, 'b) annexp

  and ('a, 'b) annexp =
    EXP of ('a, 'b) exp * 'a

  and ('a, 'b) args =
    PAIRARG of ('a, 'b) annexp * ('a, 'b) annargs |
    NULLARG 

  and ('a, 'b) annargs =
    ARGS of ('a, 'b) args * 'a

  and ('a, 'b) formals =
    AVARPAR of 'b variable |
    APAIRPAR of 'b variable * ('a, 'b) annformals |
    ANULLPAR

  and ('a, 'b) annformals =
    FORMALS of ('a, 'b) formals * 'a

  val apply_ehom: ('a, 'a anndatum, 'e, 'f, 'l, 'b variable) exp_hom -> 
                  ('a, 'b) annexp -> 'e

  exception ParseError of string

  val dat2exp: ((unit -> 'a) * (unit -> 'b)) -> 
                'a anndatum -> ('a, 'b) annexp

  val read_exp: ((unit -> 'a) * (unit -> 'b)) -> instream ->
                ('a, 'b) annexp

  end


