type 'a variable = 'a * string

datatype 'a exp =
    EMPTY |
    LITERAL of 'a anndatum |
    VARIABLE of 'a variable |
    CALL of 'a annexp * 'a annargs |
    LAMBDA of 'a annformals * 'a annexp |
    IF of 'a annexp * 'a annexp * 'a annexp |  
    ASSIGN of 'a variable * 'a annexp
and 'a datum = ...
and 'a args =
    PAIRARG of 'a annexp * 'a annargs |
    NULLARG 
and 'a formals =
  AVARPAR of 'a variable |
  APAIRPAR of 'a variable * 'a annformals |
  ANULLPAR
withtype 'a annexp = 'a * 'a exp
and 'a anndatum = 'a * 'a datum
and 'a annargs = 'a * 'a args
and 'a annformals = 'a * 'a formals 

