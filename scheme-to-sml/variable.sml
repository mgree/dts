structure SchemeVariables =
  struct
  local open UnionFind 
  in
  
  datatype ('a, 'b, c) varcont =
  | LAMBOUND of 'a
  | LETBOUND of 'b
  | FREE of 'c

  type ('a, 'b, 'c) var = ('a, 'b, 'c) varcont UF

  val new_var = make o FREE
  val new_lambound = make o LAMBOUND
  val new_letbound = make o LETBOUND

  val varcont = op!!

  end

  

  