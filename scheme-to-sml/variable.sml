structure SchemeVariables =
  struct
  local open UnionFind 
  in
  
  datatype ('a, 'b, 'c) var =
    LAMBOUND of 'a
  | LETBOUND of 'b
  | FREE of 'c

  end
end

  

  