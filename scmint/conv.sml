(* structure Conversion: CONVERSION =
  struct  *)
    local
    open Char
    in
    fun object2boolean (ref (BOOL b)) = b |
        object2boolean _ = true
    fun not (ref (BOOL false)) = true |
        not _ = false
    fun string2char s =
        case fromString s of
          NONE => raise Impossible "string2char"
        | SOME c => c
    fun mstring2string (FIXED s) = s |
        mstring2string (MUTABLE l) = implode (map ( string2char o !) l)
    val string2mstring = MUTABLE o (map (ref o toString)) o explode

    fun vector2list v =
        let val length = Vector.length v
            fun v2l (l, 0) = l
              | v2l (l, n) = v2l (Vector.sub (v, n-1) :: l, n-1)
        in v2l (nil, length)
        end
    end
(*  end *)
