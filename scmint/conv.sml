structure Conversion: CONVERSIO
  struc
    fun object2boolean (ref (BOOL b)) = b 
        object2boolean _ = true
    fun not (ref (BOOL false)) = tru
        not _ = false
    fun mstring2string (FIXED s) = s 
        mstring2string (MUTABLE l) = implode (map ! 
    val string2mstring = MUTABLE o (map ref) o explo
    fun vector2list 
        let val length = Vector.length 
	    fun v2l (l, 0) = l
              | v2l (l, n) = v2l (Vector.sub (v, n-1) :: l, n-1)
        in v2l (nil, lengt
        en
  en
