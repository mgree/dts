  structure Object: OBJECT =
    struct

    fun BOOL_TAG b = ref (BOOL b)
    fun SYMBOL_TAG s = ref (SYMBOL s)
    fun CHAR_TAG c = ref (CHAR c)
    fun VECTOR_TAG v = ref (VECTOR v)
    fun PAIR_TAG p = ref (PAIR p)
    fun NUMBER_TAG n = ref (NUMBER n)
    val COMPLEX_TAG = NUMBER_TAG
    val REAL_TAG = NUMBER_TAG
    val RATIONAL_TAG = NUMBER_TAG
    val INTEGER_TAG = NUMBER_TAG
    val NATURAL_TAG = NUMBER_TAG
    fun STRING_TAG s = ref (STRING s)          
    fun PROCEDURE_TAG p = ref (PROCEDURE p)
    fun LIST_TAG l = ref (LIST l)
    fun INPUT_PORT_TAG iport = ref (INPUT_PORT iport)
    fun OUTPUT_PORT_TAG oport = ref (OUTPUT_PORT oport)
    fun UNSPECIFIED_TAG () = ref UNSPECIFIED
    
    local open Error 
    in
    fun BOOL_UNTAG (ref (BOOL b)) = b |
        BOOL_UNTAG d = type_error ("boolean", d)
    fun SYMBOL_UNTAG (ref (SYMBOL s)) = s |
        SYMBOL_UNTAG d = type_error ("symbol", d)
    fun CHAR_UNTAG (ref (CHAR c)) = c |
        CHAR_UNTAG d = type_error ("character", d)
    fun VECTOR_UNTAG (ref (VECTOR v)) = v |
        VECTOR_UNTAG d = type_error ("vector", d)
    fun PAIR_UNTAG (ref (PAIR p)) = p |
        PAIR_UNTAG (ref (LIST (a::r))) = (a, ref (LIST r)) |
        PAIR_UNTAG d = type_error ("pair", d)
    fun NUMBER_UNTAG (ref (NUMBER n)) = n |
        NUMBER_UNTAG d = type_error ("number", d)
    val COMPLEX_UNTAG = NUMBER_UNTAG
    val REAL_UNTAG = NUMBER_UNTAG
    val RATIONAL_UNTAG = NUMBER_UNTAG
    val INTEGER_UNTAG = NUMBER_UNTAG
    fun NATURAL_UNTAG (d as (ref (NUMBER n))) =
	if n < 0 then raise TypeError ("natural", d) else n |
        NATURAL_UNTAG d = type_error ("natural", d)
    fun STRING_UNTAG (ref (STRING s)) = s |
        STRING_UNTAG d = type_error ("string", d)
    fun PROCEDURE_UNTAG (ref (PROCEDURE p)) = p |
        PROCEDURE_UNTAG d = type_error ("procedure", d)
    fun LIST_UNTAG (ref (LIST l)) = l |
        LIST_UNTAG (ref (PAIR (l,r))) = l :: LIST_UNTAG r |
        LIST_UNTAG d = type_error ("list", d)
    fun INPUT_PORT_UNTAG (ref (INPUT_PORT iport)) = iport |
        INPUT_PORT_UNTAG d = type_error ("input port", d)
    fun OUTPUT_PORT_UNTAG (ref (OUTPUT_PORT oport)) = oport |
        OUTPUT_PORT_UNTAG d = type_error ("output port", d)
    end
  
    (* TYPE TESTING PROCEDURES *)
  
    fun is_boolean (ref (BOOL b)) = true |
        is_boolean _ = false
    fun is_symbol (ref (SYMBOL s)) = true |
        is_symbol _ = false
    fun is_char (ref (CHAR c)) = true |
        is_char _ = false
    fun is_vector (ref (VECTOR v)) = true |
        is_vector _ = false
    fun is_pair (ref (PAIR p)) = true |
        is_pair (ref (LIST (x::y))) = true |
        is_pair _ = false
    fun is_number (ref (NUMBER n)) = true |
        is_number _ = false
    fun is_string (ref (STRING s)) = true |
        is_string _ = false
    fun is_procedure (ref (PROCEDURE p)) = true |
        is_procedure _ = false
  
    fun is_list (ref (LIST l)) = true |
        is_list (ref (PAIR (a, r))) = is_list r |
        is_list _ = false
    fun is_null (ref (LIST nil)) = true |
        is_null _ = false
    fun is_input_port (ref (INPUT_PORT _)) = true |
        is_input_port _ = false
    fun is_output_port (ref (OUTPUT_PORT _)) = true |
        is_output_port _ = false
    fun is_eof_object (ref (CHAR "")) = true
      | is_eof_object _ = false
  
    fun object2boolean (ref (BOOL b)) = b |
        object2boolean _ = true
    fun not (ref (BOOL false)) = true |
        not _ = false

    fun mstring2string (FIXED s) = s |
        mstring2string (MUTABLE l) = implode (map ! l)
    fun vector2list v =
        let val length = Vector.length v
	    fun v2l (l, 0) = l
              | v2l (l, n) = v2l (Vector.sub (v, n-1) :: l, n-1)
        in v2l (nil, length)
        end

    fun is_eq (ref (BOOL b), ref (BOOL b')) = (b = b') |
        is_eq (ref (SYMBOL s), ref (SYMBOL s')) = (s = s') |
        is_eq (ref (CHAR c), ref (CHAR c')) = (c = c') |
        is_eq (d as ref (VECTOR v), d' as ref (VECTOR v')) = (d = d') |
        is_eq (d as ref (PAIR p), d' as ref (PAIR p')) = (d = d') |
        is_eq (d as ref (NUMBER n), d' as ref (NUMBER n')) = (d = d') |
        is_eq (d as ref (STRING s), d' as ref (STRING s')) = (d = d') |
        is_eq (d as ref (PROCEDURE p), d' as ref (PROCEDURE p')) = (d = d') |
        is_eq (ref (LIST nil), ref (LIST nil)) = true |
        is_eq (d as ref (LIST l), d' as ref (LIST l')) = (d = d') |
        is_eq _ = false
    fun is_eqv (ref (BOOL b), ref (BOOL b')) = (b = b') |
        is_eqv (ref (SYMBOL s), ref (SYMBOL s')) = (s = s') |
        is_eqv (ref (CHAR c), ref (CHAR c')) = (c = c') |
        is_eqv (d as ref (VECTOR v), d' as ref (VECTOR v')) = (d = d') |
        is_eqv (d as ref (PAIR p), d' as ref (PAIR p')) = (d = d') |
        is_eqv (ref (NUMBER n), ref (NUMBER n')) = (n = n') |
        is_eqv (d as ref (STRING s), d' as ref (STRING s')) = (d = d') |
        is_eqv (d as ref (PROCEDURE p), d' as ref (PROCEDURE p')) = (d = d') |
        is_eqv (ref (LIST nil), ref (LIST nil)) = true |
        is_eqv (d as ref (LIST l), d' as ref (LIST l')) = (d = d') |
        is_eqv _ = false
    fun is_equal (ref (BOOL b), ref (BOOL b')) = (b = b') |
        is_equal (ref (SYMBOL s), ref (SYMBOL s')) = (s = s') |
        is_equal (ref (CHAR c), ref (CHAR c')) = (c = c') |
        is_equal (ref (VECTOR v), ref (VECTOR v')) = 
	   let fun loop n = if n < 0 then true else 
		    is_equal (Vector.sub (v, n), Vector.sub(v', n)) andalso loop (n-1)
	   in if Vector.length v = Vector.length v' 
                 then loop (Vector.length v)
              else false
           end |
        is_equal (ref (PAIR (p11, p12)), ref (PAIR (p21, p22))) = 
           is_equal (p11, p21) andalso is_equal (p12, p22) |
        is_equal (ref (NUMBER n), ref (NUMBER n')) = (n = n') |
        is_equal (ref (STRING s), ref (STRING s')) = 
           (mstring2string s = mstring2string s') |
        is_equal (d as ref (PROCEDURE p), d' as ref (PROCEDURE p')) = (d = d') |
        is_equal (ref (LIST l), ref (LIST l')) = 
	   let fun list_eq (nil,nil') = true
                 | list_eq (a::b, a'::b') = is_equal (a,a') andalso list_eq (b,b')
	         | list_eq _ = false
           in list_eq (l, l')
           end |
        is_equal (ref (PAIR (l,r)), ref (LIST (l'::r'))) =
              is_equal (l,l') andalso is_equal (r, ref (LIST r')) |
        is_equal (ref (LIST (l::r)), ref (PAIR (l',r'))) =
              is_equal (l,l') andalso is_equal (ref (LIST r), r') |
        is_equal _ = false

    end
