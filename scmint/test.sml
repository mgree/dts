local open Object SList Error
in
fun ID x = x
fun FUNC2UT (g, h, i) f =	
	(fn [x,y] => i (f (g x, h y))
          | _ => raise Impossible "")
fun FUNC2 c f = PROCEDURE_TAG (FUNC2UT c f)
val binds = [

	("memq", FUNC2 (ID, LIST_UNTAG, (fn nil => BOOL_TAG false | 
	         l => LIST_TAG l)) memq),
	("memv", FUNC2 (ID, LIST_UNTAG, (fn nil => BOOL_TAG false | 
	         l => LIST_TAG l)) memv),
	("member", FUNC2 (ID, LIST_UNTAG, (fn nil => BOOL_TAG false | 
	         l => LIST_TAG l)) member)

]
end;



