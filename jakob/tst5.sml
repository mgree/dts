
datatype ('a, 'b, 'c, 'd) dyn = in_bool of bool
                             | in_nil of unit
                             | in_pair of 'a * 'b
                             | in_func of 'c * 'd

fun out_func(in_func f) = f
fun out_pair(in_pair x) = x
fun out_bool(in_bool x) = x

datatype 'b recty = inrec of 'b * ('b, 'b recty, 'c, d)dyn

fun outrec(inrec x) = x

fun car (x,y) = x

fun cdr (x, y) = y

fun isnull x = case x of in_nil _ => true | _ => false

fun map f l  = inrec(
                     if isnull (outrec l) 
                     then in_nil ()
                     else in_pair (f (car (out_pair (outrec l))), 
                                   map f (cdr (out_pair (outrec l))))
                    )



(*
val map = fn : ('a -> 'b) -> ('a,'c,'d) recty -> ('b,'e,'f) recty

*)



val lst = inrec(in_pair(1, (inrec(in_pair(2, inrec(in_nil()))))))

val f = fn n => n+1

val m = map f lst
