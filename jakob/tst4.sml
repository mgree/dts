
datatype ('a, 'b, 'c, 'd) dyn = in_bool of bool
                             | in_nil of unit
                             | in_pair of 'a * 'b
                             | in_func of 'c * 'd

fun out_func(in_func f) = f
fun out_pair(in_pair x) = x
fun out_bool(in_bool x) = x

datatype ('a, 'c ,'d)recty = inrec of ('a, ('a, 'c ,'d)recty, 'c, 'd) dyn

fun outrec(inrec x) = x

fun car (x,y) = x

fun cons x y = (x, y)

fun cdr (x, y) = y

fun isnull x = case x of in_nil _ => true | _ => false


fun reverse l1 l2 =
            if isnull (outrec l1) 
            then l2
            else reverse (cdr (out_pair (outrec l1)))
                         (inrec (in_pair 
                                  (cons (car (out_pair (outrec l1))) l2)))

(* 
val reverse = fn : ('a,'b,'c) recty -> ('a,'d,'e) recty -> ('a,'d,'e) recty
*)
