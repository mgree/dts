
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

fun cdr (x, y) = y

fun isnull x = case x of in_nil _ => true | _ => false

fun map f l isnull = if isnull l then in_nil ()
              else in_pair (f (car (out_pair l)), 
                   (inrec (map f (outrec (cdr (out_pair l))) isnull)))


(*
val map = fn
  : ('a -> 'b)
    -> ('a,('a,'c,'d) recty,'c,'d) dyn
       -> (('a,('a,'c,'d) recty,'c,'d) dyn -> bool)
          -> ('b,('b,'e,'f) recty,'e,'f) dyn
*)

