(* TOP LEVEL DECLARATIONS : *)

datatype('a1, 'a2, 'a3, 'a4) dyn =
in_INT    of int |
in_BOOL   of bool |
in_STRING of string |
in_LST    of 'a1 list |
in_NIL    of unit |
in_PAIR   of 'a1 * 'a2 |
in_FUNC   of 'a3 -> 'a4;

exception TypeError;
exception EmptyList;

val ID           = fn x => x;
val ERR          = fn x => raise TypeError;
val check_BOOL   = fn in_BOOL x   => x | _ => raise TypeError;
val check_STRING = fn in_STRING x => x | _ => raise TypeError;
val check_INT    = fn in_INT x    => x | _ => raise TypeError;
val check_LST    = fn in_LST x    => x | _ => raise TypeError;
val check_PAIR   = fn in_PAIR x   => x | _ => raise TypeError;
val check_FUNC   = fn in_FUNC x    => x | _ => raise TypeError;
val PAIR2LST = fn (x, y) => x::y;
val LST2PAIR = fn (x::y) => (x, y) | _ => raise EmptyList;

val isnull  = fn (in_NIL(), []) => true | _ => false;
val isequal = fn (x, (y, []))   => x = y;
val car     = fn ((x, y), [])   => x | _ => raise TypeError;
val cdr     = fn ((x, y), [])   => y | _ => raise TypeError;
val cons    = fn ((x, (y, []))) => (x, y) | _ => raise TypeError;


 (* TRANSLATED PROGRAM : *) 

datatype ('a, 'b, 'c) REC0 = in_REC0 of ('a, ('a, 'b, 'c) REC0, 'b, 'c) dyn

fun out_REC0(in_REC0 x) = x

val rec map = 
        fn (CV5, CV6) => 
           let val rec map = 
                       fn (f, (l, [])) => 
                          (if (isnull (l, [])) then 
                             (in_REC0 ((in_NIL) ()))
                           else 
                             (in_REC0 ((in_PAIR) 
                              (cons 
                               ((f ((car ((CV5 l), [])), [])), 
                                ((map (f, ((cdr ((CV6 l), [])), []))), 
                                 []
                                )
                               )
                              )
                             )
                            )
                          )
           in  map
           end; 




(*  ML TYPE:

val map = fn
  : (('a,'b,'c,'d) dyn -> 'e * 'f)
    * (('a,'b,'c,'d) dyn -> 'g * ('a,'b,'c,'d) dyn)
    -> ('e * 'h list -> 'i) * (('a,'b,'c,'d) dyn * 'j list) -> ('i,'k,'l) REC0


*)

