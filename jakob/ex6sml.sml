
datatype('a1, 'a2, 'a3, 'a4) dyn =
in_INT of int |
in_BOOL of bool |
in_STRING of string |
in_LST of 'a1 list |
in_PAIR of 'a1 * 'a2 |
in_FUNC of 'a3 -> 'a4;
exception TypeError;
exception EmptyList;
val ID = fn x => x;
val check_BOOL = fn in_BOOL x => x | _ => raise TypeError;
val check_STRING = fn in_STRING x => x | _ => raise TypeError;
val check_INT = fn in_INT x => x | _ => raise TypeError;
val check_LST = fn in_LST x => x | _ => raise TypeError;
val check_PAIR = fn in_PAIR x => x | _ => raise TypeError;
val check_FUNC = fn in_FUNC x => x | _ => raise TypeError;
val PAIR2LST = fn (x, y) => x::y;
val LST2PAIR = fn (x::y) => (x, y) | _ => raise EmptyList;
val isnull = fn ([], []) => true | _ => false;
val isequal = fn (x, (y, [])) => x = y;
val car = fn ((x, y), []) => x | _ => raise TypeError;
val cdr = fn ((x, y), []) => y | _ => raise TypeError;
val cons = fn ((x, (y, []))) => (x, y) | _ => raise TypeError;
val rec lookup = 
        fn CV5 => 
           let val rec lookup = 
                       fn (key, (env, [])) => 
                          if (isnull (env, [])) then 
                            (CV5 "key not found!")
                          else 
                            if (isequal (key, 
                               ((car (LST2PAIR env, [])), [])))
                            then 
                              (car (LST2PAIR env, []))
                            else 
                              (lookup (key, ((cdr (LST2PAIR env, [])), [])))
           in  lookup
           end; 


(* type 

val lookup = fn : (string -> ''a) -> ''a * (''a list * 'b list) -> ''a

*)