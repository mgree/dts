













(
(define append (lambda (l1 l2)
                       (if (null? l1)
                            l2
                           (cons (car l1) 
                                 (append (cdr l1) l2)))))
)


val PAIR2LST = fn (x, y) => x::y;
val LST2PAIR = fn (x::y) => (x, y) | _ => raise EmptyList;

val isnull  = fn ([], [])       => true | _ => false;


 (* TRANSLATED PROGRAM : *) 

val rec append = 
        fn CV10 => 
           let val rec append = 
                       fn (l1, (l2, [])) => 
                          if (isnull (l1, [])) then 
                            l2
                          else 
                            (CV10 
                             (cons 
                              ((car (LST2PAIR l1, [])), 
                               ((append ((cdr (LST2PAIR l1, [])), 
                                        (l2, []))), [])
                              )
                             )
                            )
           in  append
           end; 

(* type

val append = fn : ('a * 'b -> 'b) -> 'a list * ('b * 'c list) -> 'b

*)

