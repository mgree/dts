














(
(define inclst  (lambda (lst)
                (if (null? lst)
                    '()
                    (cons (inc (car lst)) (inclst (cdr lst))))))
)




val PAIR2LST = fn (x, y) => x::y;
val LST2PAIR = fn (x::y) => (x, y) | _ => raise EmptyList;

val isnull  = fn ([], [])       => true | _ => false;

val inc     = fn ((x:int, []))  => x + 1

 (* TRANSLATED PROGRAM : *) 

val rec inclst = 
        fn (lst, []) => 
           (if (isnull (lst, [])) then 
              []
            else 
               (PAIR2LST 
               (cons 
                ((inc ((car (((LST2PAIR) lst), [])), [])), 
                 ((inclst ((cdr (((LST2PAIR) lst), [])), [])), [])
                )
               )
              )
             ); 


(*

val inclst = fn : int list * 'a list -> int list

*)





