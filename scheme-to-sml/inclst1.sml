














(
(define inclst  (lambda (lst)
                (if (null? lst)
                    '()
                    (cons (inc (car lst)) (inclst (cdr lst))))))
)



val isnull  = fn (in_LST [], [])       => true | _ => false;

val inc     = fn ((x:int, []))  => x + 1

 (* TRANSLATED PROGRAM : *) 

datatype ('a, 'b, 'c) REC0 = fold_REC0 of ('a, 
                                          ('a, 'b, 'c) REC0, 
                                          'b, 
                                          'c) dyn

val unfold_REC0 = fn fold_REC0 x => x

val rec inclst = 
        fn (lst, []) => 
           (if (isnull ((unfold_REC0 lst), [])) then 
              (fold_REC0 ((in_NIL) []))
            else 
              (fold_REC0 ((in_PAIR) 
               (cons 
                ((inc ((car (((check_PAIR) 
                              (unfold_REC0 lst)), [])), [])), 
                 ((inclst ((cdr (((check_PAIR) 
                                  (unfold_REC0 lst)), 
                                   [])), [])), [])
                )
               )
              ))
           ); 


(*  

val inclst = fn : (int,'a,'b) REC0 * 'c list -> (int,'d,'e) REC0

*)
