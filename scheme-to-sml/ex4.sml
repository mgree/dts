(
(define reverse (lambda (l1 l2) 
                        (if (null? l1)
                            l2
                            (reverse (cdr l1) 
                                 (cons (car l1) l2))))) 

)



