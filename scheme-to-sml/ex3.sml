(
(define append (lambda (l1 l2)
                       (if (null? l1)
                            l2
                           (cons (car l1) (append (cdr l1) l2)))))
)

