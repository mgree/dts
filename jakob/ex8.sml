(
(define taut (lambda (prop)
                     (if (equal? prop #t) 
                         #t
                         (if (equal? prop #f)
                              #f
                             (if (taut (prop #t))
                                 (taut (prop #f))
                                  #f)))))
 
(define imp (lambda (A) (lambda (B) (if A B #t))))

(define tst (lambda (x) (taut ((imp x) x))))
)

