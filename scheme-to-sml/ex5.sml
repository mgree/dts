(
(define taut (lambda (prop)
                     (if (equal? prop #t) 
                         #t
                         (if (equal? prop #f)
                              #f
                             (if (taut (prop #t))
                                 (taut (prop #f))
                                  #f)))))
 
)

