(
(define map (lambda (f l)
                    (if (null? l)
                        '()
                        (cons (f (car l)) (map f (cdr l))))))
)

