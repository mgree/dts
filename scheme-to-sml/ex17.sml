(
(define inclst  (lambda (lst)
                (if (null? lst)
                    '()
                    (cons (inc (car lst)) (inclst (cdr lst))))))
)
