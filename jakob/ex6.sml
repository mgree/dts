(
(define lookup (lambda (key env)
        (if (null? env)
            "key not found!"
            (if (equal? key (car env))
                (car env)
                (lookup key (cdr env)))))) 
)

