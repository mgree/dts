(
(define lookup (lambda (key env)
        (if (null? env)
            "key not found!"
            (if (equal? key (car env))
                (car env)
                (lookup key (cdr env)))))) 

(define salary (lambda (record) (car record)))

(define get_salary (lambda (name table getfn) (getfn (lookup name table))))
)

