(
(define zip (lambda (p1 p2)
                    (cons (cons (car p1) (car p2))
                          (cons (cdr p1) (cdr p2)))))

(define g (lambda (x) (if x (zip x '(1 . 1)) (zip x '(#f . #f)))))
)