(lambda x x)

(lambda (x) x)

(lambda (x y) (cons x y))

(lambda (x . z) (list x z))

(lambda (x) (if x x #f))

(lambda (x) (if #f x #f))
