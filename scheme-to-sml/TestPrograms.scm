(define append 
   (lambda (x1 x2)
      (if (null? x1)
          x2
          (cons (car x1) (append (cdr x1) x2)))))
          