(
(define nlist (lambda (n) 
              (if (equal? n 0)
                  '()
                  (cons (nlist n) '()))))  
                   
)
