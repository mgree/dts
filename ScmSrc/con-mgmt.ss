;----------------------------------------------------------------------------
;	Constraint management 
;----------------------------------------------------------------------------


; Needed packages: type management
;(load "typ-mgmt.so")

; coercions

(define (coercion type1 type2)
  ; returns coercion type1->type2 and adds corresponding
  ; constraints
  (add-coercion-constr! type1 type2)
  (cons type1 type2))

(define low-type car)
; returns the "low" type from a coercion

(define high-type cdr)
; returns the "high" type from a coercion

(define (coercion-show c)
  ; returns a displayable representation of c
  (list (type-show (low-type c))
	'->
	(type-show (high-type c))))
		   

; constraints

(define gen-constr cons)
; generates an inequality between arguments type1 and tv2

(define constr-lhs car)
; returns the left-hand side of an inequality constraint

(define constr-rhs cdr)
; returns the right-hand side of an inequality constraint

(define (constr-show c)
  (list (type-show (car c)) 
	'<=> 
	(type-show (cdr c))))

; constraint set management

(define global-equality-constraints '())

(define (init-global-equality-constraints!)
  (set! global-constraints '()))

(define (add-equality-constr! lhs rhs)
  (set! global-equality-constraints
	(cons (gen-constr lhs rhs) global-equality-constraints))
  '())

(define (global-equality-constraints-show) 
  ; returns printable version of global constraints
  (map constr-show global-equality-constraints))


(define global-coercion-constraints '())

(define (init-global-coercion-constraints!)
  (set! global-constraints '()))

(define (add-coercion-constr! lhs rhs)
  (set! global-coercion-constraints
	(cons (gen-constr lhs rhs) global-coercion-constraints))
  '())

(define (global-coercion-constraints-show) 
  ; returns printable version of global constraints
  (map constr-show global-coercion-constraints))


; constraint normalization

(define (normalize-global-constraints!) 
  ; solves equality and coercion constraints
  ; ***NOTE***: The following code is tricky and only works for the "simple"
  ; inference case (i.e., completion class C_pf)
  (for-each normalize! global-equality-constraints)
  (set! global-equality-constraints
	(map (lambda (c) (gen-constr (copy-type (low-type c))
				     (copy-type (high-type c))))
	     global-coercion-constraints))
  (set! global-coercion-constraints '())
  (for-each normalize! global-equality-constraints)
  (set! global-equality-constraints '())
  '())

(define (normalize! c)
  ; normalize constraint c
  (equiv! (low-type c) (high-type c)))

(define (equiv! tv1 tv2)
  ; equivalences types tv1 and tv2
  (let ((tv1-rep (find! tv1))
	(tv2-rep (find! tv2)))
    (cond
     ((eqv? tv1-rep tv2-rep)
      '())
     ((eqv? tv2-rep dynamic)
      (equiv-with-dynamic! tv1-rep))
     ((eqv? tv1-rep dynamic)
      (equiv-with-dynamic! tv2-rep))
     (else ; tv1-rep and tv2-rep are distinct, nondynamic class representatives
      (let ((tv1-con (tcon tv1-rep))
	    (tv2-con (tcon tv2-rep)))
	(if (and tv1-con tv2-con)
	    (if (eqv? tv1-con tv2-con)
		(begin
		  (link! tv1-rep tv2-rep)
		  (map equiv! (targs tv1-rep) (targs tv2-rep)))
		(begin
		  (equiv-with-dynamic! tv1-rep)
		  (equiv-with-dynamic! tv2-rep)))
	    (link! tv1-rep tv2-rep))))))
  '())

(define (equiv-with-dynamic! tv)
  ; equivalences type tv with dynamic
  (let ((tv-rep (find! tv)))
    (if (not (eqv? tv-rep dynamic))
	(begin
	  (asymm-link! tv-rep dynamic)
	  (if (tcon tv-rep)
	      (map equiv-with-dynamic! (targs tv-rep)))))))
