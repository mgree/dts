;----------------------------------------------------------------------------
; Type management
;----------------------------------------------------------------------------

; manages monomorphic (simple) types

; Needed packages: union/find
;(load "union-fi.so")

;; type Type 
;;
;; gen-tvar:          () -> Type
;; gen-tvar-with-leq: Type -> Type
;; gen-type:          TCon x Type x ... x Type -> Type
;; tsym:              Type -> Symbol
;; tcon:              Type -> TCon + #f
;; targs:             Type -> Type*
;; type-show:         Type -> Dynamic
;; 
;; type TCon			(Arity)
;;
;; 'char:	      TCon      (0)
;; 'boolean:	      TCon	(0)
;; 'number:	      TCon	(0)
;; 'string:	      TCon	(0)
;; 'symbol:	      TCon	(0)
;; 'pair:	      TCon	(2)
;; 'vector:	      TCon	(2)
;; 'procedure:	      TCon      (n+1)
;;
;; Primitive Scheme types and type constructors        (identifier used)
;;
;; char:	      Type
;; boolean:	      Type
;; number:            Type
;; string:            Type                              (charseq)
;; symbol:	      Type
;; pair:              Type x Type -> Type
;; vector:            Type -> Type                      (array)
;; procedure:         Type x Type x ... x Type -> Type
;; dynamic:           Type


; Auxiliary routines

(define counter 0)
; global counter for identification and printing

(define (init-counter)
  ; (re-)initializes counter
  (set! counter 0))

(define (gen-sym)
  ; generates a new symbol (for printing purposes)
  (set! counter (+ counter 1))
  (string->symbol (string-append "t#" (number->string counter))))


; Generic operations 

; a type is a union-find element whose info part is
; -> (for nonvariable types) ( c . a) or
; -> (for typevariables) (#f . s), where:
; c is the type constructor (#f if it is a type variable);
; s is the type id, a symbol (only for type variables);
; a is a list of types representing the argument types (only for nonvariable
; types; '() if it is a type constant such as boolean)

(define (gen-tvar)
  ; generates a new type variable (with a new counter-id)
  (gen-element (cons #f (gen-sym))))

(define (gen-type . tcon-arg-list)
  ; generates a new type, given a list consisting of a type constructor and
  ; a argument types 
  ; Assumption: tcon-arg-list is non-empty
  (gen-element tcon-arg-list))

(define (gen-type/targs tcon targs)
  ; generates a new type, given a type constructor tcon and a list of 
  ; types targs
  (gen-element (cons tcon targs)))

(define (tcon type)
  ; returns the type constructor of a type (#f if a type variable)
  (car (info type)))

(define (targs type)
  ; returns the type arguments of a nonvariable type
  ; Assumption: type is a nonvariable type 
  (cdr (info type)))

(define tsym targs)
; returns the (printable) type symbol of a type variable
; Assumption: type is a type variable


; Type constants and type constructors

(define dynamic (gen-type 'dynamic))
(define null (gen-type 'null))
(define boolean (gen-type 'boolean))
(define char (gen-type 'char))
(define number (gen-type 'number))
(define charseq (gen-type 'string))
(define symbol (gen-type 'symbol))
(define (pair type-1 type-2) (gen-type 'pair type-1 type-2))
(define (array type) (gen-type 'vector type))
(define (procedure arg-type res-type) (gen-type 'procedure arg-type res-type))
(define (list-type type) 
  (let* ((new-type (gen-tvar))
	 (result-type (pair type new-type)))
    (asymm-link! new-type result-type)
    result-type))


; Equivalencing of types

(define link! 
  ; links two type elements and combines their information such that if
  ; the type constructor field of the second argument is defined then its 
  ; info field is returned (c.f., code for link in union-fi.ss)
  (link (lambda (info-1 info-2) 
	  (if (car info-2) info-2 info-1))))

;----------------------------------------------------------------------------
; Polymorphic type management
;----------------------------------------------------------------------------

; introduces parametric polymorphic types


;; forall: (Type -> Type) -> Type
;; instantiate-type: Type -> Type
;; copy-type: Type -> Type

; type constructor literal for polymorphic types

(define (forall tv-func)
  (gen-type 'forall tv-func))

(define (forall2 tv-func2)
  (forall (lambda (tv1)
	    (forall (lambda (tv2)
		      (tv-func2 tv1 tv2))))))

(define (forall3 tv-func3)
  (forall (lambda (tv1)
	    (forall2 (lambda (tv2 tv3)
		       (tv-func3 tv1 tv2 tv3))))))

(define (forall4 tv-func4)
  (forall (lambda (tv1)
	    (forall3 (lambda (tv2 tv3 tv4)
		       (tv-func4 tv1 tv2 tv3 tv4))))))

(define (forall5 tv-func5)
  (forall (lambda (tv1)
	    (forall4 (lambda (tv2 tv3 tv4 tv5)
		       (tv-func5 tv1 tv2 tv3 tv4 tv5))))))

; polymorphic types

(define unspecified (forall (lambda (tv) tv)))


; (polymorphic) instantiation

(define (instantiate-type type)
  ; instantiates type type and returns a generic instance
  (let* ((type-rep (find! type))
	 (type-con (tcon type-rep)))
    (if (eqv? type-con 'forall)
	(instantiate-type ((car (targs type-rep)) (gen-tvar)))
	type-rep)))

(define (copy-type type)
  ; make a copy of type
  ; Assumption: type is a monotype
  ; ***NOTE***: This version uses environments -- this is doesn't require
  ; an leq-field, but it's slow!
  (define (ct et-list type)
    ; et is an association list of "encountered types" together with the type
    ; they are mapped to
    (let* ((type-rep (find! type))
	   (assoc-binding (lookup type-rep et-list)))
      (if assoc-binding
	  (binding-value assoc-binding)
	  (let ((type-con (tcon type-rep))
		(type-args (targs type-rep)))
	    (pretty-print type-con)
	    (case type-con
	      ((null char boolean number string symbol dynamic #f) type-rep)
	      ((procedure vector pair)
	       (let* ((new-tvar (gen-tvar))
		      (c-type (gen-type/targs 
			       type-con 
			       (map (lambda (t) 
				      (ct (cons (cons type-rep new-tvar)
						et-list) t))
				    type-args))))
		 (asymm-link! new-tvar c-type)
		 c-type))
	      (else (error 'copy-type "Unknown or illegal type constructor: "
		       type-con)))))))
  (ct '() type))
  

; Input/output operations

(define type-depth 10)
;; defines depth for printing of types

(define (type-show type)
  ; returns a printable list representation of type up to depth n
  (define (ts type n)
    (let* ((type-rep (find! type))
	   (type-con (tcon type-rep))
	   (type-args (targs type-rep)))
      ; (pretty-print type-con)
      (if (< n 1)
	  '...
	  (case type-con
	    ((char boolean number string symbol dynamic) type-con)
	    ((null) '())
	    ((pair) (cons (ts (car type-args) (- n 1)) 
			  (ts (cadr type-args) (- n 1))))
	    ((procedure vector)
	     (cons type-con (map (lambda (t) (ts t (- n 1)))
				 type-args)))
	    ((forall) (let ((new-tvar (gen-tvar)))
			(cons (list (type-show new-tvar)) 
			      (ts ((car type-args) new-tvar) n))))
	    (else ;; type is a variable
	     (if type-con
		 (error 'type-show "Unknown type constructor: " type-con)
		 (tsym type-rep)))))))
  (ts type type-depth))


