; ----------------------------------------------------------------------------
; Top level type environment
; ----------------------------------------------------------------------------


; Needed packages: type management (monomorphic and polymorphic)

;(load "typ-mgmt.ss")
;(load "ptyp-mgm.ss")


; type environment for miscellaneous

(define misc-env
  (list
   (cons 'quote (forall (lambda (tv) tv)))
   (cons 'eqv? (forall (lambda (tv) (procedure (pair tv tv) boolean))))
   (cons 'eq?  (forall (lambda (tv) (procedure (pair tv tv) boolean))))
   (cons 'equal?  (forall (lambda (tv) (procedure (pair tv tv) boolean))))
   ))

; type environment for input/output

(define io-env
  (list
   (cons 'open-input-file (procedure (pair charseq null) dynamic))
   (cons 'eof-object? (procedure (pair dynamic null) boolean))
   (cons 'read (forall (lambda (tv) (procedure (pair tv null) dynamic))))
   (cons 'write (forall (lambda (tv) (procedure (pair tv null) dynamic))))
   (cons 'display (forall (lambda (tv) (procedure (pair tv null) dynamic))))
   (cons 'newline (procedure null dynamic))
   (cons 'pretty-print (forall (lambda (tv)
				 (procedure (pair tv null) dynamic))))))


; type environment for Booleans

(define boolean-env
  (list
   (cons 'boolean? (forall (lambda (tv)
			     (procedure (pair tv null) boolean))))
   (cons #f boolean)
   ; #f doesn't exist in Chez Scheme, but gets mapped to null!
   (cons #t boolean)
   (cons 'not (procedure (pair boolean null) boolean))
   ))


; type environment for pairs and lists

(define list-env
  (list
   (cons 'pair? (forall (lambda (tv)
			  (procedure (pair tv null) boolean))))
   (cons 'null? (forall (lambda (tv)
			  (procedure (pair tv null) boolean))))
   (cons 'list? (forall (lambda (tv)
			  (procedure (pair tv null) boolean))))
   (cons 'cons (forall2 (lambda (tv1 tv2)
			  (procedure (pair tv1 (pair tv2 null))
				     (pair tv1 tv2)))))
   (cons 'car (forall2 (lambda (tv1 tv2)
			 (procedure (pair (pair tv1 tv2) null) tv1))))
   (cons 'cdr (forall2 (lambda (tv1 tv2)
			 (procedure (pair (pair tv1 tv2) null) tv2))))
   (cons 'set-car! (forall2 (lambda (tv1 tv2)
			      (procedure (pair (pair tv1 tv2) (pair tv1 null))
					 unspecified))))
   (cons 'set-cdr! (forall2 (lambda (tv1 tv2)
			      (procedure (pair (pair tv1 tv2) (pair tv2 null))
					 unspecified))))
   (cons 'cadr (forall3 (lambda (tv1 tv2 tv3)
			  (procedure (pair (pair tv1 (pair tv2 tv3)) null)
				     tv2))))
   (cons 'cddr (forall3 (lambda (tv1 tv2 tv3)
			  (procedure (pair (pair tv1 (pair tv2 tv3)) null)
				     tv3))))
   (cons 'caar (forall3 (lambda (tv1 tv2 tv3)
			  (procedure (pair (pair (pair tv1 tv2) tv3) null)
				     tv1))))
   (cons 'cdar (forall3 (lambda (tv1 tv2 tv3)
			  (procedure (pair (pair (pair tv1 tv2) tv3) null)
				     tv2))))
   (cons 'caaar (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (pair (pair (pair (pair tv1 tv2) tv3) tv4) null)
			      tv1))))
   (cons 'cdaar (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (pair (pair (pair (pair tv1 tv2) tv3) tv4) null)
			      tv2))))
   (cons 'cadar (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (pair (pair (pair tv1 (pair tv2 tv3)) tv4) null)
			      tv2))))
   (cons 'cddar (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (pair (pair (pair tv1 (pair tv2 tv3)) tv4) null)
			      tv3))))
   (cons 'caadr (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (pair (pair tv1 (pair (pair tv2 tv3) tv4)) null)
			      tv2))))
   (cons 'cdadr (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (pair (pair tv1 (pair (pair tv2 tv3) tv4)) null)
			      tv3))))
   (cons 'caddr (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (pair (pair tv1 (pair tv2 (pair tv3 tv4))) null)
			      tv3))))
   (cons 'cdddr (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (pair (pair tv1 (pair tv2 (pair tv3 tv4))) null)
			      tv4))))
   (cons 'cadddr
         (forall5 (lambda (tv1 tv2 tv3 tv4 tv5)
                    (procedure (pair (pair tv1
					   (pair tv2
						 (pair tv3
						       (pair tv4 tv5)))) null)
			       tv4))))
   (cons 'cddddr
         (forall5 (lambda (tv1 tv2 tv3 tv4 tv5)
                    (procedure (pair (pair tv1
					   (pair tv2
						 (pair tv3
						       (pair tv4 tv5)))) null)
			       tv5))))
   (cons 'list (forall (lambda (tv)
			 (procedure tv tv))))
   (cons 'length (forall (lambda (tv)
			   (procedure (pair (list-type tv) null) number))))
   (cons 'append (forall (lambda (tv)
			   (procedure (list-type (list-type tv)) 
				      (list-type tv)))))
   (cons 'reverse (forall (lambda (tv)
			    (procedure (pair (list-type tv) null) 
				       (list-type tv)))))
   (cons 'list-ref (forall (lambda (tv)
			     (procedure (pair (list-type tv)
					      (pair number null))
					tv))))
   (cons 'memq (forall (lambda (tv)
			 (procedure (pair tv (pair (list-type tv) null))
				    boolean))))
   (cons 'memv (forall (lambda (tv)
			 (procedure (pair tv (pair (list-type tv) null))
				    boolean))))
   (cons 'member (forall (lambda (tv)
			   (procedure (pair tv (pair (list-type tv) null))
				      boolean))))
   (cons 'assq (forall2 (lambda (tv1 tv2)
			  (procedure 
			   (pair tv1 (pair (list-type (pair tv1 tv2)) null))
			   (pair tv1 tv2)))))
   (cons 'assq (forall2 (lambda (tv1 tv2)
			  (procedure 
			   (pair tv1 (pair (list-type (pair tv1 tv2)) null))
			   (pair tv1 tv2)))))
   (cons 'assq (forall2 (lambda (tv1 tv2)
			  (procedure 
			   (pair tv1 (pair (list-type (pair tv1 tv2)) null))
			   (pair tv1 tv2)))))
   ))


(define symbol-env
  (list
   (cons 'symbol? (forall (lambda (tv)
			    (procedure (pair tv null) boolean))))
   (cons 'symbol->string (procedure (pair symbol null) charseq))
   (cons 'string->symbol (procedure (pair charseq null) symbol))
   ))

(define number-env
  (list
   (cons 'number? (forall (lambda (tv)
			    (procedure (pair tv null) boolean))))
   (cons '+ (procedure (pair number (pair number null)) number))
   (cons 'number->string (procedure (pair number null) charseq))
   (cons 'string->number (procedure (pair charseq null) number))
   ))

(define char-env
  (list
   (cons 'char? (forall (lambda (tv)
			  (procedure (pair tv null) boolean))))
   (cons 'char->integer (procedure (pair char null) number))
   (cons 'integer->char (procedure (pair number null) char))
   ))

(define string-env
  (list
   (cons 'string? (forall (lambda (tv)
			    (procedure (pair tv null) boolean))))
   ))

(define vector-env
  (list
   (cons 'vector? (forall (lambda (tv)
			    (procedure (pair tv null) boolean))))
   (cons 'make-vector (forall (lambda (tv)
				(procedure (pair number (pair tv null))
					   (array tv)))))
   (cons 'vector-length (forall (lambda (tv)
				  (procedure (pair (array tv) null)
					     number))))
   (cons 'vector-ref (forall (lambda (tv)
			       (procedure (pair (array tv) (pair number null))
					  tv))))
   (cons 'vector-set! (forall (lambda (tv)
				(procedure (pair (array tv) 
						 (pair number 
						       (pair tv null)))
					   unspecified))))
   ))

(define procedure-env
  (list
   (cons 'procedure? (forall (lambda (tv)
			       (procedure (pair tv null) boolean))))
   (cons 'map (forall2 (lambda (tv1 tv2)
			 (procedure (pair (procedure (pair tv1 null) tv2)
					  (pair (list-type tv1) null))
				    (list-type tv2)))))
   (cons 'for-each (forall2 (lambda (tv1 tv2)
			 (procedure (pair (procedure (pair tv1 null) tv2)
					  (pair (list-type tv1) null))
				    (list-type tv2)))))
   (cons 'call-with-current-continuation
	 (forall2 (lambda (tv1 tv2) 
		    (procedure 
		     (pair (procedure 
			    (pair (procedure (pair tv1 null) tv2) null)
			    tv1) null)
		     tv1))))
   ))


; global top level environment

(define (global-env)
  (append misc-env
	  io-env
	  boolean-env
	  symbol-env
	  number-env
	  char-env
	  string-env
	  vector-env
	  procedure-env
	  list-env))

(define top-level-env (global-env))

(define (init-top-level-env!)
  (set! top-level-env (global-env))
  '())

(define (top-level-env-show)
  ; displays the top level environment
  (map (lambda (binding)
	 (cons (key-show (binding-key binding))
	       (cons ': (type-show (binding-value binding)))))
       (env->list top-level-env)))

