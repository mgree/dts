; ---------------------------------------------------------------------------
; Typed abstract syntax tree management: constraint generation, display, etc
; ----------------------------------------------------------------------------

;; Abstract syntax operations, incl. constraint generation

(define (ast-gen . args)
  ; generates a syntax tree of the form
  ; ((t1 . t2) o . a) representing the explicitly coercion annotated 
  ; syntax tree [t1 -> t2] o(a), where a is a list;
  ; special representations:
  ; -> applied occurrences of variables are of the for
  ; (c o v . cs) (with o either 'variable or 'identifier) representin
  ; [c] v[cs
  (cons (coercion (gen-tvar) (gen-tvar)) args))
(define ast-coercion car)

(define (ast-low-type ast
  ;; extracts the "low" type (type without coercion applied) of ast
  (low-type (ast-coercion ast)))

(define (ast-type ast)
  ;; extracts the ("high") type (type with coercion applied) of as
  (high-type (ast-coercion ast)))
(define ast-con cadr)
;; extracts the ast-constructor from an abstract syntax tree

(define ast-args cddr
;; extracts the ast-arguments from an abstract syntax tree

(define fst car
(define snd cadr)
(define trd caddr
(define (tast-check! tast
  ; type checks the coercion-annotated ast tast by adding
  ; type equalities required by the typing rules to the global constraint s
  (let ((op (ast-con tast)
	(args (ast-args tast))
	(low-type (ast-low-type tast)))
    (pretty-print op
    (case o
      ((null) (add-equality-constr! low-type (pair (gen-tvar) (gen-tvar))))
      ((null-arg null-def) (add-equality-constr! low-type null))
      ((pair pair-arg pair-def) 
       (add-equality-constr! low-type (pair (ast-type (fst args))
			     (ast-type (snd args))))
       (for-each tast-check! args))
      ((boolean) (add-equality-constr! low-type boolean)
      ((char) (add-equality-constr! low-type char))
      ((number) (add-equality-constr! low-type number))
      ((symbol) (add-equality-constr! low-type symbol))
      ((string) (add-equality-constr! low-type charseq))
      ((vector) (let ((new-tvar (gen-tvar)))
		  (for-each (lambda (ast) 
			      (add-equality-constr! (ast-type ast) new-tvar)
			      (tast-check! ast)
			    (fst args))
		  (add-equality-constr! low-type (array new-tvar))))
      ((var-def empty begin-command) '())
      ((variable) (add-equality-constr! low-type (ast-type (fst args)))
      ((identifier) (let ((id-binding (lookup (fst args) top-level-env)))
		      (if id-binding
			  (add-equality-constr! low-type (instantiate-typ
						 (binding-value id-binding)))
			  (set! top-level-env (extend-env-with-bindin
					       top-level-en
					       (gen-binding (fst args) low-type))))))
      ((procedure-call) (add-equality-constr! (ast-type (fst args)
			       (procedure (ast-type (snd args)) 
					  low-type)
			(for-each tast-check! args)
      ((lambda-expression) (add-equality-constr! low-type 
				  (procedure (ast-type (fst args)
					     (ast-type (tail (snd args)))))
			   (tast-check! (fst args)
			   (for-each tast-check! (snd args)))
      ((conditional) (add-equality-constr! (ast-type (fst args)) boolean)
		     (add-equality-constr! (ast-type (snd args)) low-type
		     (add-equality-constr! (ast-type (trd args)) low-type
		     (for-each tast-check! args)
      ((assignment definition) (add-equality-constr! (ast-type (fst args)) 
					    (ast-type (snd args)))
			       (tast-check! (snd args))
      ((cond-expression) (for-each (lambda (clause)
				     (add-equality-constr! (ast-type (car clause)
						  boolean)
				     (tast-check! (car clause)
				     (add-equality-constr! (ast-type (tail (cdr clause))
						  low-type)
				     (for-each tast-check! (cdr clause)))
				   (fst args)))
      ((case-expression) (let ((e-type (ast-type (fst args))))
			   (tast-check! (fst args))
			   (for-each (lambda (clause)
				       (for-each (lambda (datum)
						   (add-equality-constr! (ast-type datum) 
								e-type)
						   (tast-check! datum))
						 (car clause)
				       (add-equality-constr! (ast-type (tail (cdr clause)))
						    low-type)
				       (for-each tast-check! (cdr clause)))
				     (snd args))))
      ((and-expression or-expression) (for-each (lambda (expr)
						  (add-equality-constr! (ast-type expr)
							       boolean))
						(fst args)
				      (for-each tast-check! (fst args))
				      (add-equality-constr! low-type boolean))
      ((let-expression let*-expression letrec-expression)
       (for-each (lambda (binding
		   (add-equality-constr! (ast-type (car binding)) (ast-type (cdr binding))
		   (tast-check! (cdr binding)))
		 (fst args))
       (for-each tast-check! (snd args)
       (link! (ast-type (tail (snd args))) low-type)) 
      ((named-let-expression) (error 'tast-check! 
				     "Named let-expression not handled: "
				     tast))
      ((begin-expression) (for-each tast-check! (fst args))
			  (link! (ast-type (tail (fst args))) low-type))
      ((do-expression)(error 'type-check 
			     "Do-expression not handled: "
			     tast))
      ((function-definition) (add-equality-constr! (ast-type (fst args))
					  (procedure (ast-type (snd args))
						     (ast-type (tail (trd args))))
			     (tast-check! (snd args))
			     (for-each tast-check! (trd args)))
      (else (error 'type-check "Unknown syntax operator: " op)))))
;; typed-program-check
(define (typed-program-check! p
  ;; type checks a whole program; i.e., a list of tast'
  (for-each tast-check! p))
;; Auxiliary functions

;; ta
(define (tail l
  ;; returns the tail of a nonempty lis
  (if (null? (cdr l))
      (car l
      (tail (cdr l))))
;; Unparsing

(define (tast-show ast)
  ;; converts type annotated abstract syntax tree to annotated S-expression representation
  (let ((op (ast-con ast)
	(args (ast-args ast))
	(coercion (ast-coercion ast))
    (list (coercion-show coercion
	  (case op
	    ((null) (list 'quote '())
	    ((boolean char number string) (fst args))
	    ((symbol) (list 'quote (fst args))
	    ((vector) (list->vector (map tast-show (fst args)))
	    ((pair) (cons 'cons (map tast-show args)))
	    ((var-def) (list (fst args) ': (type-show (low-type coercion))))
	    ((null-def null-arg) '())
	    ((pair-def pair-arg procedure-call) (cons (tast-show (fst args)) (tast-show (snd args))))
	    ((variable) (let* ((assoc-var-def (fst args))
			       (thesymbol (fst (ast-args assoc-var-def)))
			       (coercion-list (cdr args))
			  (if (null? coercion-list)
			      thesymbol
			      (list thesymbol (map coercion-sho
						   coercion-list))))
	    ((identifier) (let* ((thesymbol (fst args)
				 (coercion-list (cdr args))
			    (if (null? coercion-list
				thesymbol
				(list thesymbol (map coercion-sho
						     coercion-list)))))
	    ((lambda-expression) (cons 'lambda (cons (tast-show (fst args)) 
						     (map tast-show (snd args)))))
	    ((conditional) (cons 'if (cons (tast-show (fst args))
					   (cons (tast-show (snd args)
						 (let ((alt (trd args)))
						   (if (eqv? (ast-con alt) 'empty)
						       '()
						       (list (tast-show alt))))))))
	    ((assignment) (cons 'set! (map tast-show args)))
	    ((cond-expression) (cons 'cond
				     (map (lambda (cc
					    (let ((guard (car cc))
						  (body (cdr cc)))
					      (con
					       (if (eqv? (ast-con guard) 'empty)
						   'els
						   (tast-show guard)
					       (map tast-show body))))
					  (fst args))))
	    ((case-expression) (cons 'case
				     (cons (tast-show (fst args)
					   (map (lambda (cc
						  (let ((data (car cc)))
						    (if (and (pair? data)
							     (eqv? (ast-con (car data)) 'empty)
							(cons 'else
							      (map tast-show (cdr cc)))
							(cons (map tdatum-show dat
							      (map tast-show (cdr cc)))))
						(snd args))))
	    ((and-expression) (cons 'and (map tast-show (fst args))
	    ((or-expression) (cons 'or (map tast-show (fst args)))
	    ((let-expression) (cons 'le
				    (cons (map (lambda (bindin
						 (list (tast-show (car binding)
						       (tast-show (cdr binding))))
					       (fst args))
					  (map tast-show (snd args)))))
	    ((named-let-expression) (cons 'le
					  (cons (tast-show (fst args)
						(cons (map
						       (lambda (binding)
							 (list (tast-show (car binding)
							       (tast-show (cdr binding)))
						       (snd args)
						      (map tast-show (trd args))))))
	    ((let*-expression) (cons 'let*
				     (cons (map (lambda (binding
						  (list (tast-show (car binding)) 
							(tast-show (cdr binding))))
						(fst args)
					   (map tast-show (snd args))))
	    ((letrec-expression) (cons 'letr
				       (cons (map (lambda (binding)
						    (list (tast-show (car binding)) 
							  (tast-show (cdr binding)))
						  (fst args)
					     (map tast-show (snd args)))))
	    ((begin-expression) (cons 'begin
				      (map tast-show (fst args)))
	    ((do-expression) (error 'tast-show "Do expressions not handled: ~s" ast))
	    ((empty) (error 'tast-show "This can't happen: empty encountered!"))
	    ((definition) (cons 'define (map tast-show args))
	    ((function-definition) (cons 'define
					 (cons
					  (cons (tast-show (fst args))
						(tast-show (snd args)
					  (map tast-show (trd args))))
	    ((begin-command) (cons 'beg
				   (map tast-show (fst args)))
	    (else (error 'tast-show "Unknown abstract syntax operator: ~s" op))))))

;; tdatum-sho
(define (tdatum-show ast)
  ;; prints an abstract syntax tree as a datu
  (let ((op (ast-con ast)
	(args (ast-args ast))
	(coercion (ast-coercion ast))
    (list (coercion-show coercion)
	  (case 
	    ((null) (list 'quote '())
	    ((boolean char number string symbol) (fst (ast-args ast)))
	    ((vector) (list->vector (map tdatum-show (fst (ast-args ast)))))
	    ((pair) (cons (tdatum-show (fst (ast-args ast))) (tdatum-show (snd (ast-args ast))))
	    (else (error 'tdatum-show "This should not happen!"))))))

(define (unparse-typed-program ast
  ;; unparses an abstract syntax representation of a typed program into 
  ;; list of S-expressions
  (map tast-show asts))
