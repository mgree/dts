; ----------------------------------------------------------------------------
; Parsing and abstract syntax definition for IEEE Standard Scheme
; ----------------------------------------------------------------------------

;; Needed packages: environment management
;(load "env-mgmt.ss")

; Needed routines: ast-gen, ast-con, ast-args

;; Abstract syntax
;;
;; VarDef
;;
;; Identifier =		Symbol - SyntacticKeywords
;; SyntacticKeywords =	{ ... } (see Section 7.1, IEEE Scheme Standard)
;;
;; Datum
;;
;; null:		()		-> Datum
;; boolean:	        (Boolean)	-> Datum
;; char:		(Char)		-> Datum
;; number:	        (Number)	-> Datum
;; symbol:              (Symbol)        -> Datum
;; string:	        (String)	-> Datum
;; vector:	        (Datum*)	-> Datum
;; pair:		(Datum Datum)	-> Datum
;;
;;
;; VarDef, Formals
;;
;; VarDef < Formals
;;
;; var-def:             (Symbol)          -> VarDef
;; null-def:            ()                -> Formals
;; pair-def:            (VarDef Formals)  -> Formals
;;
;; Expr
;;
;; Datum < Expr
;; Variable < Expr
;; Args < Expr
;;
;; variable:		(VarDef)                  -> Variable
;; identifier:		(Symbol)       		-> Variable
;; null-arg:            ()                        -> Args
;; pair-arg:            (Expr Args)            -> Args
;; procedure-call:	(Expr Args)		-> Expr
;; lambda-expression:	(Formals Expr+)		-> Expr
;; conditional:		(Expr Expr Expr)    	-> Expr
;; assignment:		(Variable Expr)		-> Expr
;; cond-expression:	(CondClause+)	        -> Expr
;; case-expression:	(Expr CaseClause*) 	-> Expr
;; and-expression:	(Expr*)			-> Expr
;; or-expression:	(Expr*)			-> Expr
;; let-expression:	(Binding* Expr+) -> Expr
;; named-let-expression: (VarDef Binding* Expr+) -> Expr
;; let*-expression:	(Binding* Expr+)  -> Expr
;; letrec-expression:	(Binding* Expr+)  -> Expr
;; begin-expression:	(Expr+) 			-> Expr
;; do-expression:	(IterDef* CondClause Expr*) -> Expr
;; empty:		()			-> Expr
;;
;; CondClause =		(Expr . Expr+)
;; CaseClause =		(Datum* . Expr+)
;; Binding =            (VarDef . Expr)
;; IterDef =		(VarDef Expr . Expr)
;;
;;
;; Definition
;;
;; Definition < Expr
;; 
;; definition:		(Variable Expr)	        -> Definition
;; function-definition: (Variable Formals Expr+)  -> Definition
;; begin-command:	(Definition*)		-> Definition
;;
;; 
;; Program =		Expr*


;; Abstract syntax operators

; Datum

;; (define null 0)
;; (define boolean 1)
;; (define char 2)
;; (define number 3)
;; (define string 4)
;; (define symbol 5)
;; (define vector 6)
;; (define pair 7)
;; 
;; ; Bindings
;; 
;; (define var-def 8)
;; (define null-def 29)
;; (define pair-def 30)
;; 
;; ; Arguments
;;
;; (define null-arg 31)
;; (define pair-arg 32)
;;
;; ; Expr
;; 
;; (define variable 9)
;; (define identifier 10)
;; (define procedure-call 11)
;; (define lambda-expression 12)
;; (define conditional 13)
;; (define assignment 14)
;; (define cond-expression 15)
;; (define case-expression 16)
;; (define and-expression 17)
;; (define or-expression 18)
;; (define let-expression 19)
;; (define named-let-expression 20)
;; (define let*-expression 21)
;; (define letrec-expression 22)
;; (define begin-expression 23)
;; (define do-expression 24)
;; (define empty 25)
;; 
;; ; Command
;; 
;; (define definition 26)
;; (define function-definition 27)
;; (define begin-command 28)


;; Lexical notions

(define syntactic-keywords
  ;; source: IEEE Scheme, 7.1, <expression keyword>, <syntactic keyword>
  '(lambda if set! begin cond and or case let let* letrec do
	   quasiquote else => define unquote unquote-splicing))


;; Parse routines

; Datum

; parse-datum: parses nonterminal <datum>

(define (parse-datum e)
  ;; Source: IEEE Scheme, sect. 7.2, <datum>
  ;; Note: "'" is parsed as 'quote, "`" as 'quasiquote, "," as
  ;; 'unquote, ",@" as 'unquote-splicing (see sect. 4.2.5, p. 18)
  ;; ***Note***: quasi-quotations are not permitted! (It would be
  ;; necessary to pass the environment to parse-datum.)
  (ast-gen 'literal e))

; VarDef

; parse-formal: parses nonterminal <variable> in defining occurrence position

(define (parse-formal e)
  ; e is an arbitrary object; returns pair (e . a) where a is an abstract syntax tree 
  ; for variable definition e, if e is a legal identifier symbol
  (if (symbol? e)
      (if (memq e syntactic-keywords)
	  (error 'parse-formal "Illegal identifier (keyword): " e)
	  (cons e (ast-gen 'var-def e)))
      (error 'parse-formal "Not an identifier: " e)))

; parse-formals: parses <formals>

(define (parse-formals formals)
  ;; parses <formals>; see IEEE Scheme, sect. 7.3
  ;; returns a pair (bindings . ast) where:
  ;; ast: is an ast for the formals
  ;; bindings: is a list of bindings mapping the identifier symbols to the 
  ;; corresponding variable-definition ast's
  (cond
   ((null? formals)
    (cons '() (ast-gen 'null-def)))
   ((pair? formals)
    (let* ((fst-formal (car formals))
	   (rem-formals (cdr formals))
	   (binding (parse-formal fst-formal))
	   (ident (car binding))
	   (fst-formal-ast (cdr binding))
	   (rbinds-rast (parse-formals rem-formals))
	   (rbinds (car rbinds-rast))
	   (rast (cdr rbinds-rast)))
      (cons
       (cons binding rbinds)
       (ast-gen 'pair-def fst-formal-ast rast))))
   (else
    (let* ((binding (parse-formal formals))
	   (ident (car binding))
	   (ast (cdr binding)))
      (cons (list binding) ast)))))


; Expr

; parse-expression: parses nonterminal <expression>

(define (parse-expression env e)
  (cond
   ((symbol? e)
    (parse-variable env e))
   ((pair? e)
    (let ((op (car e)) (args (cdr e)))
      (case op
	((quote) (parse-quote args))
	((lambda) (parse-lambda env args))
	((if) (parse-if env args))
	((set!) (parse-set env args))
	((begin) (parse-begin env args))
	((cond) (parse-cond env args))
	((case) (parse-case env args))
	((and) (parse-and env args))
	((or) (parse-or env args))
	((let) (parse-let env args))
	((let*) (parse-let* env args))
	((letrec) (parse-letrec env args))
	((do) (parse-do env args))
	((quasiquote) (parse-quasiquote env args))
        (else (parse-procedure-call env op args)))))
   (else (parse-datum e))))

; parse-expression*

(define (parse-expression* env exprs)
  ;; Parses lists of expressions (returns them in the correct order!)
  (map (lambda (e) (parse-expression env e)) exprs))

; parse-variable: parses variables (applied occurrences)

(define (parse-variable env e)
  (if (symbol? e)
      (if (memq e syntactic-keywords)
	  (error 'parse-variable "Illegal identifier (keyword): " e)
	  (let ((assoc-var-def (lookup e env)))
	    (if assoc-var-def
		(ast-gen 'variable assoc-var-def)
		(ast-gen 'identifier e))))
      (error 'parse-variable "Not an identifier: " e)))

; parse-procedure-call

(define (parse-procedure-call env op args)
  (ast-gen 'procedure-call
	   (parse-expression env op)
	   (parse-expression* env args)))

; parse-quote

(define (parse-quote args)
  (if (list-of-1? args)
      (parse-datum (car args))
      (error 'parse-quote "Not a datum (multiple arguments): " args)))

; parse-lambda

(define (parse-lambda env args)
  (if (pair? args)
      (let* ((formals (car args))
	     (body (cdr args))
	     (bindings-fast (parse-formals formals))
	     (bindings (car bindings-fast))
	     (fast (cdr bindings-fast)))
	(ast-gen 'lambda-expression
		 fast
		 (parse-body (extend-env-with-env env bindings) body)))
      (error 'parse-lambda "Illegal formals/body: " args)))

; parse-body

(define (parse-body env body)
  ; <body> = <definition>* <expression>+
  ; ***NOTE***: Looks ugly and murky; should be looked at again
  (define (def-var* found-bindings body)
    ; finds the defined variables in a body and returns bindings for them
    (if (pair? body)
        (let ((new-bindings (def-var found-bindings (car body))))
          (if new-bindings
              (def-var* new-bindings (cdr body))
              found-bindings))
        found-bindings))
  (define (def-var found-bindings clause)
    ; finds the defined variables in a single clause and extends
    ; found-bindings accordingly; returns false if it's not a definition
    (if (pair? clause)
        (case (car clause)
          ((define) (if (pair? (cdr clause))
                        (let ((pattern (cadr clause)))
                          (cond
			   ((symbol? pattern)
			    (extend-env-with-binding 
			     found-bindings 
			     pattern
			     (ast-gen 'var-def pattern)))
			   ((and (pair? pattern) (symbol? (car pattern)))
			    (extend-env-with-binding
			     found-bindings
			     (car pattern)
			     (ast-gen 'var-def (car pattern))))
			   (else found-bindings)))
                        found-bindings))
          ((begin) (def-var* found-bindings (cdr clause)))
          (else #f))
        #f))
  (if (pair? body)
      (parse-command* (extend-env-with-env env (def-var* '() body)) body)
      (error 'parse-body "Illegal body: " body)))

; parse-if

(define (parse-if env args)
  (cond
   ((list-of-3? args)
    (ast-gen 'conditional
	     (parse-expression env (car args))
	     (parse-expression env (cadr args))
	     (parse-expression env (caddr args))))
   ((list-of-2? args)
    (ast-gen 'conditional
	     (parse-expression env (car args))
	     (parse-expression env (cadr args))
	     (ast-gen 'empty)))
   (else (error 'parse-if "Not an if-expression: " args))))

; parse-set

(define (parse-set env args)
  (if (list-of-2? args)
      (ast-gen 'assignment
	       (parse-variable env (car args))
	       (parse-expression env (cadr args)))
      (error 'parse-set "Not a variable/expression pair: " args)))

; parse-begin

(define (parse-begin env args)
  (ast-gen 'begin-expression
	   (parse-body env args)))

; parse-cond

(define (parse-cond env args)
  (if (and (pair? args) (list? args))
      (ast-gen 'cond-expression
	       (map (lambda (e)
		      (parse-cond-clause env e))
		    args))
      (error 'parse-cond "Not a list of cond-clauses: " args)))

; parse-cond-clause

(define (parse-cond-clause env e)
  ;; ***NOTE***: Only (<test> <sequence>) is permitted!
  (if (pair? e)
      (cons
       (if (eqv? (car e) 'else)
	   (ast-gen 'empty)
	   (parse-expression env (car e)))
       (parse-body env (cdr e)))
      (error 'parse-cond-clause "Not a cond-clause: " e)))

; parse-and

(define (parse-and env args)
  (if (list? args)
      (ast-gen 'and-expression
	       (parse-expression* env args))
      (error 'parse-and "Not a list of arguments: " args)))

; parse-or

(define (parse-or env args)
  (if (list? args)
      (ast-gen 'or-expression
	       (parse-expression* env args))
      (error 'parse-or "Not a list of arguments: " args)))

; parse-case

(define (parse-case env args)
  (if (and (pair? args) (list? args))
      (ast-gen 'case-expression
	       (parse-expression env (car args))
	       (map (lambda (e)
		      (parse-case-clause env e))
		    (cdr args)))
      (error 'parse-case "Not a list of clauses: " args)))

; parse-case-clause

(define (parse-case-clause env e)
  (if (pair? e)
      (cons
       (cond
	((eqv? (car e) 'else)
	 (list (ast-gen 'empty)))
	((list? (car e))
	 (map parse-datum (car e)))
	(else (error 'parse-case-clause "Not a datum list: " (car e))))
       (parse-body env (cdr e)))
      (error 'parse-case-clause "Not case clause: " e)))

; parse-let

(define (parse-let env args)
  (if (pair? args)
      (if (symbol? (car args))
	  (parse-named-let env args)
	  (parse-normal-let env args))
      (error 'parse-let "Illegal bindings/body: " args)))

; parse-normal-let

(define (parse-normal-let env args)
  ;; parses "normal" let-expressions
  (let* ((bindings (car args))
	 (body (cdr args))
	 (env-ast (parse-parallel-bindings env bindings))
	 (nenv (car env-ast))
	 (basts (cdr env-ast)))
    (ast-gen 'let-expression
	     basts
	     (parse-body (extend-env-with-env env nenv) body))))

; parse-named-let

(define (parse-named-let env args)
  ;; parses a named let-expression
  (if (pair? (cdr args))
      (let* ((variable (car args))
	     (bindings (cadr args))
	     (body (cddr args))
	     (ident-vast (parse-formal variable))
	     (ident (car ident-vast))
	     (vast (cdr ident-vast))
	     (env-ast (parse-parallel-bindings env bindings))
	     (nenv (car env-ast))
	     (basts (cdr env-ast)))
	(ast-gen 'named-let-expression
		 vast basts
		 (parse-body (extend-env-with-env 
			      (extend-env-with-binding env ident vast)
			      nenv) body)))
      (error 'parse-named-let "Illegal named let-expression: " args)))


; parse-binding

(define (parse-binding env binding)
  ; parses a single binding
  ; returns: ((i . v) . (v . e)) where:
  ; i is identifier found
  ; v is a variable definition ast for i, 
  ; e is an expression ast bound to v;
  ; env: the environment in which binding is to be parsed
  ; binding: a pair of the form (<variable> <expression>)
  (let ((ident-varast (parse-formal (car binding)))
	(varast (cdr ident-varast)))
    (cons ident-varast (cons varast (parse-expression env (cadr binding))))))

; parse-parallel-bindings

(define (parse-parallel-bindings env bindings)
  ; returns ((i . v)* . (v . e)*) where:
  ; (i . v)* is the set of bindings mapping identifier i to variable ast v
  ; (v . e)* is the set of bindings mapping variable ast's to bound exp ast's
  (define (ppb f-env basts bindings)
    (if (null? bindings)
	(cons f-env (reverse basts))
	(let* ((fst-binding (car bindings))
	       (rem-bindings (cdr bindings))
	       (idvar-varbind (parse-binding env fst-binding))
	       (idvar (car idvar-varbind))
	       (varbind (cdr idvar-varbind)))
	  (ppb (cons idvar f-env)
	       (cons varbind basts)
	       rem-bindings))))
  (if (list-of-list-of-2s? bindings)
      (ppb '() '() bindings)
      (error 'parse-parallel-bindings
	     "Not a list of bindings: " bindings)))

; parse-let*

(define (parse-let* env args)
  ;; parses let*-expression
  (if (pair? args)
      (let* ((bindings (car args))
	     (body (cdr args))
	     (env-ast (parse-sequential-bindings env bindings))
	     (nenv (car env-ast))
	     (basts (cdr env-ast)))
	(ast-gen 'let*-expression
		 basts
		 (parse-body (extend-env-with-env env nenv) body)))
      (error 'parse-let* "Illegal bindings/body: " args)))

; parse-sequential-bindings

(define (parse-sequential-bindings env bindings)
  ; returns ((i . v)* . (v . e)*) where:
  ; (i . v)* is the set of bindings mapping identifier i to variable ast v
  ; (v . e)* is the set of bindings mapping variable ast's to bound exp ast's
  (define (psb f-env env basts bindings)
    (if (null? bindings)
	(cons f-env (reverse basts))
	(let* ((fst-binding (car bindings))
	       (rem-bindings (cdr bindings))
	       (idvar-varbind (parse-binding env fst-binding))
	       (idvar (car idvar-varbind))
	       (varbind (cdr idvar-varbind)))
	  (psb (cons idvar f-env)
	       (extend-env-with-binding env (car idvar) (cdr idvar))
	       (cons varbind basts)
	       rem-bindings))))
  (if (list-of-list-of-2s? bindings)
      (psb '() env '() bindings)
      (error 'parse-sequential-bindings
	     "Not a list of bindings: " bindings)))

; parse-letrec

(define (parse-letrec env args)
  (if (pair? args)
      (let* ((bindings (car args))
	     (body (cdr args))
	     (env-ast (parse-recursive-bindings env bindings))
	     (nenv (car env-ast))
	     (basts (cdr env-ast)))
	(ast-gen 'letrec-expression
		 basts
		 (parse-body (extend-env-with-env env nenv) body)))
      (error 'parse-letrec "Illegal bindings/body: " args)))

; parse-recursive-bindings

(define (parse-recursive-bindings env bindings)
  ; returns ((i . v)* . (v . e)*) where:
  ; (i . v)* is the set of bindings mapping identifier i to variable ast v
  ; (v . e)* is the set of bindings mapping variable ast's to bound exp ast's
  (if (list-of-list-of-2s? bindings)
      (let* ((idvars (map parse-formal (map car bindings)))
	     (exprasts (parse-expression*
			 (extend-env-with-env env idvars)
			 (map cadr bindings))))
	(cons idvars (map (lambda (idvar exprast) (cons (cdr idvar) exprast))
			  idvars exprasts)))
      (error 'parse-recursive-bindings "Illegal bindings: " bindings)))

; parse-do

(define (parse-do env args)
  ;; parses do-expressions
  ;; ***NOTE***: Not implemented!
  (error 'parse-do "Nothing yet..."))

; parse-quasiquote

(define (parse-quasiquote env args)
  ;; ***NOTE***: Not implemented!
  (error 'parse-quasiquote "Nothing yet..."))


;; Command

; parse-command

(define (parse-command c)
  (if (pair? c)
      (let ((op (car c))
	    (args (cdr c)))
	(case op
	  ((define) (parse-define args))
	  ((begin) (ast-gen 'begin-command (parse-command* args)))
	  (else (parse-expression empty-env c))))
      (parse-expression empty-env c)))

; parse-command*

(define (parse-command* commands)
  ;; parses a sequence of commands
  (if (list? commands)
      (map (lambda (command) (parse-command command)) commands)
      (error 'parse-command* "Invalid sequence of commands: " commands)))

; parse-define

;;; THIS IS HOW FAR I GOT -- FH, Thu, Nov. 25, 5:30 p.m.

(define (parse-define args)
  ;; three cases -- see IEEE Scheme, sect. 5.2
  ;; ***NOTE***: the parser admits forms (define (x . y) ...)
  ;; ***NOTE***: Variables are treated as applied occurrences!
  (if (pair? args)
      (let ((pattern (car args))
	    (exp-or-body (cdr args)))
	(cond
	 ((symbol? pattern)
	  (if (list-of-1? exp-or-body)
	      (ast-gen 'definition
		       (parse-variable env pattern)
		       (parse-expression env (car exp-or-body)))
	      (error 'parse-define "Not a single expression: " exp-or-body)))
	 ((pair? pattern)
	  (let* ((function-name (car pattern))
		 (function-arg-names (cdr pattern))
		 (env-ast (parse-formals function-arg-names))
		 (formals-env (car env-ast))
		 (formals-ast (cdr env-ast)))
	    (ast-gen 'function-definition
		     (parse-variable env function-name)
		     formals-ast
		     (parse-body (extend-env-with-env env formals-env) exp-or-body))))
	 (else (error 'parse-define "Not a valid pattern: " pattern))))
      (error 'parse-define "Not a valid definition: " args)))

;; Auxiliary routines

; forall?

(define (forall? pred list)
  (if (null? list)
      #t
      (and (pred (car list)) (forall? pred (cdr list)))))

; list-of-1?

(define (list-of-1? l)
  (and (pair? l) (null? (cdr l))))

; list-of-2?

(define (list-of-2? l)
  (and (pair? l) (pair? (cdr l)) (null? (cddr l))))

; list-of-3?

(define (list-of-3? l)
  (and (pair? l) (pair? (cdr l)) (pair? (cddr l)) (null? (cdddr l))))

; list-of-list-of-2s?

(define (list-of-list-of-2s? e)
  (cond
   ((null? e)
    #t)
   ((pair? e)
    (and (list-of-2? (car e)) (list-of-list-of-2s? (cdr e))))
   (else #f)))


;; Pretty-printing abstract syntax trees

(define (ast-show ast)
  ;; converts abstract syntax tree to list representation (Scheme program)
  (let ((op (ast-con ast))
	(args (ast-args ast)))
    (case op
      ((null) (list 'quote '()))
      ((boolean char number string var-def identifier ) (fst args))
      ((symbol) (list 'quote (fst args)))
      ((vector) (list->vector (map ast-show (fst args))))
      ((pair) (cons 'cons (map ast-show args)))
      ((null-def null-arg) '())
      ((pair-def pair-arg procedure-call) (cons (ast-show (fst args)) (ast-show (snd args))))
      ((variable) (ast-show (fst args)))
      ((lambda-expression) (cons 'lambda (cons (ast-show (fst args)) 
					       (map ast-show (snd args)))))
      ((conditional) (cons 'if (cons (ast-show (fst args))
				     (cons (ast-show (snd args))
					   (let ((alt (trd args)))
					     (if (eqv? (ast-con alt) 'empty)
						 '()
						 (list (ast-show alt))))))))
      ((assignment) (cons 'set! (map ast-show args)))
      ((cond-expression) (cons 'cond
			       (map (lambda (cc)
				      (let ((guard (car cc))
					    (body (cdr cc)))
					(cons
					 (if (eqv? (ast-con guard) 'empty)
					     'else
					     (ast-show guard))
					 (map ast-show body))))
				    (fst args))))
      ((case-expression) (cons 'case
			       (cons (ast-show (fst args))
				     (map (lambda (cc)
					    (let ((data (car cc)))
					      (if (and (pair? data)
						       (eqv? (ast-con (car data)) 'empty))
						  (cons 'else
							(map ast-show (cdr cc)))
						  (cons (map datum-show data)
							(map ast-show (cdr cc))))))
					  (snd args)))))
      ((and-expression) (cons 'and (map ast-show (fst args))))
      ((or-expression) (cons 'or (map ast-show (fst args))))
      ((let-expression) (cons 'let
			      (cons (map (lambda (binding)
					   (list (ast-show (car binding)) 
						 (ast-show (cdr binding))))
					 (fst args))
				    (map ast-show (snd args)))))
      ((named-let-expression) (cons 'let
				    (cons (ast-show (fst args))
					  (cons (map
						 (lambda (binding)
						   (list (ast-show (car binding))
							 (ast-show (cdr binding))))
						 (snd args))
						(map ast-show (trd args))))))
      ((let*-expression) (cons 'let*
			      (cons (map (lambda (binding)
					   (list (ast-show (car binding)) 
						 (ast-show (cdr binding))))
					 (fst args))
				    (map ast-show (snd args)))))
      ((letrec-expression) (cons 'letrec
			      (cons (map (lambda (binding)
					   (list (ast-show (car binding)) (ast-show (cdr binding))))
					 (fst args))
				    (map ast-show (snd args)))))
      ((begin-expression) (cons 'begin
				(map ast-show (fst args))))
      ((do-expression) (error 'ast-show "Do expressions not handled: ~s" ast))
      ((empty) (error 'ast-show "This can't happen: empty encountered!"))
      ((definition) (cons 'define (map ast-show args)))
      ((function-definition) (cons 'define
				   (cons
				    (cons (ast-show (fst args))
					  (ast-show (snd args)))
				    (map ast-show (trd args)))))
      ((begin-command) (cons 'begin
			     (map ast-show (fst args))))
      (else (error 'ast-show "Unknown abstract syntax operator: ~s" op)))))

;; datum-show

(define (datum-show ast)
  ;; prints an abstract syntax tree as a datum
  (case (ast-con ast)
    ((null) (list 'quote '()))
    ((boolean char number string symbol) (fst (ast-args ast)))
    ((vector) (list->vector (map datum-show (fst (ast-args ast)))))
    ((pair) (cons (datum-show (fst (ast-args ast))) (datum-show (snd (ast-args ast)))))
    (else (error 'datum-show "This should not happen!"))))

;; Program conversion: concrete <-> abstract

; parse-program

(define (parse-program p)
  ;; parses a program; i.e. a sequence of commands
  (parse-command* empty-env p))

;; unparse-program

(define (unparse-program asts)
  ;; unparses an abstract syntax representation of a program into a
  ;; list of S-expressions
  (map ast-show asts))

;; I/O processing

; read*

(define (read* port)
  ;; reads a list of S-expressions from port until input is exhausted;
  ;; returns the list
  (let ((next-input (read port)))
    (if (eof-object? next-input)
	'()
	(cons next-input (read* port)))))

; write*

(define (write* l port)
  ;; writes a list l of S-expressions to a port
  ;; ***NOTE***: Assumes that l is a list; uses pretty-print;
  ;; returns '()
  (if (null? l)
      '()
      (begin
	(pretty-print (car l) port)
	(write* (cdr l) port))))

; input-program

(define (input-program file-name)
  ;; file-name is a string containing a Scheme source code representation
  ;; returns the program as a list of S-expressions
  (call-with-input-file file-name read*))

; output-program

(define (output-program p file-name)
  ;; writes (concrete syntax) program p to file-name
  (call-with-output-file file-name
			 (lambda (port) (write* p port))))
