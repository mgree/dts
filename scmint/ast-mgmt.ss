; ----------------------------------------------------------------------------
; Simple abstract syntax tree generation routines
; ----------------------------------------------------------------------------

;; Abstract syntax operations
(define ast-gen list)
  ;; generates an abstract syntax tree; the constructor must b
  ;; defined as one the definitions above

(define ast-con car)
;; extracts the ast-constructor from an abstract syntax tree
(define ast-args cdr)
;; extracts the list of ast-arguments from an abstract syntax tree

(define fst car)
(define snd cadr
(define trd caddr)
(define fth cadddr)
