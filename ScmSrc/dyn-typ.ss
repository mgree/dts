; ----------------------------------------------------------------------------
; Dynamic type inference for Scheme
; ----------------------------------------------------------------------------

; Needed packages:

(load "union-fi.ss")
(load "env-mgmt.ss")
(load "parse.ss")
(load "typ-mgmt.ss")
(load "con-mgmt.ss")
(load "tast-mgm.ss")
(load "tlev-mgm.ss")

(define (ic!) (init-global-constraints!))
(define (pc) (glob-constr-show))
(define (lc) (length global-constraints))
(define (n!) (normalize-global-constraints!))
(define (pt) (top-level-env-show))
(define (it!) (init-top-level-env!))
(define (io!) (set! tag-ops 0) (set! no-ops 0))
(define (i!) (ic!) (it!) (io!) '())


