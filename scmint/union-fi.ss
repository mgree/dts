;----------------------------------------------------------------------------
; Implementation of Union/find data structure in Scheme
;----------------------------------------------------------------------------

;; for union/find the following attributes are necessary: rank, paren
;; (see Tarjan, "Data structures and network algorithms", 1983)
;; In the Scheme realization an element is represented as a single
;; cons cell; its address is the element itself; the car field contains 
;; the parent, the cdr field is an address for a cons
;; cell containing the rank (car field) and the information (cdr field)


;; general union/find data structur
;; 
;; gen-element: Info -> Ele
;; find!: Elem -> Elem
;; link: (Info x Info -> Info) -> Elem! x Elem! -> Elem
;; asymm-link!: Elem! x Elem! -> Ele
;; info: Elem -> Info


(define (gen-element info)
  ; generates a new element: the parent field is initialized to '(),
  ; the rank field to 0
  (cons '() (cons 0 info)))

(define info cddr)
; returns the information stored in an element

(define (find! elem)
  ; finds the class representative of elem and sets the parent field 
  ; directly to the class representative (a class representative has
  ; '() as its parent)
  ;(display "Find!: ")
  ;(display (pretty-print (info elem))
  ;(newline)
  (let ((p-elem (car elem)))
    (if (null? p-elem)
	ele
	(let ((rep-elem (find! p-elem))
	  (set-car! elem rep-elem)
	  rep-elem))))

(define (link combine)
  ; combine: Info x Info -> Info, a function that combines the
  ; information from two elements
  (lambda (elem-1 elem-2)
    ; Precondition: elem-1, elem-2 must be distinct class representatives
    ; links elem-1 and elem-2 by rank and combines their info
    ; returns the class representative of the merged equivalence classes
    ;(display "Link!: "
    ;(display (pretty-print (list (info elem-1) (info elem-2))))
    ;(newline)
    (let ((rank-1 (cadr elem-1))
	  (rank-2 (cadr elem-2))
	  (info-1 (cddr elem-1))
	  (info-2 (cddr elem-2))
      (set-cdr! (cdr elem-2) (combine info-1 info-2))
      (cond
       ((= rank-1 rank-2
	(set-car! elem-1 elem-2
	(set-car! (cdr elem-2) (+ rank-2 1)
	elem-2)
       ((> rank-1 rank-2)
	(set-car! elem-2 elem-1)
	elem-1
       (else
	(set-car! elem-1 elem-2)
	elem-2)))))

(define asymm-link! set-car!)
; Assumption: inputs elem-1, elem-2 must be distinct class representative
; always links elem-1 to elem-2; result is unspecified


