; Grammar 

; basic types, types, coercion signatures :
; bas ::= {B,D}
; t   ::= bas | t o-> t'
; sig ::= t < t'

; Representation

; coercions :
; FUNC!, FUNC?, BOOL!, BOOL?, IOTAD, IOTAB
; Rep[c1;c2] = '(Rep[c1] & Rep[c2])
; Rep[c1o->c2] = '(Rep[c1] o-> Rep[c2])
; signatures :
; Rep[t1->t2] = '(Rep[t1] < Rep[t2])






(define false #f)
(define true #t)
(define (first x) (car x))
(define (second x) (car (cdr x)))
(define (third x) (car (cdr (cdr x))))


(define (dom sig)
        (first sig))
(define (ran sig)
        (third sig))


(define (o c1 c2)
        (list c1 '& c2))

(define (ar c1 c2)
        (list c1 'o-> c2))

(define (iscomp? c)
        (if (list? c)
            (if (> (length c) 1)
                (equal? (second c) '&)
                false)
            false))

(define (isarrow? ty)
        (if (list? ty)
            (if (> (length ty) 1)
                (equal? (second ty) 'o->)
                false)
            false))
            

(define (match? x y)
        (if (equal? x y)
            true
            (if (equal? x 'o->)
                (isarrow? y)
                (if (equal? y 'o->)
                    (isarrow? x)
                    false))))

(define (pat? sig d r)
        (and (match? (dom sig) d) (match? (ran sig) r)))

(define (simple sig)
        (if (pat? sig 'D 'D)
            'IOTAD
            (if (pat? sig 'D '(D o-> D))
                'FUNC?
                (if (pat? sig 'B 'D)
                    'BOOL!
                    (if (pat? sig '(D o-> D) 'D)
                        'FUNC!
                        (if (pat? sig 'B 'B)
                            'IOTAB
                            (if (pat? sig 'D 'B)
                                'BOOL?
                                false)))))))


; Takes a coercion signature sig and gives the unique
; canonical coercion with signature sig
(define (canon sig)
        (let ((sim (simple sig)))
             (if sim
                 sim
                 (if (pat? sig 'D 'o->)
                     (let ((c (canon (list (dom (ran sig)) '< 'D)))
                           (d (canon (list 'D '< (ran (ran sig))))))
                          (if (and c d)
                              (list 'FUNC? '& (list c 'o-> d))
                              false))
                     (if (pat? sig 'o-> 'D)
                         (let ((c (canon (list 'D '< (dom (dom sig)))))
                               (d (canon (list (ran (dom sig)) '< 'D))))
                              (if (and c d)
                                  (list (list c 'o-> d) '& 'FUNC!)
                                   false))
                         (if (pat? sig 'o-> 'B)
                             (let ((c (canon (list (dom sig) '< 'D))))
                                  (if c
                                      (list c '& 'BOOL?)
                                      false))
                             (if (pat? sig 'B 'o->)
                                 (let ((c (canon (list 'D '< (ran sig)))))
                                      (if c
                                          (list 'BOOL! '& c)
                                          false))
                                  (if (pat? sig 'o-> 'o->)
                                       (let ((c (canon (list (dom (ran sig))
                                                        '< (dom (dom sig)))))
                                             (d (canon (list (ran (dom sig))
                                                        '< (ran (ran sig))))))
                                        (if (and c d)
                                            (list c 'o-> d)
                                             false))
                                        false))))))))
; the false case is only for non-well formed signatures


; does c have form c =  ((c1 o-> c2) & (c3 o-> c4)) ?
(define (decomp? c)
        (if (iscomp? c)
            (and (isarrow? (first c)) (isarrow? (third c)))))
                 

; c = (c1 o-> c2) d = (d1 o-> d2)
; (decomp1 c d) = ((d1 & c1) o-> (c2 & d2))
(define (decomp1 c d)
        (ar (o (dom d) (dom c)) (o (ran c) (ran d))))




(canon '(((D o-> D) o-> B) < (B o-> D)))



