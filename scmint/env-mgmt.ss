;---------------------------------------------------------------------------
; Environment managemen
;---------------------------------------------------------------------------
;; environments are lists of pairs, the first component being the key

;; general environment operations
;
;; empty-env: En
;; extend-env-with-binding: Env x Key x Value -> Env
;; extend-env-with-env: Env x (Key x Value)*! -> E
;; lookup: Key x Env -> (Value + False
;; env->list: Env -> Binding
;; env-show: Env -> Symbol
(define empty-env '())
; returns the empty environment
(define (extend-env-with-binding env key value)
  ; extends env with a binding, which hides any other binding in env
  ; for the same key (see lookup
  ; returns the extended environmen
  (cons (cons key value) env))

(define (extend-env-with-env env ext-env
  ; extends environment env with list of bindin
  ; a binding for a key in ext-env hides any binding in env fo
  ; the same key (see lookup); the list of bindings must not contain
  ; a key more than once;
  ; returns the extended environment
  (append ext-env env)) ;; NOTE: Add check for duplicate key
(define (lookup key env)
  ; returns the first pair in env that matches the key; returns #f
  ; if no such pair exists
  (let ((binding (assv key env)))
    (if binding
	(cdr binding)
	#f)))

(define (env->list e
  ; converts an environment to a list of binding
  e
