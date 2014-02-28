#lang racket

;Srikanth Manikarnike
;U0706564

(require "pytoc-common.rkt")

; global-cc : set -> void
(define globals-cc (set))
(define env-defines (make-hash))

; cc-global : symbol -> bool
(define (cc-global? symbol)
  (set-member? globals-cc symbol))

; closure-convert : exp -> exp
(define (closure-convert exp)
  (match exp
    
    [`(lambda ,params ,body)
      (define $env (gensym '$env_t))
      (define params* (cons '$env params))
      (define fv (free exp))
      (define ed2 (for/list ([v fv] #:when (not (cc-global? v)))
                    v))
      (hash-set! env-defines $env ed2)
      (define env (for/list ([v fv] #:when (not (cc-global? v)))
                    (list v v)))
      (define sub (for/hash ([v fv] #:when (not (cc-global? v)))
                    (values v `(env-ref ,$env $env ,v))))
      (define body* (substitute sub body))
     `(make-closure (lambda ,params* ,body*)
                    (make-env ,$env ,@env))]
    
    [(? basic?)   exp]
    
    [(? symbol?)
     exp]
    
    [(? atomic?) exp]
    
    [`(lambda ,params ,body)
     exp]
    
    [`(set-then! ,v ,e ,body)  exp]
    
    [`((cps ,_) ,operands ...) exp]

    
    [`(error . ,exps)       exp]
    
    [`(set . ,exps)         exp]
    [`(tuple . ,exps)       exp]
    [`(py-list* . ,exps)    exp]
    [`(dict (,ks ,vs) ...)  exp]
    
    [`(if ,e1 ,e2 ,e3)      exp]
    
    [`(make-cell ,val)         exp]
    [`(get-cell ,c)            exp]
    [`(set-cell! ,c ,e ,body)  exp]
    
    [`(make-closure ,lam ,env)
     exp]
    
    [`(make-env ,var (,vs ,es) ...)
     exp]
    
    [`(env-ref ,env $env ,v)
     exp]
    
    [`(app* ,f ,args ...)
     exp]

    [`(,f ,args ...)
     `(app* ,f . ,args)]))
                   
; transform/bottom-up : f exp -> exp
(define (transform/bottom-up f exp)
  
  (define (t e) (transform/bottom-up f e))
  
  (let ([exp* (match exp
                [`(lambda ,params ,body)
                 `(lambda ,params ,(t body))]
                [(? symbol?) 
                 exp]
                
                [(? atomic?) exp]
                
                [`(make-closure ,lam ,env)
                 `(make-closure ,(t lam) ,(t env))]
                
                [`(make-env ,var (,vs ,es) ...)
                 `(make-env ,var ,@(map list vs (map t es)))]
                
                [`(env-ref ,env $env ,v)
                 `(env-ref ,(t env) ,v)]
                
                [`(if ,aexp ,cexp1 ,cexp2) `(if ,(t aexp) ,(t cexp1) ,(t cexp2))]
                
                [`(app* ,f ,args ...)
                 `(app* ,(t f) ,(map t args))]
                
                [`(,f ,args ...)
                 `(,(t f) ,@(map t args))]
                [else else])])
    (f exp*)))

(define (add-to-global-list def)
  (match def
    [`(define break (void)) (set! globals-cc globals-cc)]
    [`(define return (void)) (set! globals-cc globals-cc)]
    [`(define continue (void)) (set! globals-cc globals-cc)]
    [`(define $current-handler (void)) (set! globals-cc globals-cc)]
    [`(define ,var-def ,aexp) (set! globals-cc (set-add globals-cc var-def)) def]))

(define defines-expanded '())
(define cc-expression '())
(define (flat-closure-convert program)
  (match program
   [`(program ,defs ... ,exp)
    ; =>
    (map add-to-global-list defs)
    (set! cc-expression (transform/bottom-up closure-convert exp))
    (hash-for-each env-defines (lambda (x y) (set! defines-expanded (cons `(define-env ,x ,y) defines-expanded))))
    `(program ,@defines-expanded ,@defs ,cc-expression)]))

(pretty-write (flat-closure-convert (read)))