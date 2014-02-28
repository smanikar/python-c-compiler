; Srikanth Manikarnike
; U0706564

#lang racket
; primitive-operations is a set of symbols
;(define primitive-operations (apply set '(+ * / - py-print equal?)))
(define primitive-operations (apply set 
                                    '(py-list-set! dict-set! tuple-set! < > equal? >= <= not-equal? in? not-in? eq? not-eq? << >> + - * / quotient modulo expt assert2 bitwise-and bitwise-or bitwise-xor py-list-ref py-list-remove! tuple-ref tuple-set! dict-ref dict-remove! bitwise-not + - integer? string? tuple? dict? py-list? set? assert1 py-print not)))

(define cps-fun (apply set '(for-set for-py-list for-tuple for-dict)))

; primitive-operation? : symbol -> boolean
(define (primitive-operation? prim-op)
  (set-member? primitive-operations prim-op))

(define (cps-fun? f)
  (set-member? cps-fun f))
  
; partition-k : ('a -> boolean) 'a list ('a list 'a list -> 'a list 'a list)
(define (partition-k pred lst k)
  #f)
  
; define? : term -> boolean
(define (define? sx) 
  (match sx
    [`(define . ,_) #t]
    [else #f]))
  
; atomic? : exp -> boolean
(define (atomic? exp)
  (match exp
    [`(lambda . ,_) #t]
    [(? symbol?) #t]
    ['(void) #t]
    [(? number?) #t]
    [(? string?) #t]
    [(? boolean?) #t]
    ['None #t]
    ['Ellipsis #t]
    [`(py-list* . ,_) #t]
    [`(dict (,_ ,__) ... ) #t]
    [`(tuple . ,_) #t]
    [else #f]))
  
; cps-transform-program : cps-transforms a desugared program
(define (cps-transform-program program)
  (match program
    [`(program ,exprs ...)
     `(program ,@(map (λ (x) (if (define? x) 
                                 (cps-transform-def x)
                                 (cps-transform-q x '$halt))) exprs))]
    [else
     (error "error")]))
                              
; cps-transform-def : cps transforms a variable definition
(define (cps-transform-def def)
  (match def
    [`(define ,var ,aexp)
     `(define ,var ,(cps-atom aexp))]
    [else 
     (error "error")]))
  
; app : construct a call form
; ex: (app 'f 1 2 'x) => '(f 1 2 x)
(define (app f . args)
  #f)

; cont->kont converts a syntactic continuation
;            into a meta-continuation
; ex: (cont->kont '(λ (x) (f x))
;       =>
;     (λ (rv) `((λ (x) (f x)) ,rv)
(define (cont->kont q)
;  (define $rv (gensym 'rv))
;    (lambda ($rv)
;      `(,q ,$rv)))
    #f)

; kont->cont converts a meta-continuation
;            into a syntactic continuation
; ex: (kont->cont k) 
;       =>
;    `(λ ($rv123) ,(k $rv123))
(define (kont->cont k)
  (define $rv (gensym 'rv))
  `(lambda (,$rv) ,(k $rv)))

; let-cont fakes a let form by immediately
;          applying a lambda form
(define (let-cont v exp body)
  #f)
; let-name binds an expression to a value if
;          duplicatin it code bloat code
(define (let-name exp k)
  #f)
; cps-atom converts an atomic value to an atomic cps value
(define (cps-atom exp)
  (match exp
    [`(lambda (,var ...) ,exp)
     ;=>
     (define $k (gensym 'k))
     `(lambda (,@var ,$k) ,(cps-transform-q exp $k))]
    
     [(or 'call/ec 'call/cc)
     '(lambda (f cc) (f (lambda (x k) (cc x)) cc))]
    
    [(? atomic?) exp]
    
    [else
     (error "not match in cps-atom\n")]))
  
; cps-transform-k converts an expression to cps and calls
;                 k with an atom holding its return value;
;                 k is a meta-continuation
(define (cps-transform-k exp k)
  (match exp
    [(? atomic?) (k (cps-atom exp))]
    
    [`(begin ,expr) 
     (cps-transform-k expr k)]
    
    [`(begin ,expr ,exprs ...)
     (cps-transform-k expr (λ (_)
                             (cps-transform-k `(begin ,@exprs) k)))]
    
 
    [`(set! ,var ,expr)
      (cps-transform-k expr (λ (aexp)
                  `(set-then! ,var ,aexp
                              ,(k '(void)))))]
    
  
    [`(,_ ,_ ...)
     (cps-transform-q exp (kont->cont k))]
    [else
     (error "not matched in cps-transform-k")]))
  
; cps-transform-q converts an expression to cps and
;                 inserts a call to q with its return value
;                 q is a syntactic continuation
(define (cps-transform-q exp q)
  (match exp
    [(? atomic?) `(,q ,(cps-atom exp))]
    
    [`(begin ,expr) (cps-transform-q expr q)]
    
    [`(begin ,expr ,exprs ...)
     (cps-transform-k expr (λ (_)
                             (cps-transform-q `(begin ,@exprs) q)))]
    [`(if ,exprc ,exprt ,exprf)
     (cps-transform-k exprc (λ (c) `(if ,c
                                       ,(cps-transform-q exprt q)
                                       ,(cps-transform-q exprf q))))]
    
    [`(set! ,var ,expr)
      (cps-transform-k expr (λ (aexp)
                  `(set-then! ,var ,aexp
                              (,q (void)))))]
    
    [`(letrec ([,vs ,as] ...) ,expr)
     `(letrec (,@(map list vs (map cps-atom as))) 
        ,(cps-transform-q expr q))]
    
    [`(,(and p (? primitive-operation?)) ,es ...)
      ; =>
     (cps-transform-k* es (λ ($es)
                `((cps ,p) ,@$es ,q)))]
    
     [`(,f ,es ...)
     (cps-transform-k f (λ ($f) (cps-transform-k* es (λ ($es)
                                                       `(,(if (primitive-operation? $f) 
                                                              `(cps ,$f) (if (cps-fun? $f) (string->symbol (string-append (symbol->string $f) "-k")) $f))
                                                         ,@$es ,q)))))]
    
    [else
     (error "not matched in cps-transform-q")]))
      
; cps-transform-k* : exp list (exp list -> answer) -> answer
(define (cps-transform-k* exps k)
  (cond
    [(null? exps) (k '())]
    [(pair? exps) 
     (cps-transform-k (car exps) 
                      (λ (hd) (cps-transform-k* (cdr exps) 
                                                (λ (tl) 
                                                  (k (cons hd tl))))))]))   
(define (cps f)
  (λ args
    (match args
      [`(,xs ... ,k)
       (k (apply f xs))])))

(pretty-write (cps-transform-program (read)))
