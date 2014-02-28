(module pytoc-common
  racket/base
  
  (require racket/match)
  (require racket/set)
  
  (provide (all-defined-out))
  
  ; basic? : exp -> boolean
  ; basic expressions contain no sub-expressions
  (define (basic? exp)
    (match exp
      [`(void)      #t]
      [`(cps ,_)    #t]
      ['None        #t]
      ['Ellipsis    #t]
      ['$halt       #t]
      [#t           #t]
      [#f           #t]
      [(? string?)  #t]
      [(? number?)  #t]
      [(? symbol?)  #t]
      [`(lambda-label ,_) #t]
      [else         #f]))
  
  
  ; atomic? : exp -> boolean
  ; atomic expressions are guaranteed to terminate
  ; under evaluation; they also cause no side effects
  (define (atomic? exp)
    (match exp
      [`(void)      #t]
      [`(cps ,_)    #t]
      [`(,(or 'λ 'lambda) . ,_) #t]
      ['None        #t]
      ['Ellipsis    #t]
      ['$halt       #t]
      [#t           #t]
      [#f           #t]
      [(? string?)  #t]
      [(? number?)  #t]
      [(? symbol?)  #t]
      [`(set . ,_)       #t]
      [`(dict . ,_)      #t]
      [`(py-list* . ,_)  #t]
      [`(tuple . ,_)     #t]
      [`(lambda-label ,_) #t]
      [else         #f]))
  
  
  ; primitive-operations is a set of symbols
  (define primitive-operations
    (apply set
           '( <  > equal? >= <= not-equal? in? not-in? eq? not-eq?
                 << >>
                 + - 
                 * / quotient modulo
                 expt
                 assert2
                 bitwise-and bitwise-or bitwise-xor 
                 py-list-ref 
                 py-list-remove!
                 py-list-set!            
                 tuple-ref
                 tuple-set!
                 dict-ref
                 dict-set!
                 dict-remove!
                 bitwise-not + -
                 integer?
                 string?
                 tuple?
                 dict?
                 py-list?
                 set?
                 assert1
                 py-print)))
  
  ; primitive-operation? : symbol -> boolean
  (define (primitive-operation? prim-op)
    (set-member? primitive-operations prim-op))
  
  
  (define (set-union* sets)
    (if (null? sets)
        (set)
        (apply set-union sets)))
  
  ; free : exp -> symbol set
  (define (free exp)
    (match exp
      [(? symbol?)  (set exp)]
      [(? basic?)   (set)]
      
      [`(,(or 'λ 'lambda) ,vars ,body)  (set-subtract (free body) (apply set vars))]
      
      [`(error . ,exps)       (apply set-union (map free exps))]
       
      [`(set . ,exps)         (apply set-union (map free exps))]
      [`(tuple . ,exps)       (apply set-union (map free exps))]
      [`(py-list* . ,exps)    (apply set-union (map free exps))]
      [`(dict (,ks ,vs) ...)  (apply set-union (map free (append ks vs)))]
      
      [`(error ,exp)        (free exp)]
      [`(if ,e1 ,e2 ,e3)    (set-union (free e1) (free e2) (free e3))]
      
      [`(set-then! ,v ,e ,body)  (set-union (free body) (free e) (set v))]
      
      [`(make-cell ,val)         (free val)]
      [`(get-cell ,c)            (free c)]
      [`(set-cell! ,c ,e ,body)  (set-union (free body) (free e) (free c))]
      
      [`(make-closure ,lam ,env)        (set-union (free lam) (free env))]
      [`(env-ref ,eid ,env ,_)          (free env)]
      [`(make-env ,eid (,ks ,vs) ...)   (set-union* (map free vs))]
      [`(app* ,f . ,args)               (set-union (free f) (set-union* (map free args)))]
            
      [`(,f . ,args)        (set-union (free f) (set-union* (map free args)))]))
  
  
  (define (substitute-with sub)
    (λ (body)
      (substitute sub body)))
  
  
  (define (substitute sub body)
    (match body
      [(? symbol?)   (if (hash-has-key? sub body)
                         (hash-ref sub body)
                         body)]
      [(? basic?)    body]

      [`(,(or 'λ 'lambda) ,vars ,body) 
       (define vars* (apply set vars))
       (define sub* (for/hash ([(k v) sub] #:when (not (set-member? vars* k)))
                      (values k v)))
       `(lambda ,vars ,(substitute sub* body))]
      
      [`(,(and functor (or 'set 'tuple 'py-list 'error 'if 'set-then! 'make-cell
                           'get-cell 'set-cell 'make-closure 'app* 'error)) . ,exps)
       `(,functor ,@(map (substitute-with sub) exps))]
       
      [`(dict (,ks ,vs) ...)  
       `(dict ,@(map list (map (substitute-with sub) ks) (map (substitute-with sub) vs)))]
      
      [`(set-then! ,v ,e ,body)  
       `(set-then! ,v ,(substitute sub e) ,(substitute sub body))]
      
      [`(env-ref  ,eid ,env ,field)     `(env-ref  ,eid ,(substitute sub env) ,field)]
      [`(make-env ,eid (,ks ,vs) ...)   `(make-env ,eid ,@(map list ks (map (substitute-with sub) vs)))]
                 
      [`(,f . ,args)       
       `(,(substitute sub f) ,@(map (substitute-with sub) args))]))
      
  
  
  (define (mark-globals! program)
    
    ; mark globals
    (match program
      [`(program ,defs ... ,exp)
       (set! globals (apply set (map cadr defs)))]))
  
  (define globals #f)
  
  (define (global? symbol)
    (set-member? globals symbol))
  
  ; (free/globals e) : free non-global variables in e
  (define (free/globals exp)
    (set-subtract (free exp) globals)))
