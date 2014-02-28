; Srikanth Manikarnike
; U0706564

#lang racket

(require  srfi/1)

;; Helpers.

; partition-k : ('a -> boolean) 'a list ('a list 'a list -> 'a list 'a list)
(define (partition-k pred lst k)
  (if (not (pair? lst))
      (k '() '())
      (partition-k pred (cdr lst) (λ (in out)
        (if (pred (car lst))
            (k (cons (car lst) in) out)
            (k in (cons (car lst) out)))))))

; define? : term -> boolean
(define (define? sx)
  (match sx
    [`(define . ,_) #t]
    [else           #f]))

; not-define? : term -> boolean
(define (not-define? sx)
  (not (define? sx)))

; term -> boolean
(define (atomic? exp)
  ;(printf "in atomic ~s~n" exp)
  (match exp
    [`(,(or 'lambda 'λ) . ,_)     #t]
    [(? number?)   #t]
    [(? string?)   #t]
    [(? boolean?)  #t]
    [`(quote . ,_) #t]
    ['(void)       #t]
    ['None         #t]
    ['Ellipsis     #t]
    [else          #f]))

; term -> triop
(define (triop? exp)
  (match exp
    ['py-list-set! #t]    
    ['dict-set!    #t]                                                     
    ['tuple-set!   #t]
    [else            #f]))

; term -> binop
(define (binop? exp)
  (match exp
['<   #t]
['>   #t]
['equal?   #t]
['>=   #t]
['<=   #t]
['not-equal?   #t]
['in?   #t]
['not-in?   #t]
['eq?   #t]
['not-eq?   #t]
['<<   #t]
['>>   #t]
['+   #t]
['-   #t] 
['*   #t]
['/   #t]
['quotient   #t]
['modulo   #t]
['expt   #t]
['assert2   #t]
['for-set   #t]
['for-py-list   #t]
['for-tuple   #t]
['for-dict   #t]
['bitwise-and   #t]
['bitwise-or   #t]
['bitwise-xor   #t]
['py-list-ref   #t]
['py-list-remove!   #t]
['tuple-ref   #t]
['dict-ref   #t]
['dict-remove!   #t]
[else             #f]))


(define (unop? exp)  
  (match exp
     ['bitwise-not    #t]
     ['+    #t]
     ['-    #t]
     ['integer?    #t]                                           
     ['string?    #t]                                            
     ['tuple?    #t]                                             
     ['dict?    #t]
     ['py-list?    #t]
     ['set?    #t]
     ['assert1    #t]
     ['py-print    #t]
     ['not    #t]
     [else      #f]))
    
; atomic-define? : term -> boolean
(define (atomic-define? def)
  (match def
    [`(define ,v ,exp)  (atomic? exp)]
    [else               #f]))

; random temporary variable generator
(define (ec-tempvar)
 (gensym "ec"))

(define (ex-tempvar)
 (gensym "ex"))

(define (seq-tempvar)
 (gensym "$seq"))

(define (loop-tempvar)
 (gensym "$loop"))

; global-name : symbol -> symbol
(define (global-name name)
  (string->symbol (string-append "g$" (symbol->string name))))

; atomize-tops : top list -> top list
(define (atomize-tops tops)
  (match tops
    ['()  #;=>  '()]
    
    [(cons (and head (? atomic-define?)) tail)
     (cons head (atomize-tops tail))]
    
    [(cons `(define ,v ,exp) tail)
     `((define ,v (void))
       (set! ,v ,exp)
       ,@(atomize-tops tail))]
    
    [(cons head tail)
     (cons head (atomize-tops tail))]))   

;; Desugaring.

; desugar-top : top -> top
(define (desugar-top top)
  (match top
    [`(define ,v ,exp)
     `(define ,(global-name v) ,(desugar-exp exp))]
    
    [exp
     (desugar-exp exp)]))

     
; desugar : program -> program
(define (desugar-program program)
  
  (define prog (match program [`(program . ,stmts) stmts]))
  
  (set! prog (atomize-tops prog))
  
  (set! prog (map desugar-top prog))
  
  (set! prog (append (list
                      '(define break (void))
                      '(define return (void))
                      '(define continue (void))
                      '(define $current-handler (void)))
                     prog))
  
  (set! prog
    (partition-k 
     atomic-define?
     prog
     (λ (atomic complex)
       (append atomic `((begin ,@complex))))))
  
  `(program ,@prog))

; tops-to-defs : top list -> def list
(define (tops-to-defs tops)
  
  (define (top-to-def top)
    (match top
      [`(define (,f ,params ...) . ,body) 
       `(define ,f (λ ,params . ,body))]
    
      [`(define ,v ,exp)
       `(define ,v ,exp)]
    
      [exp
       `(define ,(gensym '_) ,exp)]))
  
  (map top-to-def tops))

; while statement
(define-syntax (while stx)
  (syntax-case stx ()
    [(_ cond body else)
     ; =>
     (with-syntax ([break    (datum->syntax #'body 'break)]
                   [continue (datum->syntax #'body 'continue)])
       #'(call/ec (λ (break)
                    (letrec ([loop (λ ()
                                     (when cond
                                       (call/ec (λ (continue)
                                                  body))
                                       (loop)))])
                                                  
                      (loop)
                      else))))]))

; return statement
(define (abc stx)
  (syntax-case stx ()
    [(_ f-params body ...)
     ; =>
     (with-syntax ([return (datum->syntax #'f-params 'return)])
     #'(define f-params 
         (call/ec
          (λ (return) body ...))))]))

; desugar-exp : exp -> exp
(define (desugar-exp exp)
  ;(printf "in desugar-exp ~s~n" exp)
  (match exp
    [(? symbol?)     exp]
    [`(quote ,_)      (error "quotes not allowed in hir")]

    [`(letrec ((,vs ,es) ...) . ,body)
     (desugar-exp
      `(let ,(for/list ([v vs])
               (list v '(void)))
         ,@(map (λ (v e)
                  `(set! ,v ,e))
                vs es)
         ,@body))]     
    
; i wrote this crap
;    [`(let () . ,body)
;     `(lambda () (begin ,(desugar-exp body)))]
;     (pretty-write exp)
;     ;(printf "in let() ~s~n" exp)
    

    [`(let ((,vs ,es) ...) . ,body)
     `((lambda ,vs (begin ,@(desugar-exp body)))
       ,@(map desugar-exp es))]
    
    [`(let* () ,body)
     (error "haven't handled let*")]
    
    [`(let* ((,v ,e) . ,rest) ,body)
     (error "haven't handled let*")]
    
    [`(,(or 'lambda 'λ) ,params ,body)
     `(lambda ,params
        ,(desugar-exp body))]

    [`(call/ec ,exp)
     ;(pretty-write exp)
     `(call/ec ,(desugar-exp exp))]
    
    [`(cond)
     '(void)]
    
    [`(cond (else ,exp))
     (desugar-exp exp)]
        
    [`(cond (,test ,exp))
     `(if ,(desugar-exp test) 
          ,(desugar-exp exp) 
          (void))]
    
    [`(cond (,test ,exp) ,rest ...)
     `(if ,(desugar-exp test)
          ,(desugar-exp exp)
          ,(desugar-exp `(cond . ,rest)))]
     
    [`(or ,exp)
     (desugar-exp exp)]
    
    [`(and ,exp)
     (desugar-exp exp)]
    
    [`(or ,exp . ,rest)
     (define $t (gensym 't))
     (desugar-exp 
      `(let ((,$t ,exp))
         (if ,$t ,$t (or . ,rest))))]
    
    [`(and ,exp . ,rest)
     `(if ,(desugar-exp exp)
          ,(desugar-exp `(and . ,rest))
          #f)]
     
   [`(if ,test ,exp)
     `(if ,(desugar-exp test) ,(desugar-exp exp) (void))]
    
    [`(if ,test ,exp1 ,exp2)
     `(if ,(desugar-exp test) 
          ,(desugar-exp exp1) 
          ,(desugar-exp exp2))]
    
    [`(set! ,v ,exp)
     `(set! ,v ,(desugar-exp exp))]

    [`(assert ,test)
     `(assert1 (lambda () ,(desugar-exp test)))]
     ;(error "haven't handled assert")]
    
    [`(assert ,test ,kind)
     `(assert2 (lambda () ,(desugar-exp test)) (lambda() ,(desugar-exp kind)))]
     ;(error "haven't handled assert")]
    
    [`(get-global ,var)
     `,(global-name var)]
    
    [`(set-global! ,var ,exp)
     `(set! ,(global-name var)
            ,(desugar-exp exp))]
    
    [`(begin . (,body))
     `(begin ,(desugar-exp body))]
    
    ['(return)
     `(return)]
     ;(error "haven't handled return")]
    
    ['(break)
     `(break)]
     ;(error "haven't handled break")]
    
    ['(continue)
     `(continue)]
     ;(error "haven't handled continue")]

    [`(while ,cond ,body)
     `(call/ec
      (lambda (break)
      ((lambda (loop)
         (begin
           (set! loop
             (lambda ()
               (if ,(desugar-exp cond)
                   (begin
                   (call/ec
                    (lambda (continue) ,(desugar-exp body) 
                       ))
                   (loop))
                 (void))))
           (loop) (void)))
       (void))))]
     ;(error "haven't handled while")]
    
    [`(while ,cond ,body ,else)
     `(call/ec
      (lambda (break)
      ((lambda (loop)
         (begin
           (set! loop
             (lambda ()
               (if ,(desugar-exp cond)
                   (begin
                   (call/ec
                    (lambda (continue) ,(desugar-exp body) 
                       ))
                   (loop))
                 (void))))
           (loop) ,(desugar-exp else)
            ))
       (void))))]
     ;(printf "in while ***** ~n")
     ;(printf "in while ~s~n" (desugar-exp cond))
     ;(printf "in while ~s~n" (desugar-exp body))
     ;(printf "in while ~s~n" (desugar-exp else))]
     ;(while cond body else)]
     ;(error "haven't handled while")]
     
    [`(for-each ,var ,expr ,body ,else)
     (let* ([seq-var (seq-tempvar)]
            [loop-var (loop-tempvar)]
            [desug-var (desugar-exp var)])
     `(call/ec
    (lambda (break)
      ((lambda (,seq-var ,loop-var)
         (begin
           (begin
             (if (set? ,seq-var)
               (for-set ,seq-var ,loop-var)
               (if (tuple? ,seq-var)
                 (for-tuple ,seq-var ,loop-var)
                 (if (py-list? ,seq-var)
                   (for-py-list ,seq-var ,loop-var)
                   (if (dict? ,seq-var) (for-dict ,seq-var ,loop-var) (void)))))
             ,(desugar-exp else)))) 
       ,(desugar-exp expr)
       (lambda (,desug-var)
         (call/ec
          (lambda (continue) ,(desugar-exp body))))))))]
     ;(error "haven't handled for-each")]
    
    [`(for-each ,var ,expr ,body)
   ;create sequence variable
   ;create loop variable
   ;desugar-exp var
;     (printf "in for ******** ~n")
;     (printf "in for ~s~n" (desugar-exp var))
;     (printf "in for ~s~n" (desugar-exp expr))
;     (printf "in for !!! ~s~n"  body)
     (let* ([seq-var (seq-tempvar)]
            [loop-var (loop-tempvar)]
            [desug-var (desugar-exp var)])
     `(call/ec
    (lambda (break)
      ((lambda (,seq-var ,loop-var)
         (begin
           (begin
             (if (set? ,seq-var)
               (for-set ,seq-var ,loop-var)
               (if (tuple? ,seq-var)
                 (for-tuple ,seq-var ,loop-var)
                 (if (py-list? ,seq-var)
                   (for-py-list ,seq-var ,loop-var)
                   (if (dict? ,seq-var) (for-dict ,seq-var ,loop-var) (void)))))
             (void)))) 
       ,(desugar-exp expr)
       (lambda (,desug-var)
         (call/ec
          (lambda (continue) ,(desugar-exp body))))))))]
     
     ;(printf "in for ******** ~n")
     ;(printf "in for ~s~n" (desugar-exp var))
     ;(printf "in for ~s~n" (desugar-exp expr))
     ;(printf "in for ~s~n" (desugar-exp body))]
     
    ; (error "haven't handled for-each")]
    
    [`(dict (,keys ,values) ...)
     (cons `dict (zip keys values))]
    ;,@(map (desugar-exp keys) (desugar-exp values)))]
     ;(error "haven't handled dict")]
    
    [`(set . ,values)
     `(set ,(desugar-exp values))]
     ;(error "haven't handled set")]
    
    [`(tuple . ,values)
     `(tuple ,@(map desugar-exp values))]
     ;(error "haven't handled tuple")]
    
    [`(py-list* . (,values))
     `(py-list* ,(desugar-exp values))]
     ;(error "haven't handled py-list*")]
    
    
    [`(try ,body ,handler)
    `((lambda ($old-handler)
      (begin
        ((lambda ($old-return)
           (begin
             ((lambda ($old-continue)
                (begin
                  ((lambda ($old-break)
                     (begin
                       ((lambda (return)
                          (begin
                            ((lambda (continue)
                               (begin
                                 ((lambda (break)
                                    (begin
                                      (call/ec
                                       (lambda ($ec15)
                                         (begin
                                           (set! $current-handler
                                             (lambda ($ex14)
                                               (begin
                                                 (set! $current-handler
                                                   $old-handler)
                                                 ($ec15
                                                  (,(desugar-exp handler)
                                                   $ex14)))))
                                           ((lambda (rv)
                                              (begin
                                                (begin
                                                  (set! $current-handler
                                                    $old-handler)
                                                  rv)))
                                            ,(desugar-exp body)))))))
                                  (lambda ()
                                    (begin
                                      (set! $current-handler $old-handler)
                                      ($old-break))))))
                             (lambda ()
                               (begin
                                 (set! $current-handler $old-handler)
                                 ($old-continue))))))
                        (lambda (rv)
                          (begin
                            (set! $current-handler $old-handler)
                            (return rv))))))
                   break)))
              continue)))
         return)))
    $current-handler)] 
      
; failed attempt     
;     ((printf "in body ~s~n" body)
;      (printf "in handler ~s~n" handler)
;     (let ([$old (current-handler)])
;     (let* ([return   (lambda args 
;                       (set-current-handler! $old)
;                       (apply desugar-exp args))]
;            [continue (lambda ()
;                       (set-current-handler! $old)
;                       `(continue))]
;            [break    (lambda ()
;                       (set-current-handler! $old)
;                       `(break))])
;       (call/ec 
;         (lambda (ec)
;            (set-current-handler!
;               (lambda (ex) (set-current-handler! $old)
;                       (ec (handler ex))))
;            (let ([rv body])
;                 (set-current-handler! $old)
;                 rv))))))]
  
     ;(error "haven't handled try")]
     
    [`(throw ,exp)
     `($current-handler ,exp)]
     ;(error "haven't handled throw")]
    
    [(? atomic?)     exp]
    
    [(? triop?)      `(,exp)]
    
    [(? binop?)      `(,exp)]
    
    [(? unop?)       `(,exp)]

    [`(,f . ,args)  
     `(,(desugar-exp f) ,@(map desugar-exp args))]
            
    [else 
     (error (format "desugar fail: ~s~n" exp))]))

; try statement
;$current-handler holds a ptr to string "top-level exception!"
(define $current-handler 
         `(lambda (ex)))

 (define (current-handler) $current-handler)

 (define (set-current-handler! handler)
         `(set! $current-handler ,handler))

(pretty-write (desugar-program (read)))
