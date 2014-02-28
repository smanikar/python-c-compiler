;; derp:     derivative-based parsing

;; Author:   Matt Might
;; Site:     http://matt.might.net/
;; Licenses: CRAPL, GPLv3

(module derivative-parsers
  racket/base
  
(provide (all-defined-out))

(provide (for-syntax (all-defined-out)))

(require (for-syntax racket/base))
  
(require srfi/41) ; Stream library
(require racket/match)
(require racket/set)
(require racket/promise)

  
; Generic tools:
(define-syntax while
  (syntax-rules ()
    [(_ cond body ...)
     ; =>
     (letrec ((lp (λ () (when cond body ... (lp)))))
       (lp))]))
  
; Define a recursive (yet monotonic) function over
; a mutually recursive graph by computing its fixed
; point:
(define-syntax define/fix
  (syntax-rules ()
    [(_ (f x) #:bottom bottom body ...)
     ; =>
     (define f (let ((cache     (make-weak-hasheq))
                     (changed?  (make-parameter 'error-changed))
                     (running?  (make-parameter #f))
                     (visited   (make-parameter 'error-visited)))
                 (λ (x)
                   (let ((cached? (hash-has-key? cache x))
                         (cached  (hash-ref cache x (lambda () bottom)))
                         (run?    (running?)))
                     (cond
                       [(and cached? (not run?))
                        ; =>
                        cached]
                       
                       [(and run? (hash-has-key? (unbox (visited)) x))
                        ; =>
                        (if cached? cached bottom)]
                       
                       [run? 
                        ; =>
                        (hash-set! (unbox (visited)) x #t)
                        (let ((new-val (begin body ...)))
                          (when (not (equal? new-val cached))
                            (set-box! (changed?) #t)
                            (hash-set! cache x new-val))
                          new-val)]
                       
                       [(and (not cached?) (not run?))
                        ; =>
                        (parameterize ([changed? (box #t)]
                                       [running? #t]
                                       [visited (box (make-weak-hasheq))])
                          (let ([v bottom])
                            (while (unbox (changed?))
                                   (set-box! (changed?) #f)
                                   (set-box! (visited) (make-weak-hasheq))
                                   (set! v (f x)))
                            v))])))))]))

(define-syntax make-weak-hash-trie
  (syntax-rules ()
    [(_ #:eq eq ...)      (make-weak-hasheq)]
    [(_ #:eqv eq ...)     (make-weak-hasheqv)]
    [(_ #:equal eq ...)   (make-weak-hash)]))

(define-syntax weak-hash-trie-get!
  (syntax-rules ()
    [(_ t [eq] [x] lazy-val)
     ; =>
     (let ([$t t]
           [$x x])
       (if (hash-has-key? $t $x)
           (hash-ref $t $x)
           (let ([val lazy-val])
             (hash-set! $t $x val)
             val)))]
    
    [(_ t [eq1 eq2 eq3 ...] [x1 x2 x3 ...] lazy-val)
     ; =>
     (let ([$t t])
       (if (hash-has-key? t x1)
           (let ([t2 (hash-ref t x1)])
             (weak-hash-trie-get! t2 [eq2 eq3 ...] [x2 x3 ...] lazy-val))
           (let ([t2 (make-weak-hash-trie eq2 eq3 ...)])
             (hash-set! t x1 t2)
             (weak-hash-trie-get! t2 [eq2 eq3 ...] [x2 x3 ...] lazy-val))))]))
 
; Define a function that is memoized by default:
(define-syntax define/memoize 
  (syntax-rules ()
    [(_ (f [v eq] ...) body ...)
     ; =>
     (define/memoize (f v ...) #:order ([v eq] ...) body ...)]
    
    [(_ (f v ...) #:order ([v* eq] ...) body ...)
     ; =>
     (define f (let ((cache (make-weak-hash-trie eq ...))
                     ($f    (lambda (v ...) (let ([v* v] ...) body ...))))
                 (lambda (v ...)
                   (let ([v* v] ...)
                     (weak-hash-trie-get! cache [eq ...] [v ...] ($f v ...))))))]
    
    [(_ (f v ...) body ...)
     ; =>
     (define/memoize (f [v #:equal] ...) body ...)]))
    
; Allows recursive functions on graphs by
; turning them into graph searches:
(define-syntax define/search 
  (syntax-rules ()
    [(_ (f x rest ...) #:reentry default body ...)
     ; =>
     (define f (let ([visited (make-parameter #f)]
                     [$def    default])
                 (lambda (x rest ...)
                   (cond
                     [(not (visited))
                      ; =>
                      (parameterize ([visited (make-hasheq)])
                        (f x rest ...))]
                     
                     [(hash-has-key? (visited) x)
                      ; =>
                      (if (procedure? $def) ($def x) $def)]
                     
                     [else 
                      ; =>
                      (hash-set! (visited) x #t)
                      (let () body ...)]))))]
    
    [(_ (f x rest ...) body ...)
     ; =>
     (define/search (f x rest ...) #:reentry (lambda (x) (void)) body ...)]))  
  
  
; Languages:
(define-struct language ())

; Atomic languages:
(define-struct (empty language) ())           ; empty set
(define-struct (eps   language) ())           ; empty string
(define-struct (token language) (pred class)) ; terminal
  
(define-struct (eps* eps) (tree-set)) ; empty string w/ a set of parse trees

; Compound languages:
(define-struct (compound-language language)      ())
(define-struct (union         compound-language) (this that))
(define-struct (concatenation compound-language) (left right))
(define-struct (reduction     compound-language) (lang reduce))
(define-struct (repetition    compound-language) (lang))

; Constructors:
(define-syntax cat
  (syntax-rules ()
    [(_)            (eps)]
    [(_ l1)         l1]
    [(_ l1 l2 ...)  (concatenation (delay l1) (delay (cat l2 ...)))]))

(define-syntax alt
  (syntax-rules ()
    [(_)            (empty)]
    [(_ l1)         l1]
    [(_ l1 l2 ...)  (union (delay l1) (delay (alt l2 ...)))]))

(define-syntax red
  (syntax-rules ()
    [(_ l f)        (reduction (delay l) f)]
    [(_ l)          (reduction (delay l) (lambda (x) x))]))

(define-syntax rep
  (syntax-rules ()
    [(_ l)          (repetition (delay l))]))

; A language for languages:
(define-syntax (lang stx)
  (syntax-case stx (empty eps eps* quote ? or rep rep+ opt seq seq* seq! quasiquote --> $--> @--> >--> car)
    [(_)                  #'(empty)]
    [(_ (empty))          #'(empty)]
    [(_ (eps))            #'(eps)]
    [(_ (eps v))          #'(eps* (set v))]
    [(_ (eps* s))         #'(eps* s)]
    [(_ (? pred class))   #'(token pred class)]
    [(f (quote lit))      (with-syntax ([literal->language (datum->syntax #'f 'literal->language)])
                            #'(literal->language 'lit))]
    
    [(_ (or))             #'(empty)]
    [(f (or l1))          #'(f l1)]
    [(f (or l1 l2 ...))   #'(alt (f l1) (f (or l2 ...)))]
    
    [(_ (seq))            #'(eps)]
    [(f (seq l1))         #'(f l1)]
    [(f (seq l1 l2 ...))  #'(cat (f l1) (f (seq l2 ...)))]
    
    [(_ (seq*))           #'(eps* (set '()))]
    [(f (seq* l1))        #'(red (f l1) (λ (w1) (list w1)))]
    [(f (seq* l1 l2 ...)) #'(cat (f l1) (f (seq* l2 ...)))]
    
    [(_ (seq!))            #'(eps* (set '()))]
    [(f (seq! `l1 l2 ...)) #'(lang (seq (lang l1) (seq! l2 ...)))]
    [(f (seq!  l1 l2 ...)) #'(lang (--> (seq (lang l1) (seq! l2 ...))
                                        (λ (res) (cdr res))))]
         
    [(f (rep l))          #'(rep (f l))]
    
    [(f (rep+ l))         #'(cat (f l) (rep (f l)))]
    
    [(f (opt l))          #'(alt (f l) (eps* (set 'none)))]
    [(f (opt l v))        #'(alt (f l) (eps* (set v)))]
    
    [(f (car l))          #'(red (f l) (λ (r) (car r)))]
    
    [(f (-->  l g))       #'(red (f l) g)]
    [(f (@--> l g))       #'(red (f l) (λ (w) (apply g w)))]
    [(f (>--> l c ...))   #'(red (f l) (λ (w) (match w c ...)))]
    [(f ($--> l e ...))   (with-syntax ([$  (datum->syntax #'l '$)]
                                        [$$ (datum->syntax #'l '$$)])
                            #'(red (f l)
                                   (λ ($$)
                                     (let (($ (λ (n) (list-ref $$ n))))
                                       e ...))))]
    
    [(f atom)             (with-syntax ([literal->language (datum->syntax #'atom 'literal->language)])
                            (let ((d (syntax->datum #'atom)))
                              (cond
                                [(string? d)   #'(literal->language atom)]
                                [(number? d)   #'(literal->language atom)]
                                [(boolean? d)  #'(literal->language atom)]
                                [else          #'atom])))]
    
    [else                 (error "syntax error in lang")]))
  
; Specifies the default behavior for literals in the grammar:
(define (default-literal->language lit)
  (token (lambda (t) (equal? t lit)) lit))
    
(define literal->language default-literal->language)
  
  
; Set the behavior for literals in the grammar:
(define (set-literal->language! f)
  (set! literal->language f))
    
; Pattern-matchers on languages:
(define-match-expander orp
  (syntax-rules ()
    [(_ l1 l2) (union (app force l1) (app force l2))]))

(define-match-expander seqp
  (syntax-rules ()
    [(_ l1 l2) (concatenation (app force l1) (app force l2))]))

(define-match-expander redp
  (syntax-rules ()
    [(_ l f) (reduction (app force l) f)]))

(define-match-expander repp
  (syntax-rules ()
    [(_ l) (repetition (app force l))]))

(define-match-expander nullablep
  (syntax-rules ()
    [(_) (app nullable? #t)]))

; Parse a stream into a tree:    
(define (parse l s 
               #:compact   [compact (lambda (x) x)] 
               #:steps     [n #f] 
               #:debug     [debug? #f])
  (cond
    [(and n (= n 0))  l]
    [(stream-null? s) (parse-null l)]
    [else             
     ; =>
     (let* ([c      (stream-car s)]
            [rest   (stream-cdr s)]
            [dl/dc  (parse-derive c l)]
            [l*     (compact dl/dc)])
       (when debug?
         (display (format "size: ~s; mem: ~s~n" (language-size l*) (current-memory-use))))
       (parse l* rest
              #:compact  compact
              #:steps    (and n (- n 1))
              #:debug    debug?))]))

(define (parse-null/input l input)
  (list->stream (set-map (parse-null l) (lambda (el) (cons el input)))))

; Nullability:
(define/fix (nullable? l)
  #:bottom #f
  (match l
    [(empty)           #f]
    [(eps)             #t]    
    [(token _ _)       #f]
    [(repp _)          #t]
    [(orp l1 l2)       (or (nullable? l1) (nullable? l2))]
    [(seqp l1 l2)      (and (nullable? l1) (nullable? l2))]
    [(redp l1 _)       (nullable? l1)]))

; Parse trees for nullability:
(define empty-tree-set (set))

(define/fix (parse-null l)
  #:bottom empty-tree-set
  (match l
    [(empty)        empty-tree-set]
    [(eps* S)       S]
    [(eps)          (set l)]
    [(token _ _)    empty-tree-set]
    [(repp (nullp)) (error "infinite parse-null")]
    [(repp _)       (set '())]
    [(orp  l1 l2)   (set-union (parse-null l1) (parse-null l2))]
    [(seqp l1 l2)   (for*/set ([t1 (parse-null l1)]
                               [t2 (parse-null l2)])
                              (cons t1 t2))]
    [(redp l1 f)    (for/set ([t (parse-null l1)])
                             (f t))]))

; Derivative of a parser combinator:
(define/memoize (parse-derive c l)
  #:order ([l #:eq] [c #:equal])
  (match l
    [(empty)     l]
    [(eps)       (empty)]
    [(token pred class)
     ; =>
     (if (pred c) (eps* (set c)) (empty))]
    
    [(orp l1 l2)
     ; =>
     (alt (parse-derive c l1) 
          (parse-derive c l2))]
    
    [(seqp (and (nullp) l1) l2)
     ; =>
     (cat (eps* (parse-null l1)) (parse-derive c l2))]
    
    [(seqp (and (nullablep) l1) l2)
     ; =>
     (alt (cat (eps* (parse-null l1)) (parse-derive c l2))
          (cat (parse-derive c l1) l2))]
    
    [(seqp l1 l2)
     ; =>
     (cat (parse-derive c l1) l2)]
    
    [(repp l1)
     ; =>
     (cat (parse-derive c l1) l)]
    
    [(redp l f)
     ; =>
     (red (parse-derive c l) f)]))

; Derivative of a context-free language:
(define/memoize (derive c l)
  #:order ([l #:eq] [c #:equal])
  (match l
    [(empty)
     ; =>
     l]
    
    [(eps)
     ; =>
     (empty)]
    
    [(token pred class)
     ; =>
     (if (pred c) (eps) (empty))]
    
    [(orp l1 l2)
     ; =>
     (alt (derive c l1) 
          (derive c l2))]
    
    [(seqp (and (nullablep) l1) l2)
     ; =>
     (alt (derive c l2)
          (cat (derive c l1) l2))]
    
    [(seqp l1 l2)
     ; =>
     (cat (derive c l1) l2)]
    
    [(repp l1)
     ; =>
     (cat (derive c l1) l)]
    
    [(redp l f)
     ; =>
     (derive c l)]
    
    [else 
     (printf "unknown language: ~s~n" l)
     (error "could not derive unknown language")]))

; Recognizes if a string is in a language:
(define (recognizes? l s
                     #:compact [compact (lambda (x) x)] 
                     #:debug   [debug? #f])
  (cond
    [(stream-null? s) (nullable? l)]
    [else
     ; =>
     (let* ([c      (stream-car s)]
            [rest   (stream-cdr s)]
            [dl/dc  (derive c l)]
            [l*     (compact dl/dc)])
       (when debug?
         (display (format "size: ~s; mem: ~s~n" (language-size l*) (current-memory-use))))
       (recognizes? l* rest
              #:compact compact
              #:debug   debug?))]))

; Partially parse a stream; return sub-parses:
(define (parse-partial l s) 
  (if (stream-null? s)
      (parse-null/input l stream-null)
      (match l 
        [(empty)      stream-null]
        [(eps)        (stream (cons (eps) s))]
        [(eps* S)     (parse-null/input l s)]
        [(token p c)
         ; =>
         (cond
           [(p (stream-car s)) (stream-cons (cons (stream-car s) (stream-cdr s))
                                            stream-null)]
           [else               stream-null])]
        [else
         ; =>
         (define c (stream-car s))
         (define rest (stream-cdr s))
         (stream-stitch (parse-partial (parse-derive c l) rest)
                        (parse-null/input l s))])))
                      
; Stream stitching:
(define (stream-stitch s1 s2 #:even [even? #t])
  (define (pull-s1) (stream-cons (stream-car s1) (stream-stitch (stream-cdr s1) s2 #:even (not even?))))
  (define (pull-s2) (stream-cons (stream-car s2) (stream-stitch s1 (stream-cdr s2) #:even (not even?))))
  (cond
    [(and even? (not (stream-null? s1))) (pull-s1)]
    [(and even? (not (stream-null? s2))) (pull-s2)]
    [even?                               stream-null]
    [(not (stream-null? s2))             (pull-s2)]
    [(not (stream-null? s1))             (pull-s1)]
    [else                                stream-null]))

; Checks whether a language is the empty string:
(define/fix (is-null? l)
  #:bottom #t
  (match l
    [(empty)           #f]
    [(eps)             #t]    
    [(token _ _)       #f]
    [(orp l1 l2)       (and (is-null? l1)  (is-null? l2))]
    [(seqp l1 l2)      (and (is-null? l1)  (is-null? l2))]
    [(repp l1)         (or (is-null? l1) (is-empty? l1))]
    [(redp l1 _)       (is-null? l1)]))

; Compute the size of a set:
(define (set-size s)
  (define size 0)
  (for ([_ s])
    (set! size (+ size 1)))
  size)

(define (singleton? s)
  (eqv? (set-size s) 1))

(define (set-choose s)
  (define el #f)
  (for ([el* s])
    (set! el el*))
  el)

; Matches a language if it is *exactly* the empty string:
(define-match-expander nullp
  (syntax-rules ()
    [(_)    (app is-null? #t)]
    [(_ el) (and (app is-null? #t) (app parse-null (and (? singleton?) (app set-choose el))))]))

; Checks whether a language is the empty set:
(define/fix (is-empty? l)
  #:bottom #t
  (match l
    [(empty)           #t]
    [(eps)             #f]    
    [(token _ _)       #f]
    [(repp l1)         #f]
    [(orp l1 l2)       (and (is-empty? l1)  (is-empty? l2))]
    [(seqp l1 l2)      (or  (is-empty? l1)  (is-empty? l2))]
    [(redp l1 _)       (is-empty? l1)]))

(define-match-expander emptyp
  (syntax-rules ()
    [(_) (app is-empty? #t)]))



;;;; Optimizations for the grammar:

; Performs top-level reductions on a grammar:
(define/memoize (simplify [l #:eq])
  (match l
    [(empty)       l]
    [(eps)         l]
    [(token p c)   l]
    [(emptyp)      (empty)]
    [(nullp)       (eps* (parse-null l))]

    [(orp (emptyp) l2)  l2]
    [(orp l1 (emptyp))  l1]
    
    [(seqp (nullp t) l2)  (red l2 (lambda (w2) (cons t w2)))]
    [(seqp l1 (nullp t))  (red l1 (lambda (w1) (cons w1 t)))]
    
    [(repp (emptyp))      (eps* (set '()))]
    
    [(redp (and e (nullp)) f) 
     ; =>
     (eps* (for/set ([t (parse-null e)]) (f t)))]
    
    [(redp (seqp (nullp t) l2) f)
     ; =>
     (red l2 (lambda (w2) (f (cons t w2))))]
    
    [(redp (redp l f) g) 
     ; =>
     (red l (compose g f))]
        
    [else    l]))

; Checkse whether a language can be compacted:
(define/fix (compactable? l)
  #:bottom #f
  (match l
    [(empty)       #f]
    [(eps)         #f]
    [(emptyp)      #t]
    [(nullp)       #t]
    [(token p c)   #f]

    [(repp (emptyp))  #t]
    [(repp l)         (compactable? l)]
    
    [(orp l1 l2)   (or (compactable? l1) (compactable? l2))]
    [(seqp l1 l2)  (or (compactable? l1) (compactable? l2))]
    
    [(redp (seqp (nullp t) l2) f)   #t]
    [(redp (redp l f) g)            #t]
        
    [(redp l f)    (compactable? l)]))

; Performs recursive reductions on a grammar:
; IF YOU UPDATE compact, YOU MUST ALSO UPDATE compactable?
(define/memoize (compact [l #:eq])
  (cond 
    [(not (compactable? l))      l]
    [else
     ; =>
     (match l
       [(empty)       l]
       [(eps)         l]
       [(emptyp)      (empty)]
       [(nullp)       (eps* (parse-null l))]
       [(token p c)   l]
       
       [(repp (emptyp))  (eps* (set '()))]
       [(repp l)         (rep (compact l))]
       
       [(orp (emptyp) l2)  (compact l2)]
       [(orp l1 (emptyp))  (compact l1)]
       
       [(seqp (nullp t) l2)  (red (compact l2) (lambda (w2) (cons t w2)))]
       [(seqp l1 (nullp t))  (red (compact l1) (lambda (w1) (cons w1 t)))]
       
       [(orp l1 l2)   (alt (compact l1) (compact l2))]
       [(seqp l1 l2)  (cat (compact l1) (compact l2))]
       
       [(redp (and e (nullp)) f) 
        ; =>
        (eps* (for/set ([t (parse-null e)]) (f t)))]
       
       [(redp (seqp (nullp t) l2) f)
        ; =>
        (red (compact l2) (lambda (w2) (f (cons t w2))))]
       
       [(redp (redp l f) g) 
        ; =>
        (red (compact l) (compose g f))]
       
       [(redp l f)    (red (compact l) f)])]))

; Checks whether a language can be strongly compacted:
(define/fix (compactable*? l)
  #:bottom #f
  (match l
    [(empty)       #f]
    [(eps)         #f]
    [(token p c)   #f]
    
    [(emptyp)      #t]
    [(nullp)       #t]

    [(orp l1 l2)   (or (compactable*? l1) (compactable*? l2))]
    [(seqp l1 l2)  (or (compactable*? l1) (compactable*? l2))]
    [(redp l f)    #t]))

; Performs algebraic reductions on a grammar.
; compact* is aggressive and will mangle
; parse trees.  It is only safe for
; recognition.
(define/memoize (compact* [l #:eq])
  (cond
    [(not (compactable*? l))      l]
    [else
     ; => 
     (match l
       [(empty)       l]
       [(eps)         l]
       [(token p c)   l]
       [(emptyp)      (empty)]
       [(nullp)       (eps)]
       
       [(orp (emptyp) l2)  (compact* l2)]
       [(orp l1 (emptyp))  (compact* l1)]
       
       [(seqp (nullp t) l2)  (compact* l2)]
       [(seqp l1 (nullp t))  (compact* l1)]
       
       [(orp l1 l2)   (alt (compact* l1) (compact* l2))]
       [(seqp l1 l2)  (cat (compact* l1) (compact* l2))]
       
       [(redp l f) (compact* l)])]))


;;;; Debugging.

; Gives every object a unique value:
(define mark-of-beast 
  (let* ([index (make-hasheq)]
         [max   0]
         [next  (lambda ()
                  (set! max (+ max 1))
                  max)])
    (lambda (object)
      (if (hash-has-key? index object)
          (hash-ref index object)
          (begin
            (hash-set! index object (next))
            (mark-of-beast object))))))

; Computes the size of a grammar.
(define/search (language-size l) 
  #:reentry 0
  (match l
    [(or (eps) (token _ _) (empty))   1]
    [(or (seqp l1 l2) (orp l1 l2))    (+ 1 (language-size l1)
                                           (language-size l2))]
    [(or (redp l _) (repp l))         (+ 1 (language-size l))]))
     
; Outputs a grammar as a dot file.
(define (dotify l #:port [port (current-output-port)])
  
  (define/search (dotify-nodes l port)
    (define m (mark-of-beast l))
    (match l
      [(empty) 
       ; =>
       (display (format "\"~s\" [label = \"empty\"~n];~n~n" m) port)]
      
      [(eps* S)
       ; =>
       (display (format "\"~s\" [shape = \"record\", label = \"eps* | ~v\"~n];~n~n" m S) port)]
      
      [(eps)
       ; =>
       (display (format "\"~s\" [label = \"eps\"~n];~n~n" m) port)]
      
      [(token _ c)
       ; =>
       (display (format "\"~s\" [shape = \"record\", label = \"token | ~s\"~n];~n~n" m c) port)]
      
      [(orp l1 l2)
       ; =>
       (define m1 (mark-of-beast l1))
       (define m2 (mark-of-beast l2))
       (display (format "\"~s\" [label = \"or\"~n];~n~n" m) port)
       (dotify-nodes l1 port)
       (dotify-nodes l2 port)
       (display (format "\"~s\" -> \"~s\" [~n];~n~n" m m1) port)
       (display (format "\"~s\" -> \"~s\" [~n];~n~n" m m2) port)]
      
      [(seqp l r)
       ; =>
       (define ml (mark-of-beast l))
       (define mr (mark-of-beast r))
       (display (format "\"~s\" [shape=\"none\", margin=0, label = <~n<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\"><tr><td colspan=\"2\">seq</td></tr><tr><td port=\"L\">L</td><td port=\"R\">R</td></tr></table>>~n];~n~n" m) port)
       (dotify-nodes l port)
       (dotify-nodes r port)
       (display (format "\"~s\":L -> \"~s\" [~n];~n~n" m ml) port)
       (display (format "\"~s\":R -> \"~s\" [~n];~n~n" m mr) port)]
      
      [(repp l)
       ; =>
       (define ml (mark-of-beast l))
       (display (format "\"~s\" [label = \"rep\"~n];~n~n" m) port)
       (dotify-nodes l port)
       (display (format "\"~s\" -> \"~s\" [~n];~n~n" m ml) port)]
     
      [(redp l f)
       ; =>
       (define ml (mark-of-beast l))
       (display (format "\"~s\" [label = \"red\"~n];~n~n" m) port)
       (dotify-nodes l port)
       (display (format "\"~s\" -> \"~s\" [~n];~n~n" m ml) port)]))
  
  (define close-port? #f)
  
  (when (string? port)
    (set! close-port? #t)
    (set! port (open-output-file port #:mode 'text #:exists 'replace)))
  
  (display (format "digraph {~n~n") port)
  (display (format "node [];~n") port)
  (dotify-nodes l port)
  (display (format "\"~s\" [shape = \"doublecircle\"~n];~n" (mark-of-beast l)) port)
  (display (format "~n}") port)
  
  (when close-port?
    (close-output-port port)))
           
; Read all lines from a file:  
(define (read-all port)
  (let ((next (read port)))
    (if (eof-object? next)
        '()
        (cons next (read-all port)))))


(define (grammar-strip rhs)
  
  (define (quasi-strip l)
    (match l
      [`(quasiquote ,l)  (grammar-strip l)]
      [else              (grammar-strip l)]))
  
  (match rhs
    [(? symbol?)          rhs]
    [(? string?)          rhs]
    
    [`(eps)               rhs]
    [`(eps* ,_)           `(eps)]
        
    [`(>--> ,l ,_ ...)    (grammar-strip l)]
    [`($--> ,l ,_)        (grammar-strip l)]
    [`(@--> ,l ,_)        (grammar-strip l)]
    [`(-->  ,l ,_)        (grammar-strip l)]
    
    [`(car  ,l)           (grammar-strip l)]
    
    [`(seq! ,l ...)      `(seq ,@(map quasi-strip l))]
    [`(seq* ,l ...)      `(seq ,@(map grammar-strip l))]
    [`(seq  ,l ...)      `(seq ,@(map grammar-strip l))]
    
    [`(or ,l ...)        `(or ,@(map grammar-strip l))]
    
    [`(rep ,l)           `(rep ,(grammar-strip l))]
    [`(rep+ ,l)          `(rep+ ,(grammar-strip l))]
    
    [`(opt ,l)           `(opt ,(grammar-strip l))]
    [`(opt ,l ,v)        `(opt ,(grammar-strip l))]
    
    [else
     (error (format "not yet strippable: ~s~n" rhs))]))
  
(define (print-grammar grammar)
  (for ([rule grammar])
    (match rule
      [`(,lhs ,rhs)
       (printf "(~s " lhs)
       (printf "~s" (grammar-strip rhs))
       (printf ")~n")])))
  
; Tools for defining grammars:
(define-syntax grammar
  (syntax-rules ()
    [(_)   (void)]
    [(_ (lhs rhs) ... body)
     ; =>
     (let () 
       (define lhs (lang rhs)) ... 
       body)]))

; Allows a grammar from an external file:  
(define-syntax (grammar-from-file stx)
  (syntax-case stx ()
    [(_ start filename)
     (let ()
       (define (read-all port)
         (let ([next (read port)])
           (if (eof-object? next)
               '()
               (cons next (read-all port)))))
       (let* ([filename (syntax->datum #'filename)]
              [contents (read-all (open-input-file filename))])
         (datum->syntax #'start `(grammar ,@contents ,(syntax->datum #'start)))))]))
  
    
)
