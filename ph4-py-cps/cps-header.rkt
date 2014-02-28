#lang racket

(require racket/mpair)

(define-syntax (program stx)
  (syntax-case stx ()
    [(_ defs ... exp)
     (with-syntax ([$halt (datum->syntax #'exp '$halt)])
       #'(call/ec (λ ($halt) defs ... exp)))]))



;; Data structures.
(define-syntax dict
  (syntax-rules ()
    [(_ (k v) ...)
     ; =>
     (make-hash (list (cons k v) ...))]))

(define dict? hash?)
(define dict-ref hash-ref)
(define dict-set! hash-set!)

(define-syntax tuple
  (syntax-rules ()
    [(_ v ...)
     ; =>
     (vector v ...)]))

(define tuple-ref vector-ref)
(define tuple-set! vector-set!)
(define tuple? vector?)


(define (mlist-set! mlst n value)
  (cond
    [(null? mlst)  (error "mlist-set! -- index too high")]
    [(= n 0)       (set-mcar! mlst value)]
    [else          (mlist-set! (mcdr mlst) (- n 1) value)]))

(define (mlist-remove! mlst n)
  (cond
    [(null? mlist) (error "cannot delete from empty list")]
    [(= n 1)       (set-mcdr! mlst (mcdr (mcdr mlst)))]
    [else          (mlist-remove! (mcdr mlst) (- n 1))]))

     
(define-struct py-list ([mlist #:mutable]))

(define (py-list-set! pl i val)
  (mlist-set! (py-list-mlist pl) i val))

(define (py-list-ref pl i)
  (mlist-ref (py-list-mlist pl) i))

(define (py-list-remove! pl i)
  (cond
    [(< i 0)  (error "index out of bounds for removal")]
    [(= i 0)  (set-py-list-mlist! (mcdr (py-list-mlist pl)))]
    [else     (mlist-remove! (py-list-mlist pl) i)]))
     
(define (py-list* . args)
  (py-list (list->mlist args)))

;; Iterators.
(define (for-list-k lst f k)
  (letrec ([loop (λ (lst)
    (if (null? lst)
        (k (void))
        (f (car lst) (λ (_)
          (loop (cdr lst))))))])
    (loop lst)))
  
(define (set->list set)
  (for/list ([v set]) v))

(define (tuple->list tuple)
  (for/list ([v tuple]) v))

(define (py-list->list lst)
  (for/list ([v (py-list-mlist lst)]) v))

(define (dict->keys dict)
  (for ([(k _) dict]) k))


(define (for-set-k set f k)
  (for-list-k (set->list set) f k))
  
(define (for-tuple-k tuple f k)
  (for-list-k (tuple->list tuple) f k))
  
(define (for-py-list-k py-list f k)
  (for-list-k (py-list->list py-list) f k))

(define (for-dict-k dict f k)
  (for-list-k (dict->keys dict) f k))



         

;; Operators.
(define (<< a n) (arithmetic-shift a n))
(define (>> a n) (arithmetic-shift a (- n)))

(define (not-equal? a b)
  (not (equal? a b)))

(define-syntax (define/return stx)
  (syntax-case stx ()
    [(_ f-params body ...)
     ; =>
     (with-syntax ([return (datum->syntax #'f-params 'return)])
     #'(define f-params (call/ec (λ (return) body ...))))]))
  
(define/return (in? needle haystack)
  (cond
    [(hash? haystack)     (for ([(x y) haystack])
                            (when (equal? x needle)
                              (return #t)))]
    [(py-list? haystack)  (return (in? needle (py-list-mlist haystack)))]
    [else                 (for ([x haystack])
                            (when (equal? x needle) 
                              (return #t)))])
  #f)
        
(define not-in? (λ (needle haystack) (not (in? needle haystack))))


;; Special variables
(define None 'None)
(define Ellipsis 'Ellipsis)



;; Variable mutation:
(define-syntax set-then!
  (syntax-rules ()
    [(_ var exp next)
     (begin
       (set! var exp)
       next)]))



;; Library functions.

(define bitwise-or bitwise-ior)

(define (py-object->string o)
  
  (define (commas seq)
    (define first? #t)
    (define ans "")
    (for ([c seq])
      (when (not first?)
        (set! ans (string-append ans ", ")))
      (when first?
        (set! first? #f))
      (set! ans (string-append ans (py-object->string c))))
    ans)
    
  (define (keyvals seq)
    (define first? #t)
    (define ans "")
    (for ([(k v) seq])
      (when (not first?)
        (set! ans (string-append ans ", ")))
      (when first?
        (set! first? #f))
      (set! ans (string-append ans (py-object->string k) ": " (py-object->string v))))
    ans)
    
  
  (cond
    [(py-list? o)   (format "[~a]" (commas (py-list-mlist o)))]
    [(tuple? o)     (format "(~a)" (commas o))]
    [(dict? o)      (format "{~a}" (keyvals o))]
    [(string? o)    (format "~v" o)] 
    [else           (format "~a" o)]))

(define (py-print x) 
  (cond 
    [(string? x)  (display x)]
    [else         (display (py-object->string x))])
  (newline))


;; CPS-ify primitives:
(define (cps proc)
  (λ args
    (match args
      [`(,real-args ... ,cont)
       (cont (apply proc real-args))])))

; -- 

