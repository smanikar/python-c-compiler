#lang racket

(require racket/mpair)

(define-syntax program
  (syntax-rules ()
    [(_ body ...) (let () body ...)]))



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
(define (for-set set f)
  (for ([v set])
    (f v)))

(define (for-tuple tuple f)
  (for ([v tuple])
    (f v)))

(define (for-py-list lst f)
  (for ([v (py-list-mlist lst)])
    (f v)))

(define (for-dict dict f)
  (for ([(v _) dict])
    (f v)))


;; Objects.
(define-syntax get-field 
  (syntax-rules ()
    [(_ obj name) (error "get-field not supported")]))

(define-syntax set-field!
  (syntax-rules ()
    [(_ obj name val) (error "set-field! not supported")]))

(define-syntax remove-field!
  (syntax-rules ()
    [(_ obj name) (error "remove-field! not supported")]))
         

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

; -- 

