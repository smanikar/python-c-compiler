#lang racket

(define (port->list port)
  (let ([next (read port)])
    (if (eof-object? next)
        '()
        (cons next (port->list port)))))

(define (exists proc . lists)
 (match lists
  [`(() ...) 
   #f]

  [`((,hd ,tl ...) ...)
   (or (apply proc hd)
       (apply exists (cons proc tl)))]))

(define (differ? s1 s2)
  (or (not (= (length s1) (length s2)))
      (exists (lambda (x1 x2) (not (equal? x1 x2))) s1 s2)))

(define (is-error? lines)
 (exists (lambda (line)
  (match line
   [`(ERROR ,_) #t]
   [else        #f]))
  lines))


(define (diff-ports p1 p2)
 (define s1 (port->list p1))
 (define s2 (port->list p2))
 (define e1 (is-error? s1))
 (define e2 (is-error? s2))
 (cond
   [(and e1 e2)     #f]
   [(or e1 e2)      #t]
   [(differ? s1 s2) #t]
   [else            #f]))
 

(match (current-command-line-arguments)
  [(vector f1 f2) 
   (if (diff-ports (open-input-file f1) (open-input-file f2))
       (printf "diff: ~s ~s~n" f1 f2)
       (printf "same: ~s ~s~n" f1 f2))]
  
  [else
   (error "insufficient arguments")])
