#lang racket

(define lambda-hash (make-hash))

(define labels '())
(define (label-lambdas exp)
  (match exp
    [`(lambda (,params ...) ,body) (set! labels (gensym '$lambda)) (hash-set! lambda-hash labels `(lambda ,params ,(label-lambdas body)))
                                   `(lambda-label ,labels)]
    [`(set-then! ,var ,aexp ,cexp) `(set-then! ,var ,(label-lambdas aexp) ,(label-lambdas cexp))]
    [`(,f . ,args)       `(,(label-lambdas f) ,@(map label-lambdas args))]
    
    [else else]))

; create empty list to store labelled so far
(define so-far-label '())

; create empty list
(define exp-lam-list '())
(define (lam-lif-fun program)
  (match program
   [`(program ,defs ... ,exp)
    ; =>
    (set! so-far-label (label-lambdas exp))
    (hash-for-each lambda-hash (lambda (x y) (set! exp-lam-list (cons `(define-label ,x ,y) exp-lam-list))))
    `(program ,@exp-lam-list ,@defs ,so-far-label)]))
  

(pretty-write (lam-lif-fun (read)))