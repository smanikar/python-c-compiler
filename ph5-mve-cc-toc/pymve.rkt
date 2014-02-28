#lang racket

; Srikanth Manikarnike
; U0706564

(require "pytoc-common.rkt")
(require  srfi/1)
;(require "scheme-to-c.scm")

(define mutable-set (set))

(define (mutable? symbol)
  (set-member? mutable-set symbol))

;mark-mutable : exp -> void
(define (mark-mutable exp)
  ;(printf "in mark-mutable ~s~n" exp)
  (match exp
  [`(program ,def ... ,exp) 
                       ;(printf "in program def=~s~n exp=~s~n" def exp)
                        (map mark-mutable def)
                        (mark-mutable exp)]
  
  [`(define ,var ,aexp) (mark-mutable aexp)]  
    
  [`(,(or `lambda `λ) ,var ... ,body) (mark-mutable body)]
  
  [`(if ,aexp ,cexp1 ,cexp2) (mark-mutable aexp)
                             (mark-mutable cexp1)
                             (mark-mutable cexp2)]
    
  [`(set-then! ,var ,aexp ,cexp) 
                                ;(printf "inside set-then! ~s~n" var)
                                 (set! mutable-set (set-union mutable-set (set var)))
                                 (mark-mutable aexp)
                                 (mark-mutable cexp)]
  
  [`(dict (,aexp ,aexp1) ...) (map mark-mutable aexp)
                              (map mark-mutable aexp1)]
   
  [`(tuple ,aexp ...)   (map mark-mutable aexp)]
    
  [`(py-list* (,aexp ...))  (map mark-mutable aexp)]
    
  [`(error ,aexp)  (mark-mutable aexp)]
  
  [ (? atomic?) (void)]
  
  [`(,f ,args ...)   (mark-mutable f)
                   (map mark-mutable args)]
  ;[`() (void)]
    
  [else (error "This shouldn't happen")]))


;replace-mutable : exp -> void
(define (replace-mutable exp)
  ;(printf "in replace-mutable ~s~n" exp)
  (match exp
  [`(program (,def ...) ,exp) 
                       ;(printf "in program def=~s~n exp=~s~n" def exp)
                        `(program ,(map replace-mutable def)
                        ,(replace-mutable exp))]
  
  [`(define ,var ,aexp) `(define ,var ,aexp)]  
    
  [`(,(or `lambda `λ) (,var ... ),body) ;(printf "in lambda ****~nvar=~s~nbody=~s~n" var body)
                                      `(lambda ,var 
                                           ;,(map replace-mutable var)
                                           ,(replace-mutable body))] 
  
  [`(if ,aexp ,cexp1 ,cexp2) `(if ,(replace-mutable aexp)
                             ,(replace-mutable cexp1)
                             ,(replace-mutable cexp2))]
    
  [`(set-then! ,var ,aexp ,cexp) 
                                ;(printf "inside set-then! ~s~n" var)
                                  (if (is-mutable? var)
                                      `(set-then! ,var 
                                                  (make-cell ,var)
                                                  (set-cell! ,var ,(replace-mutable aexp)
                                                             ,(replace-mutable cexp)))
                                      `(set-then! ,var 
                                                  ,(replace-mutable aexp)
                                                  ,(replace-mutable cexp)))]
  
  [`(dict (,keys ,values) ...) (cons `dict (zip (replace-mutable keys) 
                                                (replace-mutable values)))]
   ;`(dict ,(map replace-mutable aexp)
                              ;,(map replace-mutable aexp1))]
   
  [`(tuple (,aexp ...))   `(tuple ,(map replace-mutable aexp))]
    
  [`(py-list* (,aexp ...))  `(py-list* ,(map replace-mutable aexp))]
    
  [`(error ,aexp)  `(error ,(replace-mutable aexp))]
    
  [ (? is-mutable? exp) ;`(get-cell ,exp)]
    ;=>
    (replace-mutable-atom  exp)]
  
  [`(,aexp ,aexps ...)   (cons (replace-mutable aexp)
                   (map replace-mutable aexps))]
    
  [ (? atomic?) ;`,exp]
    ; =>
    (if (is-mutable? exp)
        `(set-then! ,exp (make-cell ,exp)
                    (set-cell! ,exp) ,(replace-mutable-atom exp))
        `,exp)]
        
    
  [else (error "This shouldn't happen too")]))

;replace-mutable-atom : exp -> list
(define (replace-mutable-atom  exp)
`(get-cell ,exp))

; is-mutable? : exp -> bool
(define (is-mutable? exp)
  (if (and (mutable? exp) 
           (not (global? exp))) 
      #t 
      #f))

; program : exp -> void
(define (program exp)
  (mark-globals! exp)
  ;(printf "globals=~s~n" globals)
  (mark-mutable exp)
  ;(printf "mutables=~s~n" mutable-set)
  (pretty-write (replace-mutable exp)))
 
(program (read))

