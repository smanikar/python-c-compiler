#lang racket

; Srikanth Manikarnike 
; u0706564

(require srfi/41) ; Stream library
(require test-engine/racket-tests)
(require racket/pretty)

;(require "derivative-parsers.rkt")

(define args (current-command-line-arguments))

(define (python-trans-file filename)
  (python-trans-port (open-input-file filename)))

; random temporary variable generator
(define (gen-tempvar)
 (gensym "cv"))
; environments are sets of symbols:
(define empty-env (set))

; flattens internal compound statements into the outer block:
(define (flatten-stmts stmts)
  stmts)

; matches augmented assignment operators:
(define-match-expander augassign
 (syntax-rules ()
   [(_)
    (or "+=" "-=" "*=" "/=" "%="
        "&=" "|=" "^=" "<<=" ">>=" "**=" "//=")]))

; env
(define (enter-func env)
 (cons '(()) env))

(define (add-to-env env x)
 (let ((current-env (car env)))
   (cond
     ((not (pair? x)) 
      (cons (cons x current-env) (cdr env)))
     (else
      (cons (append x current-env) (cdr env))))))

(define (get-current-env env)
 (car env))

(define (in-env? env x)
 (let ((current-env (get-current-env env)))
   (define (in-current-env? cenv x)
     (cond
       ((null? cenv) #f) 
       ((equal? x (car cenv)) #t) 
       (else (in-current-env? (cdr cenv) x))))
   (in-current-env? current-env x)))


(define (get-get env var)
   (if (in-env? env var)
     var 
     `(get-global ,var)))

(define (get-set! env var)
 (if (in-env? env var)
     'set!
     'set-global!))

; converts an augment assignment operator to the target binop:
;(define (select-augassign op))

; determines global bindings from a group of statements:
;(define (global-bindings stmts))

; finds all of the bindings in a list of l-values:
;(define (lvals->bindings lvals))

; finds all of the local bindings in a list of statements:
(define (local-bindings stmts)
 (filter (λ (s) (not (null? s)))
         (for/list ([s stmts])
           (match s
             [`(def (,name ,names ...) ,suite) `(,name)]
             [else '()]))))

; generates code to set an indexed l-value:
;(define (set-index env $base index expr))

; generates code to augment an indexed l-value:
;(define (set-index-augassign env $base index op expr))

; generates code to delete an indexed l-value:
;(define (delete-index env $base index))

; generates code for a statement:
(define (transform-stmt env stmt)
   (match stmt
   [`(def (,name ,names ...) ,suite)
    `(set-global! ,name
           (lambda ,names
             (call/ec
              (lambda(return)
                ,(transform-stmt env suite))))) ]
   [`(suite (,stmts ...)) `(let () ,@(map (λ (s) (transform-stmt env s)) stmts))]
   [`(suite ,stmts ...) `(let () ,@(map (λ (s) (transform-stmt env s)) stmts))]
     
   [`(cond ,clause ... (else ,suite))
    `(cond ,@(map (λ (s) (list (transform-expr (car s)) (transform-stmt env (cadr s)))) clause) (else ,(transform-stmt env suite)))]
     
   [`(cond ,clause ...)
    `(cond ,(map (λ (t s) (transform-expr t) (transform-stmt env s)) clause))]
     
   [`(return ,tests ...) `(return ,@(map transform-expr tests))]
     
   [`(expr ,tuple_or_test) (transform-expr tuple_or_test)]
     
   [`(= (,test ...) ,tuple_or_test)
    `(set-global! ,@(map transform-expr test) ,(transform-expr tuple_or_test))]  ;,(get-set! env test)
   
   [`(try ,suite (((except) ,suite2)) #f #f) `(try ,(transform-stmt env suite) (lambda (ex) ,(transform-stmt env suite2)))]
     
   ;for statement
   [`(for ,name ,test ,suite) 
    (let ([temp-var (gen-tempvar)]) `(for-each ,temp-var ,(transform-expr test) 
                                             (begin (set-global! ,name ,temp-var) ,(transform-stmt env suite))))]
   
   [`(called ,arglist) `(,@(map transform-expr arglist))]
   
   ;[`(for ,name ,test ,suite)]
   
   [`print `py-print]

   ;[`(subscript ,tuple-or-test) `,(transform-expr
   [stmt stmt]))

; a curried form of transform-stmt, useful in conjuction with map:
;(define (transform-stmt-with env))

; selects the HIR comparison op given the Python op:
;(define (select-cmp cmp))

; selects the HIR shift op given the Python op:
;(define (select-shift op)),

; selects the HIR arithmetic op given the Python op:
;(define (select-arith op))

; selects the HIR term op given the Python op:
;(define (select-term op))

; unfolds a comparison exp in Python into an HIR exp:
;(define (unwind-comparison env expr ops))

; unfolds a binary op exp in Python into an HIR exp:
;(define (unwind-op select-op env $expr ops))

; unfolds a trailer in Python into an HIR ep:
;(define (unwind-trailer env $expr trailer))

; unfolds a sequence of trailers into an HIR exp:
;(define (unwind-trailers env $expr trailers))

(define (transform-comparison expr tests)
 (match (list expr tests)
   [`(,expr (,test))
    `(,(transform-expr (car test)) ,(transform-expr expr) ,(transform-expr (car (cdr test))))]
   [`(,expr (,test ...))
    (let ((tempvar (gen-tempvar)))
      (let ((t (car test)))
        `(let ((,tempvar ,(transform-expr (car (cdr t)))))
           (if (,(transform-expr (car t)) ,(transform-expr expr) ,tempvar)
                ,(transform-comparison tempvar (cdr test))
                #f))))]))

(define (transform-indexed atom trailer)
 (match (list atom trailer)
   [`(print ,trailer) `(py-print ,(transform-expr (car (cdr (car trailer)))) ,@(map (λ (x) (transform-expr x)) (cdr trailer)))]
   [else
    (foldl (λ (x result)
             (match x
               [`(called ,arglist) `((get-global ,result) ,(transform-expr arglist))])) atom trailer)]))

; transforms a Python exp into an HIR exp:
;(define (transform-expr env expr))
(define (transform-expr expr)
 (match expr
   ;comparison 
   [`(comparison ,expr ,tests ...)
    (transform-comparison expr tests)]
   [`(star ,expr) expr]
   [`(indexed ,atom ,trailer ...)
    (transform-indexed atom trailer)]
   [`(term ,factor ,terms ...)
    (foldl (λ (x result) `(,(transform-expr (car x)) ,result ,(transform-expr (car (cdr x))))) (transform-expr factor) terms)]
   [`(arith ,term ,arith_ops ...)
    (foldl (λ (x result) `(,(transform-expr (car x)) ,result ,(transform-expr (car (cdr x))))) (transform-expr term) arith_ops)]
   [`(shift ,ar_expr ,shift_ops ...) 
    (foldl (λ (x result) `(,(transform-expr (car x)) ,result ,(transform-expr (car (cdr x))))) (transform-expr ar_expr) shift_ops)]
   [`("+" ,factor) `(+ ,(transform-expr factor))]
   [`("-" ,factor) `(- ,(transform-expr factor))]
   [`("~" ,factor) `(~ ,(transform-expr factor))]
   ["<" '<]
   ["==" 'equal?]
   ['False #f]
   ['print 'py-print]
   [expr expr]))

; transform a suite into a list of statements:
;(define (suite->stmts suite))

; transform a suite that begins a new scope:
;(define (transform-body-suite env suite))

; transform a suite that does not begin a new scope:
;(define (transform-suite env suite))

;(define input #f)
;(match (current-command-line-arguments)
;(pretty-write (transform-program input))

(define (suite-trans suite)
 (pretty-write suite)
  `(let (,suite)))

(define (transform-program input)
  ;(pretty-write input)
   (match input
   [`(program . ,(app flatten-stmts stmts))
    ; =>
    (define globals (local-bindings stmts))
    `(program
      ,@(for/list ([g globals]) `(define ,g (void)))
      ,@(map (λ (s) (transform-stmt empty-env s)) stmts))]

   [else '("not a program") ]))

; (match lines
;  [`((program ,top-form)) `(program ,(topform-trans top-form))]
;  [else (error lines)]))

(define (python-trans-port port)
  (define lines `,(read port))
  (pretty-write (transform-program lines)))
   

 (define prog1 '(program (def
 (abs x)
 (suite
  (cond
   ((comparison x ("<" 0) ("<" 1) (">" 2)) (suite (return ("-" x))))
   (else (suite (return x))))))
 (expr
 (indexed
  print
  (called
   (indexed abs (called (term ("-" 1) ("*" ("-" 2)) ("*" ("-" 1))))))))))

(define prog2 '(program
 (def
 (fact n)
 (suite
  (cond
   ((comparison n ("<" 0)) (suite (return False)))
   ((comparison n ("==" 0)) (suite (return 1)))
   (else
    (suite
     (return (term n ("*" (indexed fact (called (arith n ("-" 1))))))))))))
 (expr (indexed print (called (indexed fact (called 5)))))))

(define  prog3 '(program
 (= (x) 20)
 (while
  (comparison x (">" 0))
  (suite (= (x) (arith x ("-" 1))) (expr (indexed print (called x))))
  (suite (expr (indexed print (called "didn't run\n")))))))

;(transform-program prog3)

(match args
  [(vector filename)
   ; =>
   (python-trans-file filename)
   (exit)]
  
  [else
   ; =>
   (python-trans-port (current-input-port))])

;(define (topform-trans top-form)
; (pretty-write top-form)
; (match top-form
;  [`(def (,name ...) ,suite) (begin ( global-list (cons global-list (list `(define ,(car name) (void)))))
;	  							`,(suite-trans (list (cdr name) suite)))])) 
; (var-expr-def-trans (top-form)))
; (pretty-write top-form))

;(let ([trans-list (transform-program lines)]) 
;              (pretty-write trans-list   
;              (list (car trans-list) (global-list) (cdr trans-list) ))))

;(define (test-trans test)
; (match test
;  [`(if ,or_test ,or_test ,test)]
;  [`(ATOM or UNARY_OP or FACTOR_OP)]
;  [`(indexed ,atom ,factor)]
;  [`(power ,indexed ,factor)]
;  [`(term ,factor ,seq)]))
;
;(define (tuple-or-test-trans tuple_or_test)
; (match tuple_or_test
;  [`(tuple ,test) `(tuple ,(test-trans test))]
;  [`(,test) `(test-trans test)]))
;
;(define (var-expr-def-trans var-def)
; (match var-def
; [`(def (,var ,exp) ,suite) ( `(define ,(var-trans var) ,(var-exp-trans exp)))]
; ;[`(cond (,test ,suite) seq)]
; [`(expr ,tuple_or_test) `(,(tuple-or-test-trans tuple_or_test))]
; [else (error var-def)]))

;;;; Python grammar

;(define (mk-tag? tag-name)
;  (lambda (token)
;    (eq? (car token) tag-name)))
;
;(define (mk-value? predicate)
;  (lambda (token)
;    (predicate (cadr token))))
;
;(define NAME      (lang (@--> (token (mk-tag? 'ID) 'NAME)
;                              (λ (_ id) (string->symbol id)))))
;(define NUMBER    (lang (@--> (token (and/c (mk-tag? 'LIT) (mk-value? number?)) 'NUMBER)
;                              (λ (_ num) num))))
;(define STRING    (lang (@--> (token (and/c (mk-tag? 'LIT) (mk-value? string?)) 'STRING)
;                              (λ (_ str) str))))
;
;(define NEWLINE   (token (mk-tag? 'NEWLINE) 'NEWLINE))
;(define ENDMARKER (token (mk-tag? 'ENDMARKER) 'ENDMARKER))
;(define INDENT    (token (mk-tag? 'INDENT) 'INDENT))
;(define DEDENT    (token (mk-tag? 'DEDENT) 'DEDENT))
;
;
;(define python-literal->language
; (λ (x)
;   (match x
;     [(or "+" "-" "*" "**" "/" "//" "%"
;          "<<" ">>" "&" "|" "^" "~"
;          "<" ">" "<=" ">=" "==" "!="
;          "(" ")" "[" "]" "{" "}"
;          "," ":" "." ";" "@" "="
;          "+=" "-=" "*=" "/=" "//=" "%="
;          "&=" "|=" "^=" ">>=" "<<=" "**="
;          "..." ; not in lexical spec
;          "<>" ; not in lexical spec
;          "->" ; not in lexical spec
;          )
;      (lang (>--> (token (and/c (mk-tag? 'PUNCT) (mk-value? (λ (v) (equal? v x)))) x)
;                  [`(PUNCT ,k) k]))]
;     
;     [(or "False" "None" "True" "and" "as" "assert" "break"
;          "class" "continue" "def" "del" "elif" "else" "except" 
;          "finally" "for" "from" "global" "if" "import" "in"
;          "is" "lambda" "nonlocal" "not" "or" "pass" "raise" 
;          "return" "try" "while" "with" "yield")
;      (lang (>--> (token (and/c (mk-tag? 'KEYWORD) (mk-value? (λ (v) (equal? x (symbol->string v))))) x)
;                  [`(KEYWORD ,k) k]))]
;     
;     [else
;      ( "unknown literal: ~s~n" x)
;      (error "unknown literal")])))
;
;(set-literal->language! python-literal->language)
 
;(define py-lang
;  (grammar-from-file file_input "python-ast.grm.sx"))
;=======================================================
