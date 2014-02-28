#lang racket

(require "pytoc-common.rkt")

;Srikanth Manikarnike
;U0706564

;; Program emission.

(define (mangle-char c)
  (cond 
    [(eqv? c #\$)             "$"]
    [(char-numeric? c)        (list->string (list c))]
    [(char-alphabetic? c)     (list->string (list c))]
    [else (string-append "_" (number->string (char->integer c)))]))

(define (mangle symbol)
  (match symbol
    ['$halt    'halt]
    [else
     (format "__~a" 
             (string-append* (map mangle-char (string->list (symbol->string symbol)))))]))
           
(define (mangle-label label)
  (mangle label))



(define (emit-program program)
  
  (mark-globals! program)
  
  ; create a var for each label
  
  (match program
    [`(program ,defs ... ,exp)
      ; =>
     
     
     
     (printf "
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>

#define INLINE __inline__


#define NEW(type) ( (type*) malloc(sizeof(type)) )

/* Run-time values. */

typedef enum { FALSE = 0
             , CLOSURE
             , INT
             , VOID
             , TRUE
             , NIL
             , STRING
             , CELL
             , DICT
             , SET
             , LIST
             , TUPLE 
             , PAIR } tag_t ;

typedef void (*func_ptr)() ;

union val_t ;
struct list_t ;
struct node_t ;
struct dict_t ;

typedef struct list_t list_t ;
typedef struct node_t node_t ;
typedef struct dict_t dict_t ;
typedef union val_t val_t ;



struct clo_t {
  tag_t tag ;
  func_ptr f ;
  void* env ;
} ;


union val_t {
  tag_t tag ;
  struct clo_t clo ;
  struct {
    tag_t tag ;
    union val_t* value ;
  } c ; 
  struct {
    tag_t tag ;
    int value ;
  } z ;
  struct {
    tag_t tag ;
    char* value ;
  } s ;
  struct {
    tag_t tag ;
    struct list_t* list ;
  } l;
  struct {
    tag_t tag ;
    struct node_t* tuple ;
  } t ;
  struct {
    tag_t tag ;
    struct dict_t* dict ;
  } d ;
  struct {
    tag_t tag ;
    val_t* car ;
    val_t* cdr ;
  } p ;
} ;


struct node_t {
  val_t value ;
  struct node_t* next ;
} ;

struct list_t {
  node_t* head ;
} ;


struct dict_t {
  node_t* keys ;
  node_t* values ;
} ;


/* INLINE constructors. */

static INLINE
val_t make_clo(func_ptr f,void* env) {
  val_t ret ;
  ret.tag = CLOSURE ;
  ret.clo.f = f ;
  ret.clo.env = env ;
  return ret ;
}

static INLINE 
val_t make_int(int z) {
  val_t ret ;
  ret.tag = INT ;
  ret.z.value = z ;
  return ret ;
}

static INLINE
val_t make_bool(int b) {
  val_t ret ;
  if (b) {
    ret.tag = TRUE ;
  } else {
    ret.tag = FALSE ;
  }
  return ret ;
}

static INLINE
val_t make_string(char* s) {
  val_t ret ;
  ret.tag = STRING ;
  ret.s.value = strdup(s) ;
  return ret ;
}

static INLINE
val_t make_cell(val_t value) {
  val_t ret ;
  ret.tag = CELL ;
  ret.c.value = NEW(val_t) ;
  *(ret.c.value) = value ;
  return ret ;
}

static INLINE
val_t get_cell(val_t cell) {
  assert(cell.tag == CELL) ;
  return *(cell.c.value) ;
}

static INLINE
val_t make_cons(val_t car, val_t cdr) {
  val_t ret ;

  ret.tag = PAIR ;

  ret.p.car = NEW(val_t) ;
  ret.p.cdr = NEW(val_t) ;
  
  *ret.p.cdr = car ;
  *ret.p.cdr = cdr ;

  return ret ;
}


/* Temporaries. */

val_t tclo ;


/* Constants. */ 

val_t VVOID ;
val_t VNIL ;

val_t BFALSE ;
val_t BTRUE ;

val_t halt ;



/* Primitive operations. */

static INLINE 
val_t equalp(val_t a, val_t b) {
 if (a.tag != b.tag)
   return BFALSE ;
 switch (a.tag) {
   case INT:
     if (a.z.value == b.z.value)
       return BTRUE ; 
     else
       return BFALSE ;
   break ;
   
   default:
    printf(\"cannot compare\\n\") ;
   exit(5) ;
   break ;
 }
}

void halt_proc(void* env, val_t rv) {
  exit(0) ;
}

void print_rawvalue(val_t v) {
  switch (v.tag) {
    case INT:
    printf(\"%i\",v.z.value) ;
    break ;

    case STRING:
    printf(\"%s\",v.s.value) ;
    break ;

    case DICT: {
     node_t* ks = v.d.dict->keys ;
     node_t* vs = v.d.dict->values ;
     int first = 1 ;
     printf(\"{\") ;
     while (ks != NULL) {
       if (first)
         first = 0 ; 
       else
         printf(\", \") ;
       print_rawvalue(ks->value) ;
       printf(\": \") ;
       print_rawvalue(vs->value) ;
       ks = ks->next ;
       vs = vs->next ;
     }
     printf(\"}\") ;
    
    } break ;

    case LIST: {
     node_t* n = v.l.list->head ;
     int first = 1 ;
     printf(\"[\") ;
     while (n != NULL) {
       if (first)
         first = 0 ; 
       else
         printf(\", \") ;
       print_rawvalue(n->value) ;
       n = n->next ;
     }
     printf(\"]\") ;
    } break ;

    default:
    printf(\"#value\") ;
    break ;
  }
  fflush(stdout) ;
}

void print_value(val_t v) {
  print_rawvalue(v) ;
  printf(\"\\n\") ;
}  


/* Collections */


node_t* make_node(val_t value, node_t* next)  {
  node_t* node = NEW(node_t) ;
  node->value = value ;
  node->next = next ;
  return node ;
} 


val_t node_ref(node_t* node, val_t index) {
  int i ;
  
  assert(index.tag == INT) ;

  i = index.z.value ;
 
  while (i > 0) {
    assert(node != NULL) ;
    node = node->next ;
    --i ;
  }

  assert(node != NULL) ;
  return node->value ;
}



/* Tuples. */

val_t make_tuple(unsigned int len, ...) {
  va_list ap ;
  val_t ret ;

  node_t** next ;

  ret.tag = TUPLE ;
  ret.t.tuple = NEW(node_t) ;
 
  next = &ret.t.tuple ;
 
  va_start(ap, len) ;
  while (len > 0) {
    *next = make_node(va_arg(ap,val_t), NULL) ;
    next = &(*next)->next ;
    len-- ;
  }
  va_end(ap) ;
  *next = NULL ;

  return ret ;
}


val_t tuple_ref(val_t tuple, val_t index) {
  assert (tuple.tag == TUPLE) ;
  return node_ref(tuple.t.tuple,index) ;
} ;


void tuple_set(val_t tuple, val_t index, val_t value) {
  printf(\"tuple set\\n\") ;
  exit(6) ;
}




/* Lists. */

val_t make_list(unsigned int len, ...) {
  va_list ap ;
  val_t ret ;
  node_t** next ;

  ret.tag = LIST ;
  ret.l.list = NEW(list_t) ;
 
  next = &ret.l.list->head ;
 
  va_start(ap, len) ;
  while (len > 0) {
    *next = make_node(va_arg(ap,val_t), NULL) ;
    next = &(*next)->next ;
    len-- ;
  }
  va_end(ap) ;
  *next = NULL ;

  return ret ;
}



void list_set(val_t list, val_t index, val_t value) {
  node_t* n; 
  int i ;
  assert(list.tag == LIST) ;
  assert(index.tag == INT) ;
  
  n = list.l.list->head ;
  i = index.z.value ;

  while (i > 0) {
    assert(n != NULL) ;
    n = n->next ;
    --i ;
  }

  n->value = value ;  
} 


void list_remove(val_t list, val_t index) {
  node_t* n; 
  node_t** ln ;
  int i ;
  assert(list.tag == LIST) ;
  assert(index.tag == INT) ;
  
  n = list.l.list->head ;
  i = index.z.value ;
  ln = &list.l.list->head ;
 

  assert(i > 0) ;

  while (i > 0) {
    assert(n != NULL) ;
    ln = &n->next ;
    n = n->next ;
    --i ;
  }

  *ln = (*ln)->next ;
} 

val_t list_ref(val_t list, val_t index) {
  assert (list.tag == LIST) ;
  return node_ref(list.l.list->head,index) ;
} ;


/* Dictionaries. */

val_t make_dict(unsigned int len, ...) {
  va_list ap ;
  val_t ret ;
  node_t** knext ;
  node_t** vnext ;

  ret.tag = DICT ;
  ret.d.dict = NEW(dict_t) ;
 
  knext = &ret.d.dict->keys ;
  vnext = &ret.d.dict->values ;
 
  assert(len >= 0) ;

  va_start(ap, len) ;
  while (len > 0) {
    *knext = make_node(va_arg(ap,val_t), NULL) ;
    *vnext = make_node(va_arg(ap,val_t), NULL) ;

    knext = &(*knext)->next ;
    vnext = &(*vnext)->next ;
    len-- ;
  }
  va_end(ap) ;
  *knext = NULL ;
  *vnext = NULL ;

  return ret ;
}

val_t dict_ref (val_t dict, val_t index) {
  node_t* k ;
  node_t* v ;
  val_t cond ;

  assert(dict.tag == DICT) ;
  
  k = dict.d.dict->keys ;
  v = dict.d.dict->values ;

  do {
    assert(k != NULL) ;
    assert(v != NULL) ;
    cond = equalp(index, k->value) ;
    if (cond.tag == TRUE) {
      return v->value ;
    }
    k = k->next ;
    v = v->next ;
  } while (k != NULL) ;
 
  assert(0 || \"no key in dictionary!\") ;
}


void dict_set(val_t dict, val_t index, val_t value) {
  node_t* k ;
  node_t* v ;
  val_t cond ;

  assert(dict.tag == DICT) ;
  
  k = dict.d.dict->keys ;
  v = dict.d.dict->values ;

  do {
    cond = equalp(index, k->value) ;
    if (cond.tag == TRUE) {
      v->value = value ;
    }
    k = k->next ;
    v = v->next ;
  } while (k != NULL) ;
 
  k = dict.d.dict->keys ;
  v = dict.d.dict->values ;

  dict.d.dict->keys = make_node(index, k) ;
  dict.d.dict->values = make_node(value, v) ;
}


void dict_remove(val_t dict, val_t index) {
  node_t* k ;
  node_t* v ;

  node_t** lk ;
  node_t** lv ;
  val_t cond ;

  assert(dict.tag == DICT) ;
  
  k = dict.d.dict->keys ;
  v = dict.d.dict->values ;

  lk = &dict.d.dict->keys ;
  lv = &dict.d.dict->values ;

  do {
    assert(k != NULL) ;
    assert(v != NULL) ;
    cond = equalp(index, k->value) ;
    if (cond.tag == TRUE) {
      *lv = (*lv)->next ;
      *lk = (*lk)->next ;
      return ;
    }
    lk = &k->next ;
    lv = &v->next ;

    k = k->next ;
    v = v->next ;
  } while (k != NULL) ;
 
}


val_t dict_in(val_t key, val_t dict) {
  node_t* k = NULL ;
  node_t* v = NULL ;
  val_t cond ;

  assert(dict.tag == DICT) ;

  k = dict.d.dict->keys ;
  v = dict.d.dict->values ;

  do {
    cond = equalp(key, k->value) ;
    if (cond.tag == TRUE) {
      return BTRUE ;
    }
    k = k->next ;
    v = v->next ;
  } while (k != NULL) ;
 
  return BFALSE ;
} 


/* Sets. */



/* Collection objects. */

val_t in (val_t needle, val_t haystack) {
  switch (haystack.tag) {
    case DICT:
      return dict_in(needle,haystack) ;
    default:
      printf(\"error: in function not finished\\n\") ;
      exit(1) ;
      break ;
  }
} 


/* Iteration */

void cont_for_node_k(void* env, val_t rv) ;
void for_node_k(val_t f, node_t* node, val_t k) ;

struct for_env {
  val_t f ;
  node_t* node ;
  val_t k ;
}; 

struct for_env* env_for_node_k(val_t f, node_t* next, val_t k) {
  struct for_env* e = NEW(struct for_env) ;
  e->f = f ;
  e->node = next ;
  e->k = k ;
  return e ;
} 

void cont_for_node_k(void* env, val_t rv) {
  struct for_env* e = env ;
  for_node_k(e->f, e->node, e->k) ;
}

void for_node_k(val_t f, node_t* node, val_t k) {
  assert(k.tag == CLOSURE) ;
  assert(f.tag == CLOSURE) ;
  if (node == NULL) {
    k.clo.f(k.clo.env,VVOID) ;
  } else {
    f.clo.f(f.clo.env, node->value,
            make_clo(cont_for_node_k,
                     env_for_node_k(f,node->next,k))) ;
  }
}

static INLINE
void for_list_k(val_t lst, val_t f, val_t k) {
  assert(f.tag == CLOSURE) ;
  assert(lst.tag == LIST) ;
  assert(k.tag == CLOSURE) ;
  for_node_k(f,lst.l.list->head,k) ;
}


static INLINE
void for_dict_k(val_t f, val_t dict, val_t k) { 
  printf(\"implement for_dict_k\\n\");
  exit(2) ;
}

static INLINE
void for_set_k(val_t f, val_t dict, val_t k) {
  printf(\"implement for_set_k\\n\");
  exit(3) ;
}

static INLINE
void for_tuple_k(val_t f, val_t dict, val_t k) {
  printf(\"implement for_tuple_k\\n\");
  exit(4) ;
}



/* Initialization. */

void init () {
  halt.tag = CLOSURE ;
  halt.clo.f = halt_proc ;
  halt.clo.env = NULL ;

  VVOID.tag = VOID ;

  VNIL.tag = NIL ;

  BFALSE.tag = FALSE ;
  BTRUE.tag = TRUE ;
}


")
     
     (for ([d defs])
       (emit-def-prototype d))
     
     (for ([d defs])
       (emit-def d))
     
     (display "int main() { init() ;")
     (newline)
     (display (exp->c exp))
     (newline)
     (display " return 0;")
     (newline)
     (display "}")
     (newline)
     ]))


(define (emit-def-prototype def)
  (match def
    [`(define ,(and (? symbol?) v) (void))
     (printf "val_t ~a;~n" (mangle v))]
    
    [`(define-env ,name ,fields)
     (printf "struct ~a ;~n"
             (mangle name))
     (printf "struct ~a {~n~a};~n" 
             (mangle name)
             (string-append*
              (map (λ (f) (format "val_t ~a ;~n" (mangle f))) fields)))
     (printf "typedef struct ~a ~a;~n" 
             (mangle name)
             (mangle name))
     (printf "~a* env_~a(~a);~n" 
             (mangle name)
             (mangle name)
             (string-join (map (λ (_) "val_t") fields) ", "))]

    [`(define-label ,name ,lam)
     (match lam 
       [`(lambda (,e ,ps ...) ,_)
        (define ep (format "void* ~a" (mangle e)))
        (define pfs (map (λ (p) (format "val_t ~a" (mangle p))) ps))
        (define params (string-join (cons ep pfs) ", "))
        (printf "void ~a(~a) ;~n" (mangle-label name) params)])]
    
    [else
     (error (format "emit-def-prototype fail; not a define: ~s~n" def))]))


(define (emit-def def)
  (match def
    [`(define ,(and (? symbol?) v) (void))
     (void)]
    
    [`(define-env ,name ,fields)
     (printf "~a* env_~a(~a) {~a}~n" 
             (mangle name)
             (mangle name)
             (string-join (map (λ (f) (format "val_t ~a" (mangle f))) fields) ", ")
             (let* ([type (mangle name)])
               (format "
  ~a* ret = malloc(sizeof(~a));
  ~a
  return ret ;~n" type type
                  (string-append*
                   (map (λ (f) (format "ret->~a = ~a ;" (mangle f) (mangle f))) fields)))))]
         
    [`(define-label ,name ,lam)
     (match lam 
       [`(lambda (,e ,ps ...) ,(app exp->c body))
        (define ep (format "void* ~a" (mangle e)))
        (define pfs (map (λ (p) (format "val_t ~a" (mangle p))) ps))
        (define params (string-join (cons ep pfs) ", "))
        (printf "void ~a(~a) {~n~a~n}~n" (mangle-label name) params body)])]
    
    [else
     (error (format "emit-def fail; not a define: ~s~n" def))]))




(define (basic->c exp)
  (match exp
    
    [(? number?)      (format "make_int(~a)" exp)]
    [(? symbol?)      (mangle exp)]
    
    [(? string?)      (format "make_string(~v)" exp)]
    
    ['(void)          "VVOID"]
    
    [#t               "BTRUE"]
    [#f               "BFALSE"]
    
    [`(lambda-label ,name)
     (mangle-label name)]

    [else 
     (error (format "basic exp not handled: ~s~n" exp))]))

(define (functor->c f)
  (match f
    ['set        "make_set"]
    ['tuple      "make_tuple"]
    ['py-list*   "make_list"]
    ['make-cell  "make_cell"]
    ['get-cell   "get_cell"]
    
    [else (error (format "no match for functor: ~s~n" f))]))


(define (app-cont cont rv)
  (exp->c `(app* ,cont ,rv)))
         




(define (primapp->c exp)
  (match exp
    [`((cps py-print) ,arg ,cont)
     (format " print_value(~a);~n~a" 
             (exp->c arg)
             (app-cont cont `(c-quote "VVOID")))]
    
    [`((cps -) ,arg ,cont)
     (app-cont cont `(c-quote ,(format "(make_int(-(~a.z.value)))" (exp->c arg))))]

    [`((cps ,(and rel (or '< '>))) ,arg1 ,arg2 ,cont)
     (app-cont
      cont 
      `(c-quote ,(format "(make_bool((~a.z.value) ~a (~a.z.value)))" 
                        (exp->c arg1) 
                        rel
                        (exp->c arg2))))]
    
    [`((cps ,(and op (or '* '- '+))) ,arg1 ,arg2 ,cont)
     (app-cont
      cont 
      `(c-quote ,(format "(make_int((~a.z.value) ~a (~a.z.value)))" 
                        (exp->c arg1) 
                        op
                        (exp->c arg2))))]
    
    [`((cps equal?) ,arg1 ,arg2 ,cont)
     (app-cont
      cont 
      `(c-quote ,(format "equalp(~a,~a)" 
                        (exp->c arg1) 
                        (exp->c arg2))))]
    
    [`((cps dict?) ,arg ,cont)
     (app-cont
      cont 
      `(c-quote ,(format "make_bool((~a).tag == DICT)" 
                         (exp->c arg))))]
    
    [`((cps set?) ,arg ,cont)
     (app-cont
      cont 
      `(c-quote ,(format "make_bool((~a).tag == SET)" 
                         (exp->c arg))))]
                        
    [`((cps py-list?) ,arg ,cont)
     (app-cont
      cont 
      `(c-quote ,(format "make_bool((~a).tag == LIST)" 
                         (exp->c arg))))]
    
    [`((cps tuple?) ,arg ,cont)
     (app-cont
      cont 
      `(c-quote ,(format "make_bool((~a).tag == TUPLE)" 
                         (exp->c arg))))]
    
    [`((cps tuple-set!) ,tup ,index ,value ,cont)
     (format " tuple_set(~a,~a,~a);~n~a"
             (exp->c tup)
             (exp->c index)
             (exp->c value)
             (app-cont cont `(c-quote "VVOID")))]
    
    [`((cps dict-remove!) ,dict ,index ,cont)
      (format " dict_remove(~a,~a) ;~n ~a"
              (exp->c dict)
              (exp->c index)
              (app-cont cont `(c-quote "VVOID")))]
    
    [`((cps py-list-remove!) ,list ,index ,cont)
     (format " list_remove(~a,~a) ;~n ~a"
             (exp->c list)
             (exp->c index)
             (app-cont cont `(c-quote "VVOID")))]
        
    
    [`((cps dict-ref) ,dict ,index ,cont)
      (app-cont cont `(c-quote ,(format " dict_ref(~a,~a)"
                                        (exp->c dict)
                                        (exp->c index))))]

    [`((cps tuple-ref) ,tuple ,index ,cont)
      (app-cont cont `(c-quote ,(format " tuple_ref(~a,~a)"
                                        (exp->c tuple)
                                        (exp->c index))))]
    
    
    [`((cps py-list-ref) ,list ,index ,cont)
      (app-cont cont `(c-quote ,(format " list_ref(~a,~a)"
                                        (exp->c list)
                                        (exp->c index))))]
    
    [`((cps dict-set!) ,dict ,index ,value ,cont)
     (format " dict_set(~a,~a,~a);~n~a"
             (exp->c dict)
             (exp->c index)
             (exp->c value)
             (app-cont cont `(c-quote "VVOID")))]
    
    [`((cps py-list-set!) ,lst ,index ,value ,cont)
     (format " list_set(~a,~a,~a);~n~a"
             (exp->c lst)
             (exp->c index)
             (exp->c value)
             (app-cont cont `(c-quote "VVOID")))]
    
    
    [`((cps in?) ,needle ,haystack ,cont)
     (app-cont cont `(c-quote ,(format "in(~a,~a)"
                                       (exp->c needle)
                                       (exp->c haystack))))]
    
    
    [else 
     (error (format "no primapp match for: ~s~n" exp))]))
     





(define (exp->c exp)
  
  (match exp
    
    [`(c-quote ,c-exp)
     c-exp]
    
    [(? basic?)   
     (basic->c exp)]
    
    [`(,(and f (or 'set 'tuple 'py-list*)) ,exps ...)
     (define len (number->string (length exps)))
     (format "~a(~a)"
             (functor->c f)
             (string-join (cons len (map e->c exps)) ", "))]
    
    [`(,(and f (or 'make-cell 'get-cell)) ,exps ...)
     (format "~a(~a)"
             (functor->c f)
             (string-join (map e->c exps) ", "))]
    
    [`(if ,cond ,expt ,expf)
     (format " if ((~a).tag != FALSE) {~n ~a~n} else {~n ~a~n}~n"
             (e->c cond)
             (e->c expt)
             (e->c expf))]
    
    
    [`(make-env ,eid (,ks ,vs) ...)
     (string-append "env_" (mangle eid)
                    "("
                    (string-join (map e->c vs) ", ")
                    ")")]
    
    [`(make-closure ,lam ,env)
     (string-append "make_clo(" (e->c lam) "," (e->c env) ")")]

    [`(dict)
     "make_dict(0)"]
    
    
    [`(dict (,ks ,vs) ...) 
     (format "make_dict(~a ~a)"
             (length ks)
             (string-append*
              (for/list ([kv (map list ks vs)])
                (match kv
                  [`(,k ,v) (format ", ~a, ~a" 
                                    (exp->c k)
                                    (exp->c v))]))))]
    
    [`(,(or 'λ 'lambda) ,vars ,body)
     (error (format "misplaced lambda: ~s~n" exp))]
     
    [`((cps ,primop) ,args ... ,cont)
     (primapp->c exp)]
    
    [`(error ,msg . ,_)
     (format " printf(\"error: \"); print_value(~a);~n" (exp->c msg))]
    
    [`(set-then! ,v ,e ,body)
     (format " ~a = ~a ;~n~a" 
             (mangle v)
             (e->c e)
             (e->c body))]
    
    [`(set-cell! ,c ,e ,body)
     (format " *((~a).c.value) = ~a ;~n~a"
             (e->c c)
             (e->c e)
             (e->c body))]
        
    [`(env-ref ,eid ,env ,field)
     (format "(((~a*)~a)->~a)" (mangle eid) (mangle env) (mangle field))]
       
    [`(app* for-py-list-k ,f ,lst ,k)
     (format "for_list_k(~a, ~a, ~a);"
             (exp->c f)
             (exp->c lst)
             (exp->c k))]
    
    [`(app* for-set-k ,f ,lst ,k)
     (format "for_set_k(~a, ~a, ~a);"
             (exp->c f)
             (exp->c lst)
             (exp->c k))]
    
    [`(app* for-tuple-k ,f ,lst ,k)
     (format "for_tuple_k(~a, ~a, ~a);"
             (exp->c f)
             (exp->c lst)
             (exp->c k))]
    
    [`(app* for-dict-k ,f ,lst ,k)
     (format "for_dict_k(~a, ~a, ~a);"
             (exp->c f)
             (exp->c lst)
             (exp->c k))]
    
    
    [`(app* ,f . ,args) 
     (define c-args
       (string-append*
        (for/list ([a args])
          (format ", ~a" (exp->c a)))))
     (format " tclo = ~a;~n assert(tclo.tag == CLOSURE);~n tclo.clo.f(tclo.clo.env~a); ~n "
             (e->c f)
             c-args)]
             

    
    [else
     (error (format "emission fail: ~s~n" exp))]))
      
(define e->c exp->c)

(emit-program (read))
     