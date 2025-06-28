#lang racket
;
(provide eval_ user-eval)

(require "adt.rkt"
         "parser.rkt")

;; Environment Management
(define global-env '())

(define (extend-env! var val)
  (set! global-env
        (cons (cons var val) global-env)))

(define (init-global-env)
  (begin
    (extend-env! '+ +)
    (extend-env! '- -)
    (extend-env! '* *)
    (extend-env! '/ /)
    (extend-env! '> >)
    (extend-env! '< <)
    (extend-env! 'car car)
    (extend-env! 'cdr cdr)
    (extend-env! 'cons cons)
    (extend-env! 'list list)
    (display "ge=" ) (display global-env) (newline)))

(init-global-env)

;; Look up variables in the environment
(define (lookup-helper var env)
  (cond
    [(null? env) 'unbound]
    [(equal? (car (car env)) var) (cdr (car env))]
    [else (lookup-helper var (cdr env))]))

(define (lookup var)
  (lookup-helper var global-env))

(define (is-defined? var)
  (not (eq? (lookup var) 'unbound)))

;; Primitive Procedure Predicate
(define (primitive-procedure? op)
  (procedure? op))

;; Evaluate expressions
(define (eval_ expr)
  (cond
    [(primitive? expr) expr]
    [(value? expr) expr]
    [(symbol? expr) (eval-var expr)]
    [(define? expr) (eval-define expr)]
    [(if? expr) (eval-if expr)]
    [(lambda? expr) (eval-lambda expr)]
    [(application? expr)
     (eval-application (eval_ (application-operator expr))
                       (map eval_ (application-operands expr)))]
    [else (error "Unknown expression:" expr)])
  )

;; Variable evaluation
(define (eval-var expr)
  (let ((val (lookup expr)))
    (begin (display 'val=) (display val) (newline)
    (if (eq? val 'unbound)
        (error " Unbound variable:" expr)
        val))))

;; definition
(define (eval-define expr)
  (let ((var (define-variable expr))
        (val (eval_ (define-value expr))))
    (if (is-defined? var)
        (error "Variable already defined:" var)
        (begin
          (extend-env! var val)
          var))))

;; `if` evaluation
(define (eval-if expr)
  (let ((test (eval_ (if-test expr)))
        (then-branch (if-then expr))
        (else-branch (if-else expr)))
    (if (true? test)
        (eval_ then-branch)
        (eval_ else-branch))))

(define (true? v)
  (not (eq? v #f)))



;; `lambda` evaluation
(define (eval-lambda expr)
  (make-closure (lambda-params expr)
                (lambda-body expr)))

;; Application evaluation
(define (eval-application op args)
  (cond
    [(primitive-procedure? op) (apply op args)]
    [(closure? op)
     (let ((params (closure-params op))
           (body (closure-body op)))
       (eval_ (substitute-all body params args)))]
    [else (error "Unknown operator or unbound variable:" op)]))

;; Substitution
(define (substitute expr var val)
  (cond
    [(symbol? expr) (if (eq? expr var) val expr)]
    [(lambda? expr)
     (let ((params (lambda-params expr))
           (body (lambda-body expr)))
       (if (member var params)
           expr
           (make-lambda params (substitute body var val))))]
    [(list? expr)
      (map (lambda (arg) (substitute arg var val))
           expr)]
    [else expr]))

(define (substitute-all expr vars vals)
  (if (null? vars)
      expr
      (substitute-all
       (substitute expr (car vars) (car vals))
       (cdr vars) (cdr vals))))

;; User-level evaluation
(define (user-eval input)
  (eval_ (parse-expression input)))
