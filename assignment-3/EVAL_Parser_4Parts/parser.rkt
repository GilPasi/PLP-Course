#lang racket
(require "adt.rkt")

(provide (all-defined-out))

;; Main Parsing Function
(define (parse-expression expr)
  (cond
    [(atomic? expr) expr]          ; Derivaton not needed
    [(list? expr) (derive expr)]   ; Recursive
    [else
     (error "Invalid expression: ~a" expr)])) ; Invalid

(define (derive exp)
  (if (atomic? exp)
      exp
      (let ((derived-exp 
             (let ((mapped-derive-exp
                    (map derive exp)))
               (if (not (derived? exp))
                   mapped-derive-exp
                   (shallow-derive mapped-derive-exp)))))
        (if (equal? exp derived-exp)
            exp
            (derive derived-exp)))))


;; Check if an expression is derived
(define (derived? expr)
  (or (cond? expr)
      (function-definition? expr)  
      (let? expr)))

;; Shallow derive for specific expression types
(define (shallow-derive expr)
  (cond
    [(cond? expr) (cond->if expr)]
    [(function-definition? expr)
     (function-define->define expr)] 
    [(let? expr)
     (let->combination expr)]
    [else (error 'shallow-derive "Error: ~s" expr)]))

;; `cond->if`
(define (cond->if expr)
  (let ((clauses (cond-clauses expr)))
    (expand-cond-clauses clauses)))

(define (expand-cond-clauses clauses)
  (if (null? clauses)
      (error "cond->if: No matching clause found")
      (let ((clause (car clauses)))
        (if (and (symbol? (car clause))
                 (eq? (car clause) 'else))
            (if (null? (cdr clauses))
                (cadr clause)  ; Return the body of the `else` clause
                (error "cond->if: Else clause must be last"))
            (make-if (car clause)  ; Test expression
                     (cadr clause) ; Then branch
                     (expand-cond-clauses (cdr clauses))))))) ; Else branch

;; `let->combination`
(define (let->combination expr)
  (make-application
   (make-lambda (let-vars expr) (let-body expr))
   (let-vals expr)))

;; `function-define->define`
(define (function-define->define expr)
  (let ((name (function-definition-variable expr))
        (params (function-definition-parameters expr))
        (body (function-definition-body expr)))
    (make-define name (make-lambda params body)))) ; Transform to "regular" define


(define (if->cond expr)
  (if (if? expr)
      (let ((predicate (if-test expr))
            (consequent (if->cond (if-then expr)))
            (alternative (if->cond (if-else expr))))
        (make-cond (list (list predicate consequent)
                         (list 'else alternative))))
      expr))