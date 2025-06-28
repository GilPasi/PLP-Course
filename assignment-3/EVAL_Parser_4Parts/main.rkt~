#lang racket

(require "evaluator.rkt")
(require "parser.rkt")

;; Recursive Test Runner
(define (run-tests tests)
  (cond
    [(null? tests)
     (display "All tests completed.") (newline)]
    [else
     (let ((expr (car tests)))
       (display "Testing: ") (display expr) (newline)
       (let ((result (user-eval expr)))
         (display "Result: ") (display result) (newline)
         (run-tests (cdr tests))))]))

;; Define Tests
(define tests
  '((define x 2)
    ;(define x 3)
    (define y 5)
    (define z (lambda (x) (+ ((lambda (x) x) 1) x)))
    (z 5)

    ;...
    )

;; Run Tests
(run-tests tests)
