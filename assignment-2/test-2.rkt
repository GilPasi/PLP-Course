#lang racket
(require "plp_assignment2.rkt")

;=============== Tests ===============;
(define (run-test label actual expected)
  (let ((result (equal? actual expected)))
    (displayln (string-append "Test: " label))
    (displayln (string-append "  Expected: " (format "~a" expected)))
    (displayln (string-append "  Actual:   " (format "~a" actual)))
    (display "  Result:   ")
    (display (if result "\033[32mPASS\033[0m" "\033[31mFAIL\033[0m"))
    (newline) (newline)))

(run-test "make-dup-list"
          (make-dup-list (list 1 2 3 4))
          (list 1 1 2 2 3 3 4 4))

(run-test "make-list-pairs"
          (make-list-pairs (list 1 2 3 4))
          (list (cons 1 2) (cons 3 4)))

(run-test "reverse-li"
          (reverse-li (list 1 2 3 4))
          (list 4 3 2 1))

(run-test "is-special-form? #1"
          (is-special-form? '(if lambda define) '())
          #f)

(run-test "is-special-form? #2"
          (is-special-form? '(if lambda define) '(lambda (x) x))
          #t)

(run-test "is-special-form? #3"
          (is-special-form? '(if lambda define) '((lambda (x) x) 2))
          #f)

(run-test "get-symm #1"
          (get-symm (list 1 5 7 8))
          (list (cons 1 8) (cons 5 7)))

(run-test "get-symm #2"
          (get-symm (list 1 5 7 8 9))
          (list (cons 1 9) (cons 5 8) (cons 7 7)))

(run-test "apply-f-list"
          (apply-f-list (list (lambda (x) (+ x 1))
                              (lambda (x) (* x 3))
                              (lambda (x) x)
                              (lambda (x) (- 5 x))) 1)
          (list 2 3 1 4))

(run-test "apply-fs-single"
          (apply-fs-single (list (lambda (x) (* x 2))
                                 (lambda (x) (* x 10))
                                 (lambda (x) (+ x 1)))
                           1)
          21)

(run-test "dist_pwr #1"
          (dist_pwr 1 3 1)
          2)

(run-test "dist_pwr #2"
          (dist_pwr 1 3 3)
          2.9624960684073702)

(run-test "compute_dists"
          (compute_dists (list 1 2 3 4) 5 2)
          '(4.898979485566356 4.58257569495584 4 3))

(run-test "compute_dists_map"
          (compute_dists_map (list 1 2 3 4) 5 2)
          '(4.898979485566356 4.58257569495584 4 3))

(run-test "compute_dists_pwr_range"
          (compute_dists_pwr_range (list 1 2 3 4) 5 2 5)
          '((4.898979485566356 4.58257569495584 4 3)
            (4.986630952238645 4.890973246508748 4.610436292058446 3.936497183102173)
            (4.997998798878767 4.96768813015734 4.829472805532836 4.382849839122776)
            (4.999679959032134 4.989717797400289 4.919701979080198 4.618332642994932)))

(run-test "dist_mat"
          (dist_mat (list 2 4 10) 2)
          (list
           (list 0 3.4641016151377544 9.797958971132712)
           (list 3.4641016151377544 0 9.16515138991168)
           (list 9.797958971132712 9.16515138991168 0)))

