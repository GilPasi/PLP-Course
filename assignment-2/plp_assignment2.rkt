#lang racket

;Programming Languages Principles - Assigment 2
; Authors: Gil Pasi ($ID1$) | Yulia Moshan ($ID2$)

;=============== Question 1 ===============;
;1.a
(define (make-dup-list li)  
   (if (null? li)
       li
       (let ((head (car li)) (rest (cdr li)))
         (cons head (cons head (make-dup-list rest))))))

;1.b
(define (make-list-pairs li)
  (if (< (length li) 2)
      li
      (cons (cons (car li) (cadr li)) (make-list-pairs (cddr li)))))

;1.c
(define (reverse-li li)
  (letrec ((reverse-li-helper
            (lambda (li prev)
              (if (not (null? li))
                  (reverse-li-helper (cdr li) (cons (car li) prev))
                  prev))))
    (reverse-li-helper li null)))

;1.d
(define (is-special-form? li exp)
  (and (not (null? exp))
       (not (not (member (car exp) li)))))

;1.e
(define (get-symm-helper li rev-li mid)
  (if (<= (length li) mid)
      null
      (cons (cons (car li) (car rev-li))
            (get-symm-helper (cdr li) (cdr rev-li) mid))))

(define (get-symm li)
  (if (< (length li) 2)
      li
      (get-symm-helper li (reverse-li li) (/ (length li) 2))))

;=============== Question 2 ===============;
;2.a
(define (apply-f-list li x)
  (if (null? li)
      null
      (cons ((car li) x) (apply-f-list (cdr li) x))))

;2.b
(define (apply-fs-single f-li e)
  (if (null? f-li)
      e
      (apply-fs-single (cdr f-li) ((car f-li) e))))

;=============== Question 3 ===============;
;3.a
(define (dist_pwr num1 num2 n)
  (let* ((abs (lambda (x) (if (> x 0) x (* x -1))))
         (delta (- num1 num2))
         (sign (if (and (< delta 0) (odd? n)) -1 1))
         (result (* sign (expt (expt (abs delta) n) (/ 1 n)))))
    (if (= n 1) (abs result) result)))

;3.b
(define (compute_dists li num n)
  (if (null? li)
      null
      (cons (dist_pwr (car li) num n) (compute_dists (cdr li) num n))))

;3.c
(define (compute_dists_map li num n)
  (letrec ((repeat (lambda (x len)
                     (if (<= len 0) null (cons x (repeat x (- len 1)))))))
    (map dist_pwr li (repeat num (length li)) (repeat n (length li)))))

;3.e
(define (compute_dists_pwr_range li num min_pwr max_pwr)
  (if (< max_pwr min_pwr)
      null
      (cons (compute_dists li num min_pwr)
            (compute_dists_pwr_range li num (+ min_pwr 1) max_pwr))))


;Dear assignment grader! Please ignore this section, it's for my one testing
(provide
  make-dup-list
  make-list-pairs
  reverse-li
  is-special-form?
  get-symm
  apply-f-list
  apply-fs-single
  dist_pwr
  compute_dists
  compute_dists_map
  compute_dists_pwr_range
)

(make-dup-list (list 1 2 3 4))