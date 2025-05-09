#lang racket

;Programming Languages Principles - Assigment 2
; Authors: Gil Pasi ($ID1$) | Yulia Moshan ($ID2$)

;Preconditions to any list in the assisgment: Cannot have any null member which is not the end of the list.

;=============== Question 1 ===============;
;1.a
;Type: [List(T) -> List(T)]
;Preconditions: No null elements other than the tail element
;Tests: (make-dup-list (list 1 2 3 4)) -> (1 1 2 2 3 3 4 4)
(define (make-dup-list li)  
   (if (null? li)
       li
       (let ((head (car li)) (rest (cdr li)))
         (cons head (cons head (make-dup-list rest))))))

;1.b
;Type: [List(T) -> List(Pair(T))
;Preconditions: Length(list) >= 2, Length(list)|2 , No null elements other than the tail element
;Tests: (make-list-pairs (list 1 2 3 4)) -> ((1 . 2) (3 . 4))
(define (make-list-pairs li)
  (if (< (length li) 2)
      li
      (cons (cons (car li) (cadr li)) (make-list-pairs (cddr li)))))


;1.c
;Type: [List(T) -> List(T)]
;Preconditions: No null elements other than the tail element
;Tests (reverse-li (list 1 2 3 4)) -> (4 3 2 1)
(define (reverse-li li)
  (letrec ((reverse-li-helper
            (lambda (li prev)
              (if (not (null? li))
                  (reverse-li-helper (cdr li) (cons (car li) prev))
                  prev))))
    (reverse-li-helper li null)))

;1.d
;Type: [List(Symbol) -> Boolean]
;Preconditions: length(list) > 0, Each element must be a symbol of a real special form
;Tests: (is-special-form? '(if lambda define) '((lambda (x)x)1)) -> #f
;       (is-special-form? '(if lambda define) '(lambda (x)x))    -> #t
(define (is-special-form? li exp)
  (and (not (null? exp))
       (not (not (member (car exp) li)))))

;1.e
;Type: [ List(T) -> List(Pair(T))]
;Preconditions: length(list) >= 2, No null elements other than the tail element
;Tests: (get-symm (list 1 5 7 8 9)) -> ((1 . 9) (5 . 8) (7 . 7))
;       (get-symm (list 1 5 7 8))   -> ((1 . 8) (5 . 7))

;Recursive helper:
;Type: [List(T)*List(T)*Number -> List(Pair(T))]
;Preconditions: length(li) = length(li-rev) >= 2, for any i : li[i] = li-rev[length(li) - i], mid = length(li) / 2
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
;Type: [List([Number->Number])*Number -> List(Number)]
;Preconditions: No null elements other than the tail element
;Tests:
;(apply-f-list
; (list (lambda (x) (+ x 1))
;       (lambda (x) (* x 3))
;       (lambda (x) x)
;       (lambda (x) (- 5 x)))
; 1
; )  ->  (2 3 1 4)
(define (apply-f-list li x)
  (if (null? li)
      null
      (cons ((car li) x) (apply-f-list (cdr li) x))))

;2.b
;Type: [List([T->T])*T -> T]
;Preconditions: No null elements other than the tail element
;Tests: (apply-fs-single (list
;                  (lambda (x) (* x 2))
;                  (lambda (x) (* x 10))
;                  (lambda (x) (+ x 1)))
;                 1) -> 21
(define (apply-fs-single f-li e)
  (if (null? f-li)
      e
      (apply-fs-single (cdr f-li) ((car f-li) e))))

;=============== Question 3 ===============;
;3.a
;Type: [Number*Number*Number -> Number]
;Preconditions: n is a natual number
;Tests: (dist_pwr 1 3 1) -> 2
(define (dist_pwr num1 num2 n)
  (expt (abs (- (expt num1 n) (expt num2 n))) (/ 1 n))
)

;3.b
;Type: [List(Number)*Number*Number* -> List(Number)]
;Preconditions: None
;Tests: (compute_dists_map '(1 2 3 4 5) 7 5) -> '(6.999916699391883 6.997332411605517 6.979640344934702 6.9125440412768695 6.717844041250845)
(define (compute_dists li num n)
  (if (null? li)
      null
      (cons (dist_pwr (car li) num n) (compute_dists (cdr li) num n))))

;3.c
;Type: [List(Number)*Number*Number* -> List(Number)]
;Preconditions: None
;Tests: (compute_dists_map '(1 2 3 4 5) 7 5) -> '(6.999916699391883 6.997332411605517 6.979640344934702 6.9125440412768695 6.717844041250845)
(define (compute_dists_map li num n)
  (letrec ((repeat (lambda (x len)
                     (if (<= len 0) null (cons x (repeat x (- len 1)))))))
    (map dist_pwr li (repeat num (length li)) (repeat n (length li)))))

;3.d
;Type: [List(Number)*Number*Number*Number*Number -> List(Number)]
;Preconditions: min > max
;Tests: (dists_in_range '(1 2 3 4 5) 7 5 6.9 10) -> '(6.717844041250845)
(define (dists_in_range li num n min max)(filter
                                          (lambda (e)(or (<= e min) (>= e max)))
                                          (compute_dists_map li num n))
)


;3.e
;Type: [List(Number)*Number*Number*Number -> List(List(Number))]
;Preconditions: min_pwr and max_pwr are a natual numbers, max_pwr > min_pwr, No null elements other than the tail element
;Tests: (compute_dists_pwr_range (list 1 2 3 4) 5 2 5) -> ((4 3 2 1) (-3.9999999999999996 -3.0 -2.0 -1) (4.0 3.0 2.0 1) (-4.0 -3.0 -2.0 -1))
(define (compute_dists_pwr_range li num min_pwr max_pwr)
  (if (< max_pwr min_pwr)
      null
      (cons (compute_dists li num min_pwr)
            (compute_dists_pwr_range li num (+ min_pwr 1) max_pwr))))

;Type: [List(Number)*Number -> List(List(Number))]
;Preconditions: None
;Tests: (dist_mat (list 2 4 10) 2 ) -> '((0 3.4641016151377544 9.797958971132712) (3.4641016151377544 0 9.16515138991168) (9.797958971132712 9.16515138991168 0))
;3.f
(define (dist_mat li num)
   (letrec (
            (dist_mat_helper (lambda (li num li-origin)
                               ( if (null? li)
                                    null
                                    (cons
                                     (compute_dists_map li-origin (car li) num)
                                     (dist_mat_helper (cdr li) num li-origin))))))
     (dist_mat_helper li num li))
)




;Dear assignment grader! Please ignore this section, it's for my own testing
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
  dists_in_range
  compute_dists_pwr_range
  dist_mat_helper
)