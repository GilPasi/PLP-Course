;Programming Languages Principles - Assigment 2
; Authors: Gil Pasi ($ID1$) | Yulia Moshan ($ID2$)


;=============== Question 1 ===============;
;1.a
;Type: [List(T) -> List(T)]
;Preconditions: No null elements other than the tail element
;Tests: (make-dup-list (list 1 2 3 4)) -> (1 1 2 2 3 3 4 4)
(define (make-dup-list li)  
   {
    if (null? li)
       li
       (let ((head (car li)) (rest (cdr li)))
        (cons
        head
        (cons head
              (make-dup-list rest)
        )
      )
    )
  }
)
(equal? (make-dup-list (list 1 2 3 4)) (list 1 1 2 2 3 3 4 4))
;1.b
;Type: [List(T) -> List(Pair(T))
;Preconditions: Length(list) >= 2, Length(list)|2 , No null elements other than the tail element
;Tests: (make-list-pairs (list 1 2 3 4)) -> ((1 . 2) (3 . 4))
(define (make-list-pairs li)
  {
   if (< (length li) 2) li
      ( cons 
              (cons (car li) (cadr li))
              (make-list-pairs (cddr li)) 
          )
      }
)
(equal? (make-list-pairs (list 1 2 3 4)) (list (cons 1 2) (cons 3 4)))

;1.c
;Type: [List(T) -> List(T)]
;Preconditions: No null elements other than the tail element
;Tests (reverse-li (list 1 2 3 4)) -> (4 3 2 1)
(define (reverse-li li)
  {
   letrec [(reverse-li-helper
            (lambda (li prev)
              {
               if
               (not (null? li))
               (reverse-li-helper (cdr li) (cons (car li) prev))
               prev
               }
            ))]
    (reverse-li-helper li null)
    }
  )

(equal? (reverse-li (list 1 2 3 4)) (list 4 3 2 1))

;1.d
;Type: [List(Symbol) -> Boolean]
;Preconditions: length(list) > 0, Each element must be a symbol of a real special form
;Tests: (is-special-form? '(if lambda define) '((lambda (x)x)1)) -> #f
;       (is-special-form? '(if lambda define) '(lambda (x)x))    -> #t
(define (is-special-form? li exp)
   (and
    (not(pair? (car exp)))
    (not(not (member (car exp) li))) ;(not(not ...))  Coverts a truthy value into true
    )
)

(eq? (is-special-form? '(if lambda define) '((lambda (x)x)1)) #f)
(eq? (is-special-form? '(if lambda define) '(lambda (x)x)) #t)

;1.e
;Type: [ List(T) -> List(Pair(T))]
;Preconditions: length(list) >= 2, No null elements other than the tail element
;Tests: (get-symm (list 1 5 7 8 9)) -> ((1 . 9) (5 . 8) (7 . 7))
;       (get-symm (list 1 5 7 8))   -> ((1 . 8) (5 . 7))

;Recursive helper:
;Type: [List(T)*List(T)*Number -> List(Pair(T))]
;Preconditions: length(li) = length(li-rev) >= 2, for any i : li[i] = li-rev[length(li) - i], mid = length(li) / 2
(define (get-symm-helper li rev-li mid)
  {
   if (<= (length li) mid)
      null
      (cons (cons (car li ) (car rev-li)) (get-symm-helper (cdr li) (cdr rev-li) mid))
                  
      }
  )

(define (get-symm li)
  {
   if (< (length li) 2)
      li
      (get-symm-helper
       li
       (reverse-li li)
       ( / (length li ) 2))
  }
)

(equal? (get-symm (list 1 5 7 8)) (list (cons 1 8) (cons 5 7)))
(equal? (get-symm (list 1 5 7 8 9)) (list (cons 1 9) (cons 5 8) (cons 7 7)))

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
  {
   if (null? li)
      null
      (cons ((car li) x) (apply-f-list (cdr li) x) )
   }
  )

(equal? (apply-f-list
 (list (lambda (x) (+ x 1))
       (lambda (x) (* x 3))
       (lambda (x) x)
       (lambda (x) (- 5 x)))
 1
 ) (list 2 3 1 4))

;2.b
;Type: [List([T->T])*T -> T]
;Preconditions: No null elements other than the tail element
;Tests: (apply-fs-single (list
;                  (lambda (x) (* x 2))
;                  (lambda (x) (* x 10))
;                  (lambda (x) (+ x 1)))
;                 1) -> 21
(define (apply-fs-single f-li e)
{
 if (null? f-li)
    e
    (apply-fs-single (cdr f-li) ((car f-li) e))
 }
)

(equal? (apply-fs-single (list
                  (lambda (x) (* x 2))
                  (lambda (x) (* x 10))
                  (lambda (x) (+ x 1)))
                 1) 21)

;=============== Question 3 ===============;
;3.a
;Type: [Number*Number*Number -> Number]
;Preconditions: n is a natual number
;Tests: (dist_pwr 1 3 1) -> 2
(define (dist_pwr num1 num2 n)
  {
   let [
        (abs (lambda (x) (if (> x 0) x (* x -1))))
        (result (expt (expt (- num1 num2) n) (/ 1 n)))
        ]
    (if (= n 1) (abs result) result )
    }
  )

(equal? (dist_pwr 1 3 1) 2 )

;3.b
;Type: [List(Number)*Number*Number -> List(Number)]
;Preconditions: n is a natual number
;Tests: (compute_dists (list 1 2 3 4) 5 2) -> (4 3 2 1)
(define (compute_dists li num n)
  {
   if (null? li)
      null
      (cons (dist_pwr (car li) num n) (compute_dists (cdr li) num n))
   }
)
(equal? (compute_dists (list 1 2 3 4) 5 2) (list 4 3 2 1))

;3.c
;Type: [List(Number)*Number*Number -> List(Number)]
;Preconditions: n is a natual number
;Tests: (compute_dists_map (list 1 2 3 4) 5 2) -> (4 3 2 1)
(define (compute_dists_map li num n)
  {
   letrec ((repeat (lambda (x len) (if (<= len 0) null (cons x (repeat x (- len 1) ))))))
    {map dist_pwr li (repeat num (length li)) (repeat n (length li))}
   }
)
(equal? (compute_dists_map (list 1 2 3 4) 5 2) (list 4 3 2 1))

