;Programming Languages Principles - Assigment 1
; Authors: Gil Pasi (________) | Yulia Moshan (_________)


;=============== Question 1 ===============;

;1.a
;Type: [number -> number]
(define (cubicroot num) (expt num (/ 1 3)))

;1.b
;Type: [number*number*number -> number]
(define (3geomavg a b c) (cubicroot (* a b c)))

;1.c
;Type: [number*number*number -> number]
(define (numLarger a b c)
  (let

      (
       (count-a (if (> a (3geomavg a b c)) 1 0))
       (count-b (if (> b (3geomavg a b c)) 1 0))
       (count-c (if (> c (3geomavg a b c)) 1 0))
       )
    (+ count-a count-b count-c)
    )
 )

;=============== Question 2 ===============;
;2.a
;Type: [[T1 -> T2] * [T2 -> T3] -> [T1 -> T3]]
(define (compose2 f g) (lambda (x) (f (g x))))

;2.b
;Type: [number*number -> [number -> number]]
(define (logmul m n) (compose2 (lambda (x) (* n x)) (lambda (x) (log x m))))

;2.c
;Type: [numner*number -> [number -> number]]
(define (make-log-expt m n) (lambda (x) (log (expt x n) m)))

;2.d
;Type: [numner*number -> [number -> number]]
(define (make-log-expt-v2 m n ) (compose2  (lambda (x) (log x m)) (lambda (x) (expt x n))))

;2.e
;Type: [number -> number]
(define (log2 x) ((make-log-expt 2 1)x))

;2.f
;___BONUS___;

(define (verify-eq m n x)
   (let

       ((res1 ((make-log-expt-v2 m n)x))
       (res2 ((make-log-expt m n)x)))
     
        (display res1)
        (newline)
        (display res2)
        (newline)
        (display (= res1 res2))
        (newline)
     )
  )

(define (check_n cur)
  (if (< cur 1)
      (newline)
    (begin
      (verify-eq (expt 2 cur) (+ 1 (* 2 cur)) 64)
      (check_n (- cur 1))
      )
    )
  
  
  )
;___BONUS___;
(check_n 3)






