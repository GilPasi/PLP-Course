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
  (
   (let

       ((res1 ((make-log-expt-v2 m n)x))
       (res2 ((make-log-expt m n)x)))

     (
        (display res1)
        (newline)
        (display res2)
        (newline)
        (display (= res1 res2))
        (newline)
        )
     )
   
   )
  )

(let*
    ((x 64)
     (m1 2) (n1 3) (res1_1  ((make-log-expt-v2 m1 n1)x)) (res1_2  ((make-log-expt m1 n1)x))
     (m2 4) (n2 5) (res2_1  ((make-log-expt-v2 m2 n2)x)) (res2_2  ((make-log-expt m2 n2)x))
     (m3 8) (n3 11) (res3_1  ((make-log-expt-v2 m3 n3)x)) (res3_2  ((make-log-expt m3 n3)x))

     )
  (verify-eq m1 n1 x)
)





