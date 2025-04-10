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
(define (logmul m n) (compose2 (lambda (x) (* n x)) (lambda (x) (log x m))))

((logmul 2 3) 16)
