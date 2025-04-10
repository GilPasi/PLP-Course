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
(define (compose2 f g) (lambda (x) (f (g x))))

((compose2 (lambda (x) (+ 1 x)) (lambda (x) (* 2 x)))6)
