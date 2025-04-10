;Programming Languages Principles - Assigment 1
; Authors: Gil Pasi (________) | Yulia Moshan (_________)

;1.a
;Type: [number -> number]
(define (cubicroot num) (expt num (/ 1 3)))

;1.b
;Type: [number*number*number -> number]
(define (3geomavg a b c) (cubicroot (* a b c)))

;1.c
;Type: []
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

(numLarger 1 4 4)
