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
(cond
  ((and (> a (3geomavg a b c)) (> b (3geomavg a b c))) 2)
  ((and (> a (3geomavg a b c)) (> c (3geomavg a b c))) 2)
  ((and (> b (3geomavg a b c))(> c (3geomavg a b c))) 2)
  (else 1)
  ))

(numLarger 1 4 4)
