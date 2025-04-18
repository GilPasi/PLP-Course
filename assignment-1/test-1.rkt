;=============== Tests 1 ===============;


(define (test-cubicroot)
  (display "cubicroot tests:")
  (newline)
  (display (cubicroot 8)) ; should be 2
  (newline)
  (display (cubicroot 27)) ; should be 3
  (newline)
  (display (cubicroot 1)) ; should be 1
  (newline)
)

(define (test-3geomavg)
  (display "3geomavg tests:")
  (newline)
  (display (3geomavg 1 1 1)) ; should be 1
  (newline)
  (display (3geomavg 2 4 8)) ; should be 4
  (newline)
)

(define (test-numLarger)
  (display "numLarger tests:")
  (newline)
  (display (numLarger 1 2 3)) ; avg ≈ 1.817, two are larger
  (newline)
  (display (numLarger 4 4 4)) ; 0 or 3, dependent if the average 4 or 3.9999999999999996
  (newline)
  (display (numLarger 0 0 8)) ; one is larger
  (newline)
)


;(test-cubicroot)
;(test-3geomavg)
;(test-numLarger)

;=============== Tests 2 ===============;

(define (add1 x) (+ 1 x))

(define (test-compose2)
  (display "compose2 test:")
  (newline)
  (display ((compose2 add1 square) 3)) ; square(3) = 9, add1(9) = 10
  (newline)
)

(define (test-logmul)
  (display "logmul test:")
  (newline)
  (display ((logmul 2 3) 8)) ; log base 2 of 8 = 3, * 3 = 9
  (newline)
)

(define (test-log2)
  (display "log2 test:")
  (newline)
  (display (log2 8)) ; should be 3
  (newline)
)

;(test-compose2)
;(test-logmul)
;(test-log2)
;=============== Tests 3 ===============;

(define (test-make_parabola)
  (display "parabola test:")
  (newline)
  (display ((make_parabola 1 2 1) 1)) ; 1*1^2 + 2*1 + 1 = 4
  (newline)
)

(define (test-make_triple)
  (display "triple test:")
  (newline)
  (display ((make_triple 1 2 3) 4)) ; (4-1)*(4-2)*(4-3) = 3*2*1 = 6
  (newline)
)

(define (test-make_triple_or_parabola_N)
  (display "triple or parabola selector test:")
  (newline)
  (display ((make_triple_or_parabola_N 8 2 7 5)5)) ;217 Since 217 > 18
)

(define (test-make_triple_parabola)
  (display "triple_parabola decision test:")
  (newline)
  (display (make_triple_parabola 1 2 0 0)) ; should be 1
  (newline)
  (display (make_triple_parabola 1 2 5 0)) ; should be 5
  (newline)
)

(test-make_parabola)
(test-make_triple)
(test-make_triple_or_parabola_N)
(test-make_triple_parabola)