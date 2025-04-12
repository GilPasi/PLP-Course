;Programming Languages Principles - Assigment 1
; Authors: Gil Pasi ($ID1$) | Yulia Moshan ($ID2$)
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
;Type: [number*number*number -> void]
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

;Type: [number -> void]
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



;=============== Question 3 ===============;
;3.a
;Type: [number*number -> [number -> number]]
;Preconditions: a != 0
(define (make_times_n n) (lambda (x) (* x n)))
(define (square x) (* x x))
(define (make-parabola a b c)
  (lambda (x) (+
               ((make_times_n a) (square x))
               ((make_times_n b) x)
               c
               )))

;3.b
;Type: [number*number*number -> [number -> number]]
(define (make-triple a b c)(lambda (x) (* (- x a) (- x b) (- x c))))

;3.c
;Type: [number*number*number*number -> [number*number*number -> number]]
;Tests:
;     ((make_triple_or_parabola_N 8 2 7 5)5);  -> 217 Since 217 > 18
(define (make_triple_or_parabola_N a b c N)
  (let (
        (parabola (make-parabola a b c)) (triple (make-triple a b c)))
    (if (> (parabola N) (triple N)) parabola triple)
    )
  )


;3.d
;Type: [number*number*number*number -> number]
;Tests:
;     (make-triple-parabola 1 2 0 0) -> 1 Since 1*0^2 + 2*0 + 0 = 0 = (0 - 1)*(0 - 2)*(0 - 0) 
;     (make-triple-parabola 1 2 5 0) -> 5 Since 1*0^2 + 2*0 + 5 >  (0 - 1)*(0 - 2)*(0 - 5)
(define (make-triple-parabola a b c x)
  (let ((parabola-val ((make-parabola a b c)x))(triple-val ((make-triple a b c)x)))
    (if (= parabola-val 0)
        (if (= triple-val 0)
            (if (= x 0) 1 x)
            triple-val)
        parabola-val))
  )



