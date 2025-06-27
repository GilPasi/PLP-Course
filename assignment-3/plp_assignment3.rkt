#lang racket
(require "adt.rkt")
;Programming Languages Principles - Assigment 3
; Authors: Gil Pasi ($ID1$) | Yulia Moshan ($ID2$)
;=============== Question 2 ===============;
;2.a
;Type: [T1*T2*T3 -> [Symbol -> T1 U T2 U T3 U Symbol]]
;Preconditios: None
(define (make-triple a b c)
  (lambda (op)
    (cond
        ((eq? op 'get_first) a)
        ((eq? op 'get_second) b)
        ((eq? op 'get_third) c)
        (else 'operation-not-found-error)
        )
    )
  )

;Type: [[Symbol -> T | Symbol] -> T]
;Preconditions: Triplet procedure accepts the "get_first" symbol as a legitimate operation
(define (get1 triplet)(triplet 'get_first ))
;Type: [[Symbol -> T | Symbol] -> T]
;Preconditions: Triplet procedure accepts the "get_second" symbol as a legitimate operation
(define (get2 triplet)(triplet 'get_second ))
;Type: [[Symbol -> T | Symbol] -> T]
;Preconditions: Triplet procedure accepts the "get_third" symbol as a legitimate operation
(define (get3 triplet)(triplet 'get_third ))
;Type: [T -> Boolean]
;Preconditions: Triplet procedure is logically correct and not just any procedure with the same type
(define triple? procedure?)


;2.b
(define (new_cons a b) (make-triple 'new-pair a b))
(define new_car get2)
(define new_cdr get3)

;2.c
;Type: [Number*Number -> TaggedData(NewCons(Number*Number))]
;Preconditions: None
(define (make-point x y) (attach-tag (new_cons x y) 'point))
;Type: [TaggedData(NewCons(Number*Number)) -> Number]
;Preconditions: None
(define (get-x p) (new_car ( get-content p)))
;Type: [TaggedData(NewCons(Number*Number)) -> Number]
;Preconditions: None
(define (get-y p) (new_cdr ( get-content p)))
;Type: [T -> Boolean]
;Preconditions: None
(define (point? p)(tagged-by? p 'point))


;2.d
;Type: [Point*Point -> TaggedData(Pair(Point*Number))]
;Preconditions: p1.x - p2.x != 0
(define (make-line p1 p2)
  (attach-tag (cons p1(/
   (- (get-x p1)(get-x  p2))
   (- (get-y p1)(get-y p2))
   )))
  )
;Type: [Point*Number -> TaggedData(Pair(Point*Number))]
;Preconditions: None
(define (make-line2 p1 slope)(attach-tag (cons p1 slope) 'line))
;Type: [TaggedData(Pair(Point)) -> Point]
;Preconditions: None
(define (get-p1 ln) (car (get-content ln)))
;Type: [TaggedData(Pair(Point*Number)) -> Number]
;This selector is eager since the value of the slope is calculated on construction
;as soon as there are enough parameters to determine it.
(define (get-slope ln) (cdr (get-content ln)))
;Type: [T -> Boolean]
;Preconditions: None:
(define (line? obj) (tagged-by? obj 'line))
;2.e
;Type [List(Pair U Triple U Point U Line)*T ->[List(Pair U Triple U Point U Line]
;Preconditions:None
(define (only-with-x li x)
  (filter
          (lambda (elm)
            (cond
              ((line? elm)(or (eq? x (get-x (get-p1 elm))) (eq? x (get-y (get-p1 elm)))))
              ((point? elm) (or (eq? x (get-x elm)) (eq? x (get-y elm))))
              ((triple? elm) (or (eq? x (get1 elm)) (eq? x (get2 elm)) (eq? x (get3 elm))))
              ((pair? elm) (or (eq? x (car elm)) (eq? x (cdr elm))))
              (else #f)
              )
            )
          li
          )
  )
