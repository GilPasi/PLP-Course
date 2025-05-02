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
;Preconditions:
