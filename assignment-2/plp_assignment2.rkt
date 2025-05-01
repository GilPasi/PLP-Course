;Programming Languages Principles - Assigment 2
; Authors: Gil Pasi ($ID1$) | Yulia Moshan ($ID2$)


;=============== Question 1 ===============;
;1.a
;Type: [List(T) -> List(T)]
;Preconditions: No null elements other than the tail element
(define (make-dup-list li)  
   (if (null? li)
       li
       (let ((head (car li)) (rest (cdr li)))
        (cons
        head
        (cons head
              (make-dup-list rest)
        )
      )
    )
  )
)


