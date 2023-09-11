#lang eopl

;;down:
;; Proposito:
;; L -> L': procedimiento para retornar una lista con
;;cada elemento de L asociado a un nivel más de paréntesis comparado
;;con su estado original en L
;;
;;<down> := ()
;;        := ((lista)(down)) 

(define (down L)
  (cond
    [(null? L) '()]
    [else
      (cons(list(car L)) (down(cdr L)))]
    ))

;;Pruebas
(display "Pruebas down")
(newline)
(display (down '()))
(newline)
(display (down '(a (2 3) ((a)) 7)))
(newline)


;;list-index:
;;Propósito:
;;P x L -> false | Z+: devolver la posición del primer elemento que
;;satisface el predicado
;;
;;<list-index> := false
;;             := Z+

(define list-index
  (lambda (P L)
    (define (aux P L position)
      (cond
      [(null? L) #f]
      [(P (car L)) position]
      [else (aux P (cdr L) (+ position 1))]
      )
     )
   (aux P L 0)
    
   )
)

;;Pruebas
(display "Pruebas list-index")
(newline)
(display (list-index number? '()))
(newline)
(display (list-index symbol? '(#t (a) b 4 2 1 m s !)))

;;mapping:
;;Propósito:
;;F x L1 x L2 -> '(l1 l2)
(define (mapping F L1 L2)
  (define (aux L1 L2 result)
    (cond
      [(and (null? L2) (null? L1)) '()]
      [(= (F (car L1)) (car L2))
       (aux (cdr L1) (cdr L2) (cons (list (car L1) (car L2)) result))]
      [else (aux (cdr L1) (cdr L2) result)]
      ))
  (aux L1 L2 '())
)

;zip
(define (zip F L1 L2)
  (define (aux L1 L2 result)
    (cond
      ((and (null? L1) (null? L2)) '())
      (else
       (aux (cdr L1) (cdr L2) (cons (F (car L1) (car L2)) result)))
      )
    )
  
  (aux L1 L2 '())
)











