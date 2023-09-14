#lang eopl
(require racket/list)
;;down:
;; Proposito:
;; L -> L': procedimiento para retornar una lista con
;;cada elemento de L asociado a un nivel más de paréntesis comparado
;;con su estado original en L
;;
;;<down> := ()
;;        := ((lista)<down>) 

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
;;             := <int>

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
(newline)

;;mapping:
;;Propósito:
;;F x L1 x L2 -> '(a b): retorna una lista de pares (a,b)
;; donde a pertenece 'a' L1 y 'b' es elemento de L2
;;
;;<mapping> := ()
;;          := ((par-lista)<mapping>)
(define (mapping F L1 L2)
  (cond
    [(or (null? L2) (null? L1)) '()] 
    [(= (F (car L1)) (car L2))
     (cons (list (car L1) (car L2))
           (mapping F (cdr L1) (cdr L2)))]
    [else
     (mapping F (cdr L1) (cdr L2))]
    )
  )

;;Pruebas:
(display "Pruebas mapping")
(newline)
(display (mapping (lambda (d) (* d 4)) (list 5 3 2 3 0 3) (list 20 4 6 4 0 1)))
(newline)
(display (mapping (lambda (d) (+ d 4)) (list) (list)))
(newline)

;;zip:
;;Propósito:
;;F x L1 x L2 -> L1 x L2: retornar una lista donde la posición
;;n-ésima corresponde al resultado de aplicar la función F sobre el
;elemento en la posición n-ésima en L1 y L2
;;<zip> := ()
;;      :=((par-lista)<zip>)
(define (zip F L1 L2)
  (cond
    [(and (null? L1) (null? L2)) '()] 
    [else
     (cons (F (car L1) (car L2))
           (zip F (cdr L1) (cdr L2)))
     ]))


;;Pruebas
(display "Pruebas zip")
(newline)
(display (zip + '(1 4) '(6 2)))
(newline)
(display (zip * '(2 0 1) '(2 3 1)))
(newline)

;;prod-scalar-matriz
;;Propósito
;;mat x vect -> tupla : multipliza la matriz por el vector
;;
;;<prod-scalar-matriz> := ()
;;                     :=((tupla)<prod-scalar-matriz>)

(define (prod-scalar-matriz mat vec)
  (cond
    [(or (null? vec) (null? mat)) '()]
    [else
     (cons (list (* (car (car mat)) (car vec))
                 (* (cadr (car mat)) (cadr vec)))
           (prod-scalar-matriz (cdr mat) vec))]))

;;Pruebas
(display "Pruebas prod-scalar-matriz")
(newline)
(define matriz1 '((1 2)(3 4)))
(define vector1 '(2 5))
(display (prod-scalar-matriz matriz1 vector1))
(newline)
(display (prod-scalar-matriz '() '(1 4)))
(newline)
(display(prod-scalar-matriz '((5 3)) '(6 4)))

















