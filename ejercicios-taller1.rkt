

#lang eopl

; Juan Sebastian Cifuentes Vallejo - 202179800
; Maria Alejandra Carvajal Perez - 202178495
; Yissy Katherine Posso Perea - 202181910

;;3

;;list-set:
;;Proposito:
;;L x n x x -> 'L : Procedimiento que remueve el elemento en la 
;;posicion n de la lista L y lo cambia por el elemento x.
;;
;;<lista>::= ({<elemento>}*)
;;
;;<elemento>::=(<Símbolo> | <number> | <lista> | <booleano> | <cadena> | <caracter>)
;;

(define (list-set L n x)
  (cond
    [(or(null? L) (< n 0))'()]
    [(= n 0) (cons x(cdr L))]
    [else (cons (car L) (list-set (cdr L) (- n 1)x))]

  )
)

;; Pruebas

(list-set '(1 2 3 4 5) 4 99)
(list-set '(1 a 3 (b k) 5) 3 '(s (sw (s) sw)))
(list-set '(1 2 k #f d) 3 #t)

;;6

;;swapper:
;;Proposito:
;;E1 x E2 x L -> 'L : Procedimiento que recibe dos elementos (E1 Y E2)
;;y una lista L y devuelven la lista con los elementos E1 cambiados por
;;los elementos E2, y los E2, cambiados por E1.
;;
;;<lista>::= ({<elemento>}*)
;;
;;<elemento>::=(<Símbolo> | <number> | <lista> | <booleano> | <cadena> | <caracter>)
;;

(define (swapper E1 E2 L)
  (cond
    [(null? L) '()]
    [(equal? E1 (car L)) (cons E2 (swapper E1 E2 (cdr L)))]
    [(equal? E2 (car L)) (cons E1 (swapper E1 E2 (cdr L)))]
    [else (cons (car L) (swapper E1 E2 (cdr L)))]

  )
)

;; Pruebas
(swapper 1 'n '(v '(1 n) 2 d n 3 1))
(swapper '() 'abc '(a b c k abc 32 () ))
(swapper #f #f '(n 2 3 #f a #f v))
(swapper #f #t '(#t #f #t a b n #t #f #t))


;;9

;;inversions:
;;Proposito:
;;L -> n : Procedimiento que recibe una lista L y devuelve
;;el numero de inversiones de la lista.
;;
;;<lista-de-numeros>::= <number> | <number> <lista-de-numeros>
;;<number>::= 0 | <numero-no-nulo>
;;<numero-no-nulo>::= <digito-no-nulo> <digitos restantes>
;;<digitos restantes>::= ε | <digito> <digitos restantes>
;;<digito>:= 0 | <digito-no-nulo>
;;<digito-no-nulo>::= 1|2|3|4|5|6|7|8|9|

(define (aux l1 l2 l3 acum)
  (cond
    ((null? l1) acum)
    (else
      (cond
        ((= (car l2) (car l1))
         (aux (cdr l1) l2 l3 acum))
        (else
         (cond
           ((> (car l2) (car l1))
            (aux l1 (cdr l2) l3 (+ acum 1)))
           (else
            (aux l1 (cdr l2) l3 acum))))))))


(define (inversions L)
  (aux L L L 0)
 )
;; pruebas
 (inversions '(2 4 8 9 3 1))
 (inversions '(2 7 8 3 9))
 (inversions '(2 4 5 7 3 1))


;;12

;;filter-acum:

;;Proposito:
;;a x b x F x acum x filter -> acum : Procedimiento que recibe
;; un intervalo [a b], un operador binario F, un acumulador y
;; un predicado filter y devuelve la funcion F aplicada a todos los
;; elementos que cumplen con el predicado.
;;
;; <aplicacion> ::= (<rango> <OpBinario> <int> <predicado>)
;; <rango> ::= (<number><number>)
;; <OpBinario>::= (+ | - | * | /)
;; <predicado>::= ( odd? | even? | integer? | equal? )
;;
;; 

(define (filter-acum a b F acum filter)
    (cond
      [(> a b) acum]
      [(filter a)
       (filter-acum (+ a 1)b F (F acum a)filter)
       ]
      [else
       (filter-acum (+ a 1)b F acum filter)
       ]
      )
)


;; pruebas
(filter-acum 1 5 * 1 even?)
(filter-acum 1 6 * 1 even?)
(filter-acum 1 20 + 0 odd?)



;; 18)

;; pascal-aux-1:
;; Proposito:
;; l -> 'l : Recibe una lista l y le agrega un cero al inicio de la lista.
;;
;; <lista-de-enteros> := (<0>{int}*)

(define pascal-aux-1
  (lambda (l)
    (cons 0 l)
  )
)

;; pascal-aux-2:
;; Proposito:
;; l -> 'l : Recibe una lista l y agrega la cabeza de la lista
;; al inicio de la lista resultado (toma una lista y la invierte)
;;
;; <lista-de-enteros> := ({int}*)

(define pascal-aux-2
  (lambda (l)
    (cond
      [(null? l) '(0)]
      [else (cons (car l) (pascal-aux-2 (cdr l)))]
    )
  )
)

;;pascal-aux-3:
;; Proposito:
;; l1 l2 -> 'l : Recibe dos listas l1 y l2 y retorna una lista l
;; donde cada elementi es la suma de los elementos correspondientes
;; a la misma posicion en las listas l1 y l2
;;
;; <lista-de-enteros> := ({int}*)

(define pascal-aux-3
  (lambda (l1 l2)
    (cond
      [(null? l1) l1]
      [else (cons (+ (car l1) (car l2)) (pascal-aux-3 (cdr l1) (cdr l2)))]
    )
  )
)
;;pascal-aux-4:
;; Proposito:
;; N l -> 'l : Recibe un numero N y una lista l y calcula la fila N del
;; triángulo de pascal y devuelve una lista que representa esta fila.
;;
;; <lista-de-enteros> := ({int}*)

(define pascal-aux-4
  (lambda (N l)
    (cond
      [(eqv? N 0) l]
      [else (pascal-aux-4 (- N 1) (pascal-aux-3 (pascal-aux-2 l) (pascal-aux-1 l)))]
    )
  )
)

 (pascal-aux-1 '(1 2 3 4))
(pascal-aux-2 '(1 2 3 4))
(pascal-aux-3 '(1 2 3 4) '(1 2 3 4))

;; pascal:
;; Proposito:
;; N -> f : recibe un numero N y devuelve la fila N del triangulo de pascal
;;
;; <lista-de-enteros> := ({int}*)

(define pascal
  (lambda (N)
    (pascal-aux-4 (- N 1) '(1))
  )
)

;; Pruebas
(pascal 1)
(pascal 2)
(pascal 3)
(pascal 4)
(pascal 5)
