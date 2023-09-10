#lang eopl

; Juan Sebastian Cifuentes Vallejo - 202179800
; Maria Alejandra Carvajal Perez - 202178495
; Yissy Katherine Posso Perea - 202181910

;; 1)

;; invert:
;; Proposito:
;; L -> L’ : Procedimiento que recibe una lista L de n tuplas 
;; de dos elementos y devuelve una lista L' con los elementos
;; dentro de cada tupla invertidos.
;;
;; <lista> := ()
;;         := (<tupla> <lista>)
;;
;; <tupla> := (<tipo-de-dato> <tipo-de-dato>)
;;
;; <tipo-de-dato> := <int> | <simbolo> | <cadena> | <otro-tipo-de-dato>

(define invert
  (lambda (L)
    (cond
      [(null? L) L]
      [else (cons     (cons  (cadar L)  (cons (caar L) '()))     (invert (cdr L)))]
    )
  )
)

;; Pruebas
(display (invert '((a 1) (a 2) (1 b) (2 b))))
(newline)
(display (invert '((5 9) (10 91) (82 7) (a e) ("Hola" "Mundo"))))
(newline)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 4)

;; filter-in:
;; Proposito:
;; P x L -> L’ : Procedimiento que recibe un predicado P y una lista L  
;; y devuelve una lista L' con los elementos de L que cumplen P.
;;
;; <lista> := ({<expresion>}*)
;;
;; <expresion> := <lista> | <int> | <simbolo> | <cadena> | <otro-tipo-de-dato>

(define filter-in
  (lambda (P L)
    (cond
      [(not (null? L))
       (cond
         [(P (car L)) (cons (car L) (filter-in P (cdr L)))]
         [else (filter-in P (cdr L))]
       )]
      [else L]
    )
  )
)

;; Pruebas
(display (filter-in number? '(a 2 (1 3) b 7)))
(newline)
(display (filter-in symbol? '(a (b c) 17 foo)))
(newline)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 7)

;; cartesian-product-aux:
;; Proposito:
;; L1 x L2 x L3 -> L’ : Procedimiento que recibe tres listas, L1, L2 y L3,
;; y devuelve una lista L' con el producto cartesiano entre L1 y L2
;; siempre y cuando L2 sea igual a L3.
;;
;; <lista-de-datos> := ({<tipo-de-dato>}*)
;;
;; <lista-de-tuplas> := ({<tupla-de-datos>}*)
;;
;; <tupla-de-datos> := (<tipo-de-dato> <tipo-de-dato>)
;;
;; <tipo-de-dato> := <int> | <simbolo> | <otro-tipo-de-dato>

(define cartesian-product-aux
  (lambda (L1 L2 L3)
    (cond
      [(null? L1) L1]
      [(and (not (null? L1)) (not (null? L3))) (cons (cons (car L1) (cons (car L3) '())) (cartesian-product-aux L1 L2 (cdr L3)))]
      [(null? L3) (cartesian-product-aux (cdr L1) L2 L2)]
    )
  )
)

;; Pruebas
(display (cartesian-product-aux '(a b c) '(x y) '(x y)))
(newline)
(display (cartesian-product-aux '(p q r) '(5 6 7) '(5 6 7)))
(newline)

;; cartesian-product:
;; Proposito:
;; L1 x L2 -> L’ : Procedimiento que recibe dos listas, L1 y L2,
;; y devuelve una lista L' con el producto cartesiano entre L1 y L2.
;;
;; <lista-de-datos> := ({<tipo-de-dato>}*)
;;
;; <lista-de-tuplas> := ({<tupla-de-datos>}*)
;;
;; <tupla-de-datos> := (<tipo-de-dato> <tipo-de-dato>)
;;
;; <tipo-de-dato> := <int> | <simbolo> | <otro-tipo-de-dato>

(define cartesian-product
  (lambda (L1 L2)
    (cartesian-product-aux L1 L2 L2)
  )
)

;; Pruebas
(display (cartesian-product '(a b c) '(x y)))
(newline)
(display (cartesian-product '(p q r) '(5 6 7)))
(newline)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 10)

;; up:
;; Proposito:
;; L -> L’ : Procedimiento que recibe una lista L que contiene expresiones,
;; y devuelve una lista L' con las expresiones de L que no son listas y las
;; expresiones de las listas dentro de L.
;;
;; <lista> := ({<expresion>}*)
;;
;; <expresion> := <lista> | <int> | <simbolo> | <cadena> | <otro-tipo-de-dato>

(define up
  (lambda (L)
    (cond
      [(null? L) L]
      [else
        (cond
          [(not (list? (car L))) (cons (car L) (up (cdr L)))] ;[(or (symbol? (car L)) (number? (car L))) (cons (car L) (up (cdr L)))]
          [else
            (cond
              [(not (null? (car L))) (cons (caar L) (up (cons (cdar L) (cdr L))))]
              [else (up (cdr L))]
            )
          ]
        )
      ]
    )
  )
)

;; Pruebas
(display (up '((1 2) (3 4))))
(newline)
(display (up '((x (y)) z)))
(newline)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 13)

;; operate-aux:
;; Proposito:
;; lrators x lrands x result -> L’ : Procedimiento que recibe dos listas y un numero, 
;; lrators que contiene n funciones binarias, lrands que contiene n+1 numeros y result,
;; y devuelve el resultado de aplicar sucesivamente las operaciones en lrators a los
;; valores en lrands como si el primer valor de lrands fuera result.
;;
;; <lista-de-enteros> := ({int}*)
;;
;; <lista-de-operaciones-binarias> := ({<operaciones-binarias>}*)
;;
;; <operaciones-binarias> := + | - | * | / | <otras-operaciones-binarias>

(define operate-aux
  (lambda (lrators lrands result)
    (cond
      [(null? lrators) result]
      [else (operate-aux (cdr lrators) (cdr lrands) ((car lrators) result (car lrands)))]
    )
  )
)

;; Pruebas
(display (operate-aux (list * + - *) '(8 4 11 6) 3))
(newline)
(display (operate-aux (list ) '() 20))
(newline)

;; operate:
;; Proposito:
;; lrators x lrands -> L’ : Procedimiento que recibe dos listas, lrators que
;; contiene n funciones binarias y lrands que contiene n+1 numeros y devuelve
;; resultado de aplicar sucesivamente las operaciones en lrators a los valores en lrands.
;;
;; <lista-de-enteros> := ({int}*)
;;
;; <lista-de-operaciones-binarias> := ({<operaciones-binarias>}*)
;;
;; <operaciones-binarias> := + | - | * | / | <otras-operaciones-binarias>

(define operate
  (lambda (lrators lrands)
    (operate-aux (cdr lrators) (cddr lrands) ((car lrators) (car lrands) (cadr lrands)))
  )
)

;; Pruebas
(display (operate (list + * + - *) '(1 2 8 4 11 6)))
(newline)
(display (operate (list *) '(4 5)))
(newline)
(newline)

