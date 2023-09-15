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
          [(not (list? (car L))) (cons (car L) (up (cdr L)))]
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
;; lrators x lrands x result -> r : Procedimiento que recibe dos listas y un numero, 
;; lrators que contiene n funciones binarias, lrands que contiene n+1 numeros y result,
;; y devuelve el resultado r de aplicar sucesivamente las operaciones en lrators a los
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
;; lrators x lrands -> r : Procedimiento que recibe dos listas, lrators que
;; contiene n funciones binarias y lrands que contiene n+1 numeros y devuelve
;; resultado r de aplicar sucesivamente las operaciones en lrators a los valores en lrands.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 16)

;; Operar-binarias:
;; Proposito:
;; OperacionB -> r :Procedimiento que recibe una operacion binaria valida
;; y retorna el resultado de hacer las operaciones suma, resta y multiplicacion correspondientes.
;;
;; <OperacionB> ::= <int>
;;              ::= (<OperacionB> ’suma <OperacionB>)
;;              ::= (<OperacionB> ’resta <OperacionB>)
;;              ::= (<OperacionB> ’multiplica <OperacionB>)

(define Operar-binarias
  (lambda (operacionB)
    (cond
      [(number? operacionB) operacionB]
      [else
        (cond
          [(eqv? (cadr operacionB) (car '(suma))) (+ (car operacionB) (caddr operacionB))]
          [(eqv? (cadr operacionB) (car '(resta))) (- (car operacionB) (caddr operacionB))]
          [else (* (car operacionB) (caddr operacionB))]
        )
      ]
    )
  )
)

;; Pruebas
(display (Operar-binarias 4))
(newline)
(display (Operar-binarias '(2 suma 9)))
(newline)
(display (Operar-binarias '(2 resta 9)))
(newline)
(display (Operar-binarias '(2 multiplica 9)))
(newline)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 14)

;; path:
;; Proposito:
;; n x BST -> r : Procedimiento que recibe un numero n y un <árbol-binario> BST con ese numero
;; y devuelve el camino desde la raiz del arbol para llegar al numero en algun nodo.
;;
;; <árbol-binario> := (árbol-vacío) empty
;;                  := (nodo) número <árbol-binario> <árbol-binario>

(define path
  (lambda (n BST)
    (cond
      [(> (car BST) n) (cons 'left (path n (cadr BST)))]
      [(< (car BST) n) (cons 'rigth (path n (caddr BST)))]
      [else '()]
    )
  )
)

;; Pruebas
(display (path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ())))))
(newline)
(display (path 14 '(8 (4 (3 () ()) (5 () ())) (15 (13 (12 () ()) (14 () ())) (17 (16 () ()) (18 () ()))))))
(newline)
(display (path 14 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ())))))
(newline)
(display (path 8 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ())))))
(newline)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 15)

(define count-odd-and-even-aux
  (lambda (a b)
    (cons (+ (car a) (car b)) (cons (+ (cadr a) (cadr b)) '()))
  )
)

;; count-odd-and-even:
;; Proposito:
;; arbol -> r: Procedimiento que recibe un arbol "arbol" y devuelve
;; una tupla r de la forma (x y) donde x es el numero de pares y y el numero de impares.
;;
;; <árbol-binario> := (árbol-vacío) empty
;;                  := (nodo) número <árbol-binario> <árbol-binario>
;;
;;<tupla> := (<entero-no-negativo> <entero-no-negativo>)

(define count-odd-and-even
  (lambda (arbol)
    (cond
      [(null? arbol) '(0 0)]
      [else
        (cond
          [(odd? (car arbol)) (count-odd-and-even-aux '(0 1) (count-odd-and-even-aux (count-odd-and-even (cadr arbol)) (count-odd-and-even (caddr arbol))))]
          [else (count-odd-and-even-aux '(1 0) (count-odd-and-even-aux (count-odd-and-even (cadr arbol)) (count-odd-and-even (caddr arbol))))]
        )
      ]
    )
  )
)

;; Pruebas
(display (count-odd-and-even '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ())))))
(newline)
(display (count-odd-and-even '(8 (4 (3 () ()) (5 () ())) (15 (13 (12 () ()) (14 () ())) (17 (16 () ()) (18 () ()))))))
(newline)
(display (count-odd-and-even '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ())))))
(newline)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(display (pascal-aux-1 '(1 2 3 4)))
(newline)
(display (pascal-aux-2 '(1 2 3 4)))
(newline)
(display (pascal-aux-3 '(1 2 3 4) '(1 2 3 4)))
(newline)

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
(display (pascal 1))
(newline)
(display (pascal 2))
(newline)
(display (pascal 3))
(newline)
(display (pascal 4))
(newline)
(display (pascal 5))
(newline)
(newline)

