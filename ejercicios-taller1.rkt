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