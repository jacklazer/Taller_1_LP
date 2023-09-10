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
;; <tipo-de-dato> := <int> | <simbolo> | <cadena> | <otro>

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


;; 4)
;; filter-in:
;; Proposito:
;; P x L -> L’ : Procedimiento que recibe un predicado P y una lista L  
;; y devuelve una lista L' con los elementos de L que cumplen P.
;;
;; <lista> := ()
;;         := (<lista-o-tipo-de-dato> <lista>)
;;
;; <lista-o-tipo-de-dato> := <lista> | <tipo-de-dato>
;;
;; <tipo-de-dato> := <int> | <simbolo> | <cadena> | <otro>

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