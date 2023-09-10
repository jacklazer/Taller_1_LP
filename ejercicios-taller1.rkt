#lang eopl
(define (down L)
  (cond
    [(null? L) '()]
    [else
      (cons(list(car L)) (down(cdr L)))]
    ))
