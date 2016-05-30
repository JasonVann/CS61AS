#lang racket
(require berkeley)
(provide (all-defined-out))

(+ 3 4)
(define (square x) (* x x))
(square 3)

(define (sum-of-squares x y)
  (+ (square x) (square y))
  )

(sum-of-squares 3 4)

(define x 3)
(define y 4)
(define z (list '+ x y))
