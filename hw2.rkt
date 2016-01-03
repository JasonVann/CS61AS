#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define substitute

(define (substitute sent old-word new-word)
  ; Your code here
  (if (empty? sent) '()
      (if (equal? old-word (first sent)) (se new-word (substitute (bf sent) old-word new-word))
          (se (first sent) (substitute (bf sent) old-word new-word)))
))

(substitute '(she loves you yeah yeah yeah) 'yeah 'maybe)


; Exercise 2 - Try out the expressions!

#|
(lambda (x) (+ x 3))
-> returns:

((lambda (x) (+ x 3)) 7)
-> returns:

(define (make-adder num)
  (lambda (x) (+ x num))) 
((make-adder 3) 7)
-> returns:

(define plus3 (make-adder 3)) 
(plus3 7)
-> returns:

(define (square x) (* x x)) 
(square 5)
-> returns:

(define square (lambda (x) (* x x))) 
(square 5)
-> returns

(define (try f) (f 3 5)) 
(try +)
-> returns: 8

(try word)
-> returns: 35
|#


; Exercise 3
#|

Number of arguments g has: 1

Type of value returned by g: number

|#

; Exercise 4 - Define f1, f2, f3, f4, and f5

(define f1 3)
(define (f2) 3)
(define (f3 a) (* a a))
(define (f4) (lambda () (+ 1 5)))
(define (f5) (lambda () (lambda (n) (+ 1 n))))

; Exercise 5 - Try out the expressions


(define (t f) 
  (lambda (x) (f (f (f x)))) )

#|
1. ((t add1) 0) returns: 3

2. ((t (t add1)) 0) returns: 9

3. (((t t) add1) 0) returns: 27

|#

; Exercise 6 - Try out the expressions

(define (s x)
  (+ 1 x))

#|

1. ((t s) 0) returns: 3

2. ((t (t s)) 0) returns: 9

3. (((t t) s) 0) returns: 27

|#

; Exercise 7 - Define make-tester

(define (make-tester wd)
  ; Your code here
  (lambda (wd2) (equal? wd2 wd))
)

((make-tester 'hal) 'cs61a)

; Exercise 8 - SICP exercises

; SICP 1.31a

(define (product term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b)))
)

(define (product-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result))))
    (iter a 1))

(define (factorial n)
    (product-iter * 1 (lambda (x) (+ x 1)) n)
)
  
(define (estimate-pi)
  ; Your code here
  (* 4.0 (product (lambda (n) (/ (* n (+ n 2)) (* (+ 1 n) (+ 1 n)))) 2 (lambda (n) (+ n 2)) 10000))
)

; SICP 1.32a

;; This is called my-accumulate so it doesn't conflict with Simply
;; Scheme's accumulate.
(define (my-accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (my-accumulate combiner null-value term (next a) next b)))
)

;; Write sum in terms of my-accumulate:
(define (sum-accum term a next b)
  ; Your code here
  (my-accumulate + 0 term a next b)
)

;; Write product in terms of my-accumulate:
(define (product-accum term a next b)
  ; Your code here
  (my-accumulate * 1 term a next b)
)



; SICP 1.33

(define (filtered-accumulate combiner null-value term a next b pred)
    (if (> a b)
        null-value
        (combiner (if (pred a) (term a) null-value) (filtered-accumulate combiner null-value term (next a) next b pred)))
)

(define (prime? n)
    (define (iter i)
        (if (= i n) #t
            (and (not (= (modulo n i) 0)) (iter (+ i 1)))))
    (iter 2)
)

(define (sum-sq-prime a b)
    (filtered-accumulate + 0 (lambda (x) (* x x)) a (lambda (x) (+ x 1)) b prime?)
)

(define (rel-prime? x y)
  (= (gcd x y) 1))

(define (prod-of-some-numbers n)
  (define (rel-prime2? x)
    (rel-prime? n x))
    (filtered-accumulate * 1 * 1 (lambda (x) (+ x 1)) n rel-prime2?))

; SICP 1.40 - Define cubic

(define (cubic a b c)
  ; Your code here
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c))
)

; SICP 1.41 - Define double

(define (double proc)
  ; Your code here
  (lambda (x) (proc (proc x)))
)

; SICP 1.43 - Define repeated

; (define (compose f g)
;  (lambda (x) (f (g x))))

(define (my-repeated f n)
  (cond ((= n 1) (lambda (x) (f x)))
        ((= n 2) (compose f f))
        (else (compose f (my-repeated f (- n 1))))))


; Exercise 9 - Define my-every

(define (my-every proc sent)
  ; Your code here
  (if (empty? sent) '()
      (se (proc (first sent)) (my-every proc (bf sent))))
)


; Exercise 10 - Try out the expressions

#|

(every (lambda (letter) (word letter letter)) 'purple)
-> returns:

(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
-> returns:

(keep even? '(781 5 76 909 24))
-> returns:

(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
-> returns:

(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
-> returns:

(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
-> returns:

(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))
-> returns:
|#
