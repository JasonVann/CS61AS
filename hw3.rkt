#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define fast-expt-iter


(define (fast-expt-iter b n)
  ; Your code here
  (define (iter i result)
    (cond
      ((= n 0) 1)
          ((= i n) result)
          ((and (even? i) (< (* 2 i) n)) (iter (* i 2) (* result result))
                         )
          (else (iter (+ i 1) (* b result)))))
  (iter 1 b)
)

#|
(define (fast-expt-iter b n)
(error "Not yet implemented")
)
|#

; (fast-expt-iter 10 6)

; Exericse 2 - Define phi

(define tolerance 0.00001)

(define (average a b)
    (/ (+ a b) 2))

(define (phi)
    (fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Exercise 3 - Define cont-frac

;; Recursive version
(define (cont-frac n d k)
    (define (iter i)
        (if (= i k)
            (/ (n i) (d i))
            (/ (n i) (+ (d i) (iter (+ i 1))))
        ))
    (iter 1))

;; Iterative version
(define (cont-frac-iter n d k)
  (define (iter i  result)
    (if (= i 1) result
          (iter (- i 1) (/ (n (- i 1)) (+ (d (- i 1)) result)))
    )
  )
  (iter k (/ (n k) (d k)))
)

(define (e k)
    (define (N i)
        1)

    (define (D i)
        (if (= 0 (remainder (+ i 1) 3))
            (* 2 (/ (+ i 1) 3))
            1))

    (+ 2.0 
       (cont-frac N D k)))

; Exercise 4 - Define next-perf

(define (next-perf n)
  ;  Your code here
  (define (iter n)
  (cond ((= (sum-of-factors n) n) n
              )
         (else (iter (+ n 1)))))
  (iter (+ n 1))
)

(define (find-factors i num)
  (if (= i num) '()
      (if (= 0 (remainder num i)) (se i (find-factors (+ i 1) num))
          (find-factors (+ i 1) num))))

(define (sum-of-factors num)
  (sum-of-factors-helper (find-factors 1 num))
)

(define (sum-of-factors-helper nums)
  (if (empty? nums) 0
      (+ (first nums) (sum-of-factors-helper (bf nums)))))

; Exercise 5 - Explain what happens when the base cases are interchanged.

#|

There's no difference

|#

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

#|

Formula for expt:

Formula for expt-iter: b^(n - counter) = product

|#
