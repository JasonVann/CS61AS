;; Exercise 0 - Introduce yourself
;; Make a followup on the "Hello World!" post on Piazza introducing yourself.
I'm the king of pop.

;; Exercise 1 - Define sum-of-squares
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))


;; Exercise 2a - Define can-drive

(define (can-drive? age)
	(if(> age 16)
	'Good to Good
	'not yet)
)

;; Exercise 2b - Define fizzbuzz

(define (fizzbuzz num)
  (cond ((= (remainder num 15) 0) num)
	((= (remainder num 3) 0) 'fizz)
	((= (remainder num 5) 0) 'buzz)
	)
  )

;; Exercise 3 - Why did the Walrus cross the Serengeti?

#|
Your answer here


|#

;; Exercise 4 - new-if vs if

#|
Your answer here

|#
