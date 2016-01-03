#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed

(define (dupls-removed sent)
  ; Your code here
  (if (empty? sent) '()
      (if (member? (first sent) (bf sent)) (dupls-removed (bf sent))
          (se (first sent) (dupls-removed (bf sent)))))
)

(dupls-removed '(a a a a b a a))

; Exercise 2 - Define count-word

(define (count-word sent wd)
  ; Your code here
  (if (empty? sent) 0
      (+ (if (equal? (first sent) wd) 1 0) (count-word (bf sent) wd)))
)

(count-word '(i really really like 61as) 'really)

; Exercise 3

(define (new-if test then-case else-case)
  (if test
    then-case
    else-case))

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Your explanation here
Program not terminate because all parameters to new-if are evaluated fully

|#


; Exercise 4 - Define squares

(define (squares sent)
  ; Your code here
  (define (square num)
    (* num num))
  (if (empty? sent) '()
      (se (square (first sent)) (squares (bf sent))))
  
)

(squares '(1 2 3))

; Exercise 5 - Define switch

(define (switch sent)
  ; Your code here
  (define (switch-helper sent)
    (if (empty? sent) '()
        (cond ((member? (first sent) '(me I)) (se 'you (switch-helper (bf sent))))
              ((equal? (first sent) 'you) (se 'me (switch-helper (bf sent))))
              (else (se (first sent) (switch-helper (bf sent))))
              )))
  (if (empty? sent) '()
      (if (equal? (first sent) 'you) (se 'I (switch-helper (bf sent)))
          (switch-helper sent)))
)


(switch '(you told me that I should wake you up))

; Exercise 6 - Define ordered?

(define (ordered? sent)
  ; Your code here
  (cond ((empty? sent) #t)
        ((and (= (count sent) 1) (number? (first sent))))
        (else (and (number? (first sent)) (< (first sent) (first (bf sent))) (ordered? (bf sent)))) 
))

(ordered? '(1 2 3)) ; #t

; Exercise 7 - Define ends-e

(define (ends-e sent)
  ; Your code here
  (if (empty? sent) '()
      (if (equal? (last (first sent)) 'e) (se (first sent) (ends-e (bf sent)))
          (ends-e (bf sent))))
)

(ends-e '(please put the salami above the blue elephant))

; Exercise 8

(or (= 0 0) (/ 1 0))
(and (= 1 0) (/ 1 0))
#|

Your explanation here
They are special forms so that can perform shortcuts evaluation
|#
