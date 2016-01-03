; Exercise 1 - Define dupls-removed

(define (dupls-removed sent)
    (if (= (length sent) 1) 
            sent
            (if (member? (first sent) (bf sent)) (se (dupls-removed (bf sent)))
                (se (first sent) (dupls-removed (bf sent))))) 

)

; Exercise 2 - Define count-word


(define (count-word-n n sent wd)
    (if (not (member? wd sent)) n
        (if (equal? wd (first sent))
                (count-word-n (+ n 1) (bf sent) wd)
                (count-word-n  n (bf sent)  wd) 

)))

(define (count-word sent wd)
        (count-word-n 0 sent wd))

; Exercise 3

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

|#

; Exercise 4 - Define squares

(define (squares sent)
    (if (empty? sent) '()
        (se (square (first sent)) (squares (bf sent))))

)

; Exercise 5 - Define switch

(define (switch sent)
    (if (equal? (first sent) 'you) (se 'I (switch-helper (bf sent)))
        (switch-helper sent))

)

(define (switch-helper sent)
    (cond ((empty? sent) '())
         ((equal? 'you (first sent)) (se 'me (switch-helper (bf sent))))
        ((equal? 'me (first sent)) (se 'you (switch-helper (bf sent))))
        ((equal? 'i (first sent)) (se 'you (switch-helper (bf sent))))
        (else (se (first sent) (switch-helper (bf sent))))
))
; Exercise 6 - Define ordered?

(define (ordered? sent)
    (cond ((= 1 (length sent)) #t)
        (else (ordered-helper (first sent) (bf sent))))

)

(define (ordered-helper a  sent)
    (if (> a (first sent)) #f
        (if (= 1 (length sent)) #t
        (ordered-helper (first sent) (bf sent))
)))

; Exercise 7 - Define ends-e

(define (ends-e sent)
    (cond ((empty? sent) '())
         ((equal? 'E (last (first sent))) (se (first sent) (ends-e (bf sent))))
        (else (ends-e (bf sent))))
)

; Exercise 8

; special form, evaluate one at a time
or (= 0 0) (= 1 (/ 1 0)) )

