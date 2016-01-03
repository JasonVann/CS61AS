#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom

;2. Compound Expression (3 Atoms)

;3. Compound Expression (4 Atoms)

;4. Compound Expression (1 Atom and 2 subexpressions)

;5. Any Other Kind Expression


;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  ; your code here
  (word (first wd) (first (bf wd)))
)

;;2. Define two-first
(define (two-first x y)
  ; your code here
  (word (first x) (first y))
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  ; your code here
  (word (first (first sent)) (first (first (bf sent))))
)

(first-two 'ambulatory)
(two-first 'brian 'epstein)
(two-first-sent '(brian epstein))

;Exercise 2 - Define teen?
(define (teen? num)
  ; your code here
  (if (and (<= num 19) (>= num 13))
      #t
      #f)
)

(teen? 19)
(teen? (/ 39 2))

;Exercise 3 - Define indef-article
(define (indef-article wd)
  ; your code here
  (if (member? (first wd) 'aeiou)
         (se 'an wd)
         (se 'a wd))
)

(indef-article 'beetle)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  ; your code here
  (se (bl sent) 'and (last sent))
)

(insert-and '(john bill wayne fred joey))

;Exercise 5 - Define query
(define (query sent)
  ; your code here
  (se (first (bf sent)) (first sent) (bl (bf (bf sent))) (word (last sent) '?))
)

(query '(you are experienced))

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  ; your code here
  (cond ((equal? time `(12 am)) 0)
        ((equal? time '(12 pm)) 12)
        ((equal? (last time) 'am) (first time))
        ((equal? (last time) 'pm) (+ (first time) 12))
        (else (error 'impossible)))
)

(european-time '(8 am))
(european-time '(4 pm))
(european-time '(12 pm))
(european-time '(12 am))

(define (american-time time)
  ; your code here
  (cond ((= time 12) '(12 pm))
        ((> time 12) (se (- time 12) 'pm))
        ((= time 0) '(12 am))
        (else (se time 'am)))
)

(american-time 21)
(american-time 12)

;Exercise 7 - Define describe-time
(define (describe-time secs)
  ; your code here
  (cond ((< secs 60) (se secs 'seconds))
        ((< secs 3600) (se (/ secs 60.0) 'minutes))
        ((< secs (* 24 3600)) (se (/ secs 3600.0) 'hours))
        (else (se (/ secs 86400.0) 'days)))
)

(describe-time 45)
(describe-time 930)

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective word1)
  (se (word adjective 'est) word1))

(superlative 'dumb 'exercise)

#|

Explanation here.

|#