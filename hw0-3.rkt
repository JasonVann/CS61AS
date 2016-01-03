#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define describe-time
(define (describe-time secs)
  ; your code here
  (cond ((= secs 1) (se secs 'SECOND))
        ((< secs 60) (se secs 'SECONDS))
        ((< secs 120) (se (quotient secs 60) 'MINUTE (describe-time (modulo secs 60))))
        ((< secs 3600) (se (quotient secs 60) 'MINUTES (describe-time (modulo secs 60))))
        ((< secs 7200) (se (quotient secs 60) 'HOUR (describe-time (modulo secs 3600))))
        ((< secs 86400) (se (quotient secs 3600) 'HOURS (describe-time (modulo secs 3600))))
        ((< secs 172800) (se (quotient secs 86400) 'DAY (describe-time (modulo secs 86400))))
        (else (se (quotient secs 86400) 'DAYS (describe-time (modulo secs 86400))))
))

(describe-time 22222)
(describe-time 550441)
(describe-time 61)
(describe-time 3661)
(describe-time (- 172800 1))

; Exercise 2 - Define remove-once
(define (remove-once wd sent)
  ; your code here
  (cond ((empty? sent) '())
        ((equal? wd (first sent)) (bf sent))
        (else (se (first sent) (remove-once wd (bf sent)))))
)

; (trace remove-once)
(remove-once 'morning '(good morning good morning morning good))

(define (remove-all wd sent)
  ; your code here
  (cond ((empty? sent) '())
        ((equal? wd (first sent)) (remove-all wd (bf sent)))
        (else (se (first sent) (remove-all wd (bf sent)))))
)

(remove-all 'morning '(good morning good morning morning good))

; Exercise 3 - Define differences
(define (differences nums)
  ;your code here
  (cond ((< (count nums) 2) '())
        (else (se (- (first (bf nums)) (first nums)) (differences (bf nums))))
))

(differences '(4 23 9 87 6 12))
(differences '(4 23 9))
             
; Exercise 4 - Define location
(define (location small big)
  ; your code here
  (if (not (member? small big)) #f
      (if (equal? small (first big)) 1
          (+ 1 (location small (bf big))))
))

(location 'me '(you never give me your money))
(location 'i '(you never give me your money))
(location 'the '(the fork and the spoon))

; Exercise 5 - Define initials
(define (initials sent)
  ; your code here
  (cond ((empty? sent) '())
        (else (se (first (first sent)) (initials (bf sent)))))
)

(initials '(if i needed someone))

; Exercise 6 - Define copies
(define (copies num wd)
  ; your code here
  (if (= num 0) '()
      (se wd (copies (- num 1) wd)))
)

; (trace copies)
(copies 8 'spam)

; Exercise 7 - Define gpa
(define (base-grade n)
  (cond ((equal? n 'A) 4)
        ((equal? n 'B) 3)
        ((equal? n 'C) 2)
        ((equal? n 'D) 1)
        (else 0)
))

(define (grade-modifier sym)
  (cond ((equal? sym '+) 0.33)
        ((equal? sym '-) -0.33)
        (else 0))
)

(define (cal-grade wd)
  (if (= (count wd) 1) (base-grade wd)
      (+ (base-grade (first wd)) (grade-modifier (bf wd)))
  ))

(base-grade 'A)
(cal-grade 'A+)

(define (gpa-sum grades)
  ; your code here
  (if (empty? grades) 0
      (+ (cal-grade (first grades)) (gpa-sum (bf grades)))
))

(define (gpa grades)
  (/ (gpa-sum grades) (count grades)))

(trace gpa)
(gpa '(A A+ B+ B))

; Exercise 8 - Define repeat-words

(define (repeat-word n wd)
  (if (= n 0) '()
      (se wd (repeat-word (- n 1) wd))))

(repeat-word 4 'calling)

(define (repeat-words sent)
  ; your code here
  (if (empty? sent) '()
      (if (number? (first sent))
          (se (repeat-word (first sent) (first (bf sent))) (repeat-words (bf (bf sent))))
          (se (first sent) (repeat-words (bf sent)))
      )) 
)

(repeat-words '(4 calling birds 3 french hens))
(repeat-words '(the 7 samurai))


             
; Exercise 9 - Define same-shape?
(define (same-shape? sent1 sent2)
  ; your code here
  (if (not (= (count sent1) (count sent2))) #f
        (cond ((and (empty? sent1)
                   (empty? sent2)) #t)
        (else (and (= (count (first sent1)) (count (first sent2))) (same-shape? (bf sent1) (bf sent2))))
        )
  )
)

;(trace same-shape?)
(same-shape? '(the fool on the hill) '(you like me too much))
(same-shape? '(the fool on the hill) '(and your bird can sing))
                       
