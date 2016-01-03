

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom
10
;2. Compound Expression (3 Atoms)
(+ 3 7)
;3. Compound Expression (4 Atoms)
(+ 1 2 7)
;4. Compound Expression (1 Atom and 2 subexpressions)
(+ (+ 1 2) (+ 3 4))
;5. Any Other Kind Expression
(- 2 -8)

;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  ; your code here
  (word (first wd) (first (bf (first wd))))
)

(first-two 'ambulatory)

;;2. Define two-first
(define (two-first x y)
  ; your code here
  (error "Not yet implemeted")
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  ; your code here
  (error "Not yet implemented")
)

;Exercise 2 - Define teen?
(define (teen? num)
  ; your code here
  (error "Not yet implemented")
)

;Exercise 3 - Define indef-article
(define (indef-article wd)
  ; your code here
  (error "Not yet implemented")
)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  ; your code here
    (se (bl sent) 'and (last sent))
)

;Exercise 5 - Define query
(define (query sent)
  ; your code here
    (se (item 2 sent) (first sent) (bl(bf (bf sent) )) (word (last sent) '?))
)

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  ; your code here
    (se '(european-time:) (if(equal? (last time) 'am)
        (first time)
         (+ (first time) 12))) 
)

(define (american-time time)
  ; your code here
  (error "Not yet implemented")
)

;Exercise 7 - Define describe-time
(define (describe-time2 secs)
    (cond ((equal? (last secs) 'days) (* (first secs) 24 60 60))
    ((equal? (last secs) 'hour) 14))
)

(define (describe-time secs)
    (cond ((< secs 60) (se secs 'seconds))
    ((< secs 3600)  (se (/ secs 60) 'minutes))
    ((< secs (* 24 3600)) (se (/ secs 60 60) 'hours))
    (else (se (/ secs 24 60 60 ) 'days ))
)
)

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective wd)
  (se (word adjective 'est) wd))

