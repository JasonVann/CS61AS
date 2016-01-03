; Exercise 1 - Define describe-time
(define (describe-time secs)
    (cond ((< secs 60) (se secs 'seconds))
            ((< secs 3600) (se  (quotient secs 60) 'minutes (describe-time (modulo secs 60))))
            ((< secs (* 24 3600)) (se  (quotient secs 3600) 'hours (describe-time (modulo secs 3600))))
            (else (se  (quotient secs (* 24 60 60)) 'days (describe-time (modulo secs (* 24 60 60))))
)))   


; Exercise 2 - Define remove-once
(define (remove-all wd sent)
    (if (empty? sent) '()
        (se (if (equal? wd (first sent)) '() (first sent) ) (remove-all wd (bf sent)))

))

(define (remove-once wd sent)
    (cond ((empty? sent) '())
        ((equal? wd (first sent)) (bf sent))
        (else (se (first sent) (remove-once wd (bf sent))))
))

; Exercise 3 - Define differences
(define (differences nums)
    (cond ((< (count nums) 2) '())
        (else (se (- (first (bf nums)) (first nums)) (differences (bf nums)))))

)

; Exercise 4 - Define location
(define (location-helper n  small big)
    (cond ((equal? small (first big))  n )
        ((equal? (length big) 1) #f)
        (else (location-helper (+ n 1)  small (bf big))))
)

(define (location small big)
    (location-helper 1 small big))


; Exercise 5 - Define initials
(define (initials sent)
    (if (empty? sent)
        '()
        (se (first (first  sent)) (initials (bf sent)))))


; Exercise 6 - Define copies
(define (copies num wd)
    (if (= num 1) wd
        (se wd (copies (- num 1) wd))))


; Exercise 7 - Define gpa
(define (gpa grades)
    (if (empty? grades) 0
        (/ (+ (grade-modifier (first grades))
             (gpa (bf grades))) (length grades))))

(define (total-gpa grades)
    (define n 0)
    (if (empty? grades) 0
          (+ (grade-modifier (first grades)) 
             (total-gpa (bf grades))) (length grades)
 ))

(define (gpa grades)
    (/ (total-gpa grades) (length grades)))


(define (base-grade grade)
    (cond ((equal? (first grade) 'A) 4)
            ((equal?  (first grade) 'B) 3)
            ((equal? (first grade) 'C) 2)
            ((equal? (first grade) 'D) 1)
            (else 0)
))

(define (grade-modifier grade)
    (cond ((equal? (bf grade) '+) (+ 0.33 (base-grade grade)))
          ((equal? (bf grade) '-) (+ -0.33 (base-grade grade)))
            (else (cal-grade grade))))


; Exercise 8 - Define expand

(define (expand sent)
    (cond ((empty? sent) '())
        ((equal? (length sent) 1) (first sent))
        ((number? (first sent)) (se (expand-helper (first sent) (first (bf sent))) (expand (bf (bf sent)))))
        (else (se (first sent) (expand (bf sent))))))

(define (expand-helper n sent)
    (if (equal? n 1) (se sent)
        (se sent (expand-helper (- n 1) sent))))


; Exercise 9 - Define same-shape?
(define (same-shape? sent1 sent2)
    (cond ((not (equal? (length sent1) (length  sent2))) #f)
         ((not (equal? (count (first sent1)) (count (first sent2)))) #f)
        ((equal? (length sent1) 1) #t)

        (else (same-shape? (bf sent1) (bf sent2))))

)

(define (count wd)
    (if (empty? wd) 0
        (+ 1 (count (bf wd)))))  
