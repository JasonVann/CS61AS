#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1

; SICP 2.7 - Define upper-bound and lower-bound

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

; We assum that the lower-bound may not necessarily on the left
(define (lower-bound interval)
    (min (car interval) (cdr interval)))

(define (upper-bound interval)
    (max (car interval) (cdr interval)))

; SICP 2.8 - Define sub-interval

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; SICP 2.10 - Modify div-interval

(define (div-interval x y)
  (if (and (>= (upper-bound y) 0
            ) (<= (lower-bound y) 0)
        ) (error "Spans over zero!")
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))


;SICP 2.12 - Define make-center-percent and percent

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percent)
    (let ((w (/ (* center percent) 100)))
        (make-center-width center w)))

(define (percent interval)
    (* (/ (width interval) (center interval)) 100))

; SICP 2.17 - Define last-pair

(define (last-pair l)
  (cond ((null? l) nil)
        ((null? (cdr l))
             l)
      (else (last-pair (cdr l)))))

; SICP 2.20 - Define same-parity

(define (same-parity x . y)
  (filter (if (even? x) even? odd?)
      (cons x y)))

(same-parity 1 2 3 4 5 6 7)

; SICP 2.22 - Write your explanation in the comment block:

#|
Your explanation here
|#

; Exercise 2 - Define my-substitute

(define (substitute lst old new)
  (cond ((empty? lst) '())
        ((not (list? lst)) (if (equal? lst old) new old))
        ((and (list? lst) (list? (car lst))) (cons (substitute (car lst) old new) (substitute (cdr lst) old new)))
        ((and (list? lst) (not (list? (car lst)))) (if (equal? (car lst) old) (cons new (substitute (cdr lst) old new))
                                                       (cons (car lst) (substitute (cdr lst) old new))))
        
          )
)

; (trace my-substitute)
(substitute '((lead guitar) (bass guitar) (rhythm guitar) drums)
                  'guitar
                  'axe)

; Exercise 3 - Define my-substitute2

(define (replace lst1 lst2 ele)
  (cond ((empty? lst1) '())
        ((equal? (car lst1) ele) (car lst2))
        (else (replace (cdr lst1) (cdr lst2) ele))))

(define (substitute2 lst old new)
  (cond ((empty? lst) '())
        ((not (list? lst)) (if (member? lst old) (replace old new lst) lst))
        ((and (list? lst) (list? (car lst))) (cons (substitute2 (car lst) old new) (substitute2 (cdr lst) old new)))
        ((and (list? lst) (not (list? (car lst)))) (if (member? (car lst) old) (cons (replace old new (car lst)) (substitute2 (cdr lst) old new))
                                                       (cons (car lst) (substitute2 (cdr lst) old new))))
        
          )
)

; (trace substitute2)
(substitute2 '((4 calling birds) (3 french hens) (2 turtle doves))
                   '(1 2 3 4)
                   '(one two three four))

; P4
(define (cxr-function wd)
  (define (iter wd) 
  (cond ((null? wd) '())
        ((empty? wd) '())
        ((equal? 'a (first wd)) (list 'car (iter (bf wd))))
        ((equal? 'd (first wd)) (list 'cdr (iter (bf wd))))
        (else (iter (bf wd)))))
  
  (iter wd)
)

#|
(define (cxr-function2 wd)
  (define (iter wd) 
  (cond ((null? wd) '())
        ((empty? wd) '())
        ((equal? 'a (first wd)) ((car (iter (bf wd)))))
        ((equal? 'd (first wd)) (cdr (iter (bf wd))))
        (else (iter (bf wd)))))
  
  (iter wd)
)

; use lambda?


(define (iter wd) 
  (cond ((null? wd) (lambda (n) (n)))
        ((empty? wd) (lambda (n) (n)))
        ((equal? 'a (first wd)) ((car (iter (bf wd)))))
        ((equal? 'd (first wd)) (cdr (iter (bf wd))))
        (else (iter (bf wd)))))

(trace iter)
  (iter wd)
|#

(define wd 'cdddadaadar)
; ((lambda () (car (cdr wd))) wd)
    
; (cxr-function 'cdddadaadar)

; P5
#|
SICP Ex. 2.6. Besides addition, invent multiplication and exponentiation of nonnegative integers.
If you're really enthusiastic, see if you can invent subtraction. (Remember, the rule of this game is that
you have only lambda as a starting point.) Read ~cs61as/lib/church-hint for some suggestions.
|#

; P6
(define (reverse2 l)
  (define (reverse-iter lst result)
  (cond ((null? lst) result)
        ((null? (cdr l))
                (cons (car l) result))
        (else  (reverse-iter (cdr lst) (cons (car lst) result)))))
  (reverse-iter l nil))

(reverse2 (list 1 2 3 4 5))