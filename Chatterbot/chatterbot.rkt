#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

;;Begin Project 1
(require "adjectives.rkt") 

;;Below is some procedures to help you out. Dont worry about what it says or does.
;;Questions are below.

(define (want-exit? line)
  (or (member? 'exit line) (member? 'quit line) (member? 'bye line)))

(define (print-sentence sent)
  (for-each (lambda (x) (display x) (display " "))
            sent)
  (newline))

(define (interact bot)
  (define (helper)
    (display "> ") (flush-output)
    (let ([line (read-line)])
      (unless (want-exit? line)
        (print-sentence (bot line))
        (helper))))
  (read-line)
  (helper))

(define (chatter bot1 bot2 start iterations)
  (define (helper b1 b2 sent i)
    (when (< i iterations)
          (display "bot ") (display (add1 (remainder i 2))) (display ": ")
          (let ((out (b1 sent)))
            (print-sentence out)
            (helper b2 b1 out (add1 i)))))
  (display "start: ") (print-sentence start)
  (helper bot1 bot2 start 0))

;;; Checks if a given word is an adjective or not
;;; Requires adjectives.scm to be loaded beforehand
(define adjective?
  (let ((hash (make-hash)))
    (for-each (lambda (adj)
		(hash-set! hash adj #t))
	      adjectives)
    (lambda (wd) (hash-ref hash wd #f))))


;; Begin Questions:
;;Q1 - babybot
  (define (babybot sent)
    ;;insert your answer here
    (if (equal? sent 'bye) 'Okay
        sent)
  )

;;Q2 - stupidbot-creator
  (define (stupidbot-creator motto)
    ;;insert your answer here
    (lambda (x) motto)
  )

(define dalek (stupidbot-creator '(exterminate!)))
(dalek '(I am the doctor!))

;;Q3 - matcherbot-creator
; It's interpretted that the pattern has to be in the sent as a whole, ie, no word in between
  (define (matcherbot-creator pattern)
    ;;insert your (trace iter)
    (define (iter sent pattern full-pattern started)
          (cond ((and (empty? sent) (empty? pattern)) '())
                ((empty? sent) #f)
                ((empty? pattern) sent)
                ((equal? (first sent) (first pattern)) (iter (bf sent) (bf pattern) full-pattern #t))
                ((equal? started #t) (iter sent full-pattern full-pattern #f))
                (else (iter (bf sent) pattern full-pattern #f))
                 ))
    ; (trace iter)
    (lambda (sent)
        (if (empty? pattern) sent
            (iter sent pattern pattern #f))
  ))

(define cedric (matcherbot-creator '(hufflepuffs are great)))
(cedric '(hufflepuffs are great finders))
(cedric '(what the heck is a hufflepuff))
(cedric '(slytherins hate hufflepuffs but hufflepuffs are great finders))
(cedric '(slytherins hate hufflepuffs but hufflepuffs are the great finders))

;;Q4 - substitutebot-creator
  (define (substitutebot-creator from to)
    ;;insert your answer here
    (define (replace wd from to)
      (if (equal? wd (first from)) (first to)
          (replace wd (bf from) (bf to))))
    (define (iter sent)
      (cond ((empty? sent) '())
            ((member? (first sent) from) (se (replace (first sent) from to) (iter (bf sent))))
            (else (se (first sent) (iter (bf sent))))))
    (lambda (sent) (if (or (empty? from) (empty? to)) sent
                       (iter sent)))
  )

(define marions-vacay
          (substitutebot-creator '(indonesia winter noodles)
                                 '(texas summer steak)))

(marions-vacay '(I visited indonesia this winter and ate noodles))

;;Q5 - switcherbot
  (define (switcherbot sent)
    ;;insert your answer here
    (define me '(me I am was my yours))
    (define you '(you you are were your mine))
    (define switch (substitutebot-creator (se me you)
                                       (se you me)))
    (if (member (first sent) '(you You)) (se 'I (switch (bf sent)))
        (switch sent))
  )

(switcherbot '(you are reia but I am a bird))

;;Q6 - inquisitivebot
  (define (inquisitivebot sent)
    ;;insert your answer here
    (if (empty? sent) sent
        (se (switcherbot sent) '?))
  )

(inquisitivebot '(I am happy))
(inquisitivebot '(I can see you))

;;Q7 - eliza
  (define (eliza sent)
    ;;insert your answer here
    (define cedric? (matcherbot-creator '(I am)))
    (cond ((empty? sent) '(how can I help you ?))
          ((equal? (first sent) 'hello) '(hello there!))
          ((not (equal? #f (cedric? sent))) (se '(why are you) (switcherbot (cedric? sent)) '?))
          ((equal? (last sent) '?) '(I can not answer your question.))
          (else
           (switcherbot sent)))
  )

(eliza '(hello))
(eliza '(I am excited to finish unit 1))
(eliza '(you are happy to see me))
(eliza '(how are you ?))

;;Q8 - reactorbot-creator
  (define (reactorbot-creator bot pat out)
    ;;insert your answer here
    (lambda (sent) (if (equal? pat sent) out
                       (bot sent)))
  )

(define stupidbot (stupidbot-creator '(I am Groot)))
(define groot (reactorbot-creator stupidbot '(no Groot youll die why are you doing this) '(WE are Groot)))
(groot '(whats up groot))
(groot '(no Groot youll die why are you doing this))

;;Q9 - replacerbot-creator
  (define (replacerbot-creator bot pat before after)
    ;;insert your answer here
    (define cedric? (matcherbot-creator pat))
    (lambda (sent) (
                    cond ((not (equal? #f (cedric? sent))) (se before (cedric? sent) after))
                          (else (bot sent))))
  )

(define stupidbot2 (stupidbot-creator '(I am Groot)))
(define groot2 (replacerbot-creator stupidbot2 '(no Groot youll die why are you doing this) 'before
                                   'after ))
(groot2 '(whats up groot))
(groot2 '(no Groot youll die why are you doing this ahh))

;;Q10 - exagerate
  (define (exaggerate bot n)
    ;;insert your answer here
    (define (iter sent n)
      (cond ((= n 0) sent)
          (else (iter (replace sent) (- n 1)))))
    (define (replace sent)
      (cond ((empty? sent) '())
            ((adjective? (first sent)) (se 'very (first sent) (replace (bf sent))))
            (else (se (first sent) (replace (bf sent))))))
    (lambda (sent) (iter (bot sent) n))
  )

(define exaggerated-babybot1 (exaggerate babybot 1))
(exaggerated-babybot1 '(this soup is hot and tasty))
(define exaggerated-babybot2 (exaggerate babybot 2))
(exaggerated-babybot2 '(this soup is hot and tasty))

;;REMEMBER TO ADD YOUR OWN TESTS TO GRADER.RKT!
;;END OF PROJECT 1
