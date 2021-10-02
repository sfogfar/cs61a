#lang simply-scheme
;; Q2
(define (squares nums)
  (if (empty? nums)
      '()
      (se (square (first nums))
	  (squares (bf nums)) )))

(define (square x) (* x x))

;; Test Q2
(equal?
 (squares '(2 3 4 5))
 '(4 9 16 25))

;; Q3
(define (switch sentence)
  (se (switch-first (first sentence))
      (switch-rest (bf sentence))))

(define (switch-first wd)
  (if (equal? wd 'You)'I wd))

(define (switch-rest sentence)
  (if (empty? sentence)
      '()
      (se (switch-any (first sentence))
          (switch-rest (bf sentence)))))

(define (switch-any wd)
  (cond ((equal? wd 'you) 'me)
        ((equal? wd 'I) 'you)
        ((equal? wd 'me) 'you)
        (else wd)))

;; Test Q3
(equal?
 (switch '(You told me that I should wake you up))
 '(I told you that you should wake me up))

;;Q4
(define (ordered? nums)
  (if (ascending? (first nums) (bf nums))
      #t
      #f))

(define (ascending? num remaining-nums)
  (cond ((empty? remaining-nums) #t)
        ((< num (first remaining-nums))
         (ascending? (first remaining-nums) (bf remaining-nums)))
        (else #f)))

;; Testing q4
(equal? (ordered? '(1 2 3 4 5)) #t)
(equal? (ordered? '(3 4 1 2 5)) #f)
(equal? (ordered? '(5 4 3 2 1)) #f)

;; Q5
(define (ends-e sentence)
  (if (empty? sentence)
      '()
      (se (keep-only-e-ends (first sentence))
          (ends-e (bf sentence)))))

(define (keep-only-e-ends wd)
  (if (equal? (last wd) 'e)
      wd
      '()))

;; Testing Q5
(equal? (ends-e '(please put the salami above the blue elephant)) '(please the above the blue))

;; Q6
;; (or 1 2 throw-error-if-or-ordinary)
;; (and 1 2 throw-error-if-and-special)
