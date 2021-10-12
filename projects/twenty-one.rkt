#lang simply-scheme
;; CS61A Project One
;; Twenty-One

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

(define (best-total hand)
  (define (total-iter hand aces total)
    (if (empty? hand)
        (use-aces aces total)
        (let ((card (first (first hand))))
        (cond ((equal? card 'A)
               (total-iter (bf hand) (+ aces 1) total))
              ((member? card '(K Q J))
               (total-iter (bf hand) aces (+ total 10)))
              (else
               (total-iter (bf hand) aces (+ total card)))))))
  (define (use-aces aces total)
    (cond ((= 0 aces) total)
          ((< total 10) (use-aces (- aces 1) (+ total 11)))
          (else (use-aces (- aces 1) (+ total 1)))))
  (total-iter hand 0 0))

;; Testing
;; best-total returns total value in hand with best use of ace
(equal? (best-total '(AD 8S)) 19)
(equal? (best-total '(AD 8S 5H)) 14)
(equal? (best-total '(AD AS 9H)) 21)