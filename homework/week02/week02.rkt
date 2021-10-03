#lang simply-scheme
;; Helpers
(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

;; Q1
;; exercise 1.31a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (! a)
    (product (lambda (x) x) 1 inc a))

(define (pi terms) (* 4 (product
                           (lambda (x) (/ (* 2 (+ 1 (floor (/ x 2))))
                                          (+ 1 (* 2 (ceiling (/ x 2))))))
                           1
                           inc
                           terms)))

;; exercise 1.32a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum a b)
  (accumulate + 0 (lambda (x) x) a inc b))

(define (productAlt a b)
  (accumulate * 1 (lambda (x) x) a inc b))

;; exercise 1.33a
(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter (term a)) (combiner (term a)
                                     (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value term (next a) next b))))

;; (filtered-accumulate even? + 0 (lambda (x) x) 1 inc 10)


;; exercise 1.40
(define (cubic a b c) (lambda (x) (+ (* x x x) (* a x x)) (* b x) c))

;; exercise 1.41
(define (double f) (lambda (x) (f (f x))))

;; exercise 1.42
(define (compose f g) (lambda (x) (f (g x))))

;; exercise 1.43
(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose (repeated f (- n 1)) (lambda (x) (f x)))))

;; exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt x)
  ((iterative-improve (lambda (guess) (= guess (average guess (/ x guess))))
                      (lambda (guess) (average guess (/ x guess))))
  1.0))

(define (fixed-point f)
  (define tolerance 0.00001)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) tolerance))
                      (lambda (guess) (f guess)))
   1.0))

;; Q2
(define (every f sent)
  (if (empty? sent)
      '()
      (se (f (first sent))
          (every f (bf sent)))))

;; Testing Q2
(equal?
 (every square '(1 2 3 4))
 '(1 4 9 16))
(equal?
 (every first '(nowhere man))
 '(n m))