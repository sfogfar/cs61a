#lang simply-scheme

;; Helpers
(define (fixed-point f)
  (define tolerance 0.00001)
  (define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) tolerance))
                      (lambda (guess) (f guess)))
   1.0))

;; Q1
;; exercise 1.16
(define (fast-expt b n)
  (define (expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (expt-iter a (square b) (/ n 2)))
          (else (expt-iter (* a b) b (- n 1)))))
  (define (square x) (* x x))
  (define (even? x) (= (remainder x 2) 0))
  (expt-iter 1 b n))

;;exercise 1.35
(fixed-point (lambda (x) (+ 1 (/ 1 x))))

;; exercise 1.37
(define (cont-frac n d k)
  (if (= k 0)
      0
      (/ (n 1) (+ (d 1)
                  (cont-frac (lambda (x) (n (+ x 1)))
                             (lambda (x) (d (+ x 1)))
                             (- k 1))))))

;; Testing 1.37
(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 6))

;; Q2

