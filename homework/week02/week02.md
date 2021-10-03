# Week two homework
## Question one
> Abelson & Sussman, exercises 1.31(a), 1.32(a), 1.33, 1.40, 1.41, 1.43, 1.46
### Answer
#### exercise 1.31a
```
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (! a)
    (product (lambda (x) x) 1 (lambda (x) (+ x 1)) a))

(define (pi terms) (* 4 (product
                           (lambda (x) (/ (* 2 (+ 1 (floor (/ x 2))))
                                          (+ 1 (* 2 (ceiling (/ x 2))))))
                           1
                           (lambda (x) (+ x 1))
                           terms)))
```
#### exercise 1.32a
```
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum a b)
  (accumulate + 0 (lambda (x) x) a inc b))

(define (product a b)
  (accumulate * 1 (lambda (x) x) a inc b))
```
#### exercise 1.33a
```
(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter (term a)) (combiner (term a)
                                     (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value term (next a) next b))))

(filtered-accumulate even? + 0 (lambda (x) x) 1 inc 10) ;; Chose to filter by even? instead of prime?
```
#### exercise 1.40
```
(define (cubic a b c) (lambda (x) (+ (* x x x) (* a x x)) (* b x) c))
```
#### exercise 1.41
```
(define (double f) (lambda (x) (f (f x))))
```
`(((double (double double)) inc) 5)` will return `21`.
#### exercise 1.43
```
(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose (repeated f (- n 1)) (lambda (x) (f x)))))
```
#### exercise 1.46
```
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
```
## Question 3
> Last week you wrote procedures squares, that squared each number in its argument sentence, and saw pigl-sent, that pigled each word in its argument sentence. Generalize this pattern to create a higher-order procedure called every that applies an arbitrary procedure, given as an argument, to each word of an argument sentence. This procedure is used as follows:
```
> (every square ’(1 2 3 4))
(1 4 9 16)

> (every first ’(nowhere man))
(n m)
```
### Answer
```
(define (every f sent)
  (if (empty? sent)
      '()
      (se (f (first sent))
          (every f (bf sent)))))
```
