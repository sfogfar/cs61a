# Week three homework
## Question one
> Abelson & Sussman, exercises 1.16, 1.35, 1.37, 1.38
### Answer
#### exercise 1.16
```
(define (fast-expt b n)
  (define (expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (expt-iter a (square b) (/ n 2)))
          (else (expt-iter (* a b) b (- n 1)))))
  (define (square x) (* x x))
  (define (even? x) (= (remainder x 2) 0))
  (expt-iter 1 b n))
```
#### exercise 1.35
If a fixed point is `f(x) = x` and the golden ratio is `f(x) = 1 + (1 / x)` then the fixed point of it should be `x = 1 + (1 / x)`, or `x + 1 =  x^2`. Therefore:
```
(fixed-point (lambda (x) (+ 1 (/ 1 x))))
```
_Where fixed-point is as defined in my solution to 1.46 for the week02 homework._
