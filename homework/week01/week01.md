# Week one homework
## Question one
> Do exercise 1.6, page 25. This is an essay question; you needn’t hand in any computer printout, unless you think the grader can’t read your handwriting. If you had trouble understanding the square root program in the book, explain instead what will happen if you use new-if instead of if in the pigl Pig Latin procedure.
### Answer
Alyssa will be caught in an infinite loop until she reaches a stack overflow. This is because she is no longer using the special form of if and as such must evaluate every parameter. As her second parameter (sqrt-iter) is recursive, this creates a loop.
## Question two
> Write a procedure squares that takes a sentence of numbers as its argument and returns a sentence of the squares of the numbers.

```
> (squares ’(2 3 4 5))
(4 9 16 25)
```
### Answer
```
(define (squares nums)
  (if (empty? nums)
      '()
      (se (square (first nums))
	  (squares (bf nums)) )))
```
## Question three
> Write a procedure switch that takes a sentence as its argument and returns a sentence
in which every instance of the words I or me is replaced by you, while every instance of
you is replaced by me except at the beginning of the sentence, where it’s replaced by I.
(Don’t worry about capitalization of letters.)
```
> (switch ’(You told me that I should wake you up))
(i told you that you should wake me up)
```
### Answer
```
(define (switch sentence)
  (se (switch-first (first sentence))
      (switch-rest (bf sentence))))

(define (switch-first wd)
  (if (equal? wd 'you)'I wd))

(define (switch-rest sentence)
  (if (empty? sentence)
      '()
      (se (switch-any (first sentence))
          (switch-rest (bf sentence)))))

(define (switch-any wd)
  (cond ((equal? wd 'you) 'I)
        ((equal? wd 'I) 'you)
        ((equal? wd 'me) 'you)
        (else wd)))
```
## Question four
> Write a predicate ordered? that takes a sentence of numbers as its argument and returns a true value if the numbers are in ascending order, or a false value otherwise.
### Answer
```
(define (ordered? nums)
  (if (ascending? (first nums) (bf nums))
      #t
      #f))

(define (ascending? num remaining-nums)
  (cond ((empty? remaining-nums) #t)
        ((< num (first remaining-nums))
         (ascending? (first remaining-nums) (bf remaining-nums)))
        (else #f)))
```
## Question five
> Write a procedure ends-e that takes a sentence as its argument and returns a sentence containing only those words of the argument whose last letter is E.
```
> (ends-e ’(please put the salami above the blue elephant))
(please the above the blue)
```
### Answer
```
(define (ends-e sentence)
  (if (empty? sentence)
      '()
      (se (keep-only-e-ends (first sentence))
          (ends-e (bf sentence)))))

(define (keep-only-e-ends wd)
  (if (equal? (last wd) 'e)
      wd
      '()))
```
## Question six
> Most versions of Lisp provide and and or procedures like the ones on page 19. In principle there is no reason why these can’t be ordinary procedures, but some versions of Lisp make them special forms. Suppose, for example, we evaluate
(or (= x 0) (= y 0) (= z 0))
If or is an ordinary procedure, all three argument expressions will be evaluated before or is invoked. But if the variable x has the value 0, we know that the entire expression has to be true regardless of the values of y and z. A Lisp interpreter in which or is a special form can evaluate the arguments one by one until either a true one is found or it runs out of arguments.
Your mission is to devise a test that will tell you whether Scheme’s and and or are special forms or ordinary functions. This is a somewhat tricky problem, but it’ll get you thinking about the evaluation process more deeply than you otherwise might.
Why might it be advantageous for an interpreter to treat or as a special form and evaluate its arguments one at a time? Can you think of reasons why it might be advantageous to treat or as an ordinary function?
### Answer
```
(or 1 2 throw-error-if-or-ordinary)
(and 1 2 throw-error-if-and-special)
```
Reasons for special forms:
1. Spatial effiency
2. Makes recursion possible: if all expressions were evaluated we would miss base cases.
Reasons against special forms:
1. Readability
