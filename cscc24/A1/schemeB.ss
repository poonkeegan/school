#lang racket

; CSCC24 Lab Week 4: Intermediate Scheme exercises.
;
; There are three functions to write.  Their marks (mainly correctness) are
; stated along with their comments.
;
; Due Friday 6PM on MarkUs: https://markus.utsc.utoronto.ca/cscc24w18/

; This line exports the functions.
(provide my-filter eval-poly poly-func)


; This function is worth 2 marks.
;
; Recall my-filter from lecture:
;
; (define (my-filter pred lst)
; (match lst
;   ['() '()]
;   [(cons hd tl) (if (pred hd)
;                     (cons hd (my-filter pred tl))
;                     (my-filter pred tl))]))
;
; Write a new version that uses foldr to replace the recursion.
(define (my-filter pred lst)
    (foldr
        (lambda (start end) (if (pred start)
                                (cons start end)
                                end)) `() lst))


; This function is worth 2 marks.
;
; Evaluate a polynomial p at a point x, using Horner's rule.
;
; The polynomial is represented by a list of the coefficients, from the constant
; term to the highest degree term, e.g.,
;
; 2 + 3x - 1x^2 + 5x^3 is represented by '(2 3 -1 5).
;
; Horner's rule says e.g.,
;
;   d + c x + b x^2 + a x^3
; = d + x*(c + x*(b + x*(a + 0)))
;
; See how it's trying to say, recursively,
;
;   (eval-poly '(d c b a) x)
; = d + x * (eval-poly '(c b a) x)
;
; Implement this using foldr.
(define (eval-poly p x)
    (foldr 
        (lambda (con prev) (+ con (* x prev )))
        0 p))



; This function is worth 1 mark.
;
; Take a list that represents a polynomial (same convention as above) and
; convert it to a Scheme function.
;
; Example: (poly-func '(8 1 0 2)) is a function, and I can use it like
; (define f (poly-func '(8 1 0 2)))
; and (f 10) gives me 2018.
;
; You can use eval-poly to help you.
(define (poly-func p)
  (lambda (x) (eval-poly p x)))
