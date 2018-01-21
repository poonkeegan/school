#lang racket

; Question 1.
(define (pn-calc lst)
    (let ([x (pn-calc-hlp lst)])
      (if (or (equal? x `syntax-error) (null? (cdr x)))
        x
        `syntax-error)))

(define (pn-calc-hlp lst)
  (cond 
    [(null? lst) `syntax-error]
    [else (match (first lst)
        [ `+ (pn-calc-add (rest lst))]
        [ `- (pn-calc-sub (rest lst))]
        [ `* (pn-calc-mul (rest lst))]
        [ `/ (pn-calc-div (rest lst))]
        [ `neg (pn-calc-neg (rest lst))]
        [? (number? (first lst)) (cons (first lst) (rest lst))]
        [ _ `syntax-error])]))

(define (pn-calc-add lst)
  (cond
    [(or (null? lst) (null? (cdr lst))) `syntax-error]
    [else (let ([x (pn-calc-hlp lst)])
        (let ([y (pn-calc-hlp (cdr x))])
          (cond
            [(and (number? (car x)) (number? (car y))) (cons (+ (car x) (car y)) (cdr y))]
            [else `syntax-error])))]))

(define (pn-calc-sub lst)
  (cond
    [(or (null? lst) (null? (cdr lst))) `syntax-error]
    [else (let ([x (pn-calc-hlp lst)])
        (let ([y (pn-calc-hlp (cdr x))])
          (cond
            [(and (number? (car x)) (number? (car y))) (cons (- (car x) (car y)) (cdr y))]
            [else `syntax-error])))]))

(define (pn-calc-mul lst)
  (cond
    [(or (null? lst) (null? (cdr lst))) `syntax-error]
    [else (let ([x (pn-calc-hlp lst)])
        (let ([y (pn-calc-hlp (cdr x))])
          (cond
            [(and (number? (car x)) (number? (car y))) (cons (* (car x) (car y)) (cdr y))]
            [else `syntax-error])))]))

(define (pn-calc-div lst)
  (cond
    [(or (null? lst) (null? (cdr lst))) `syntax-error]
    [else (let ([x (pn-calc-hlp lst)])
        (let ([y (pn-calc-hlp (cdr x))])
          (cond
            [(and (number? (car x)) (number? (car y))) (cons (/ (car x) (car y)) (cdr y))]
            [else `syntax-error])))]))

(define (pn-calc-neg lst)
  (cond
    [(null? lst) `syntax-error]
    [else (let ([x (pn-calc-hlp lst)])
            (cond
              [(number? (car x)) (cons (- (car x)) (cdr x))]
              [else `syntax-error]))]))

; Binary leafy tree.
(struct branch (left right) #:transparent)
(struct leaf (datum) #:transparent)
; The #:transparent enables printing out these records at the REPL.

(define alberts-tree
  (branch
    (branch
      (leaf 2)
      (branch (leaf 7) (leaf 8)))
    (branch (leaf 6) (leaf 3))))


; Question 2.
(define (blt-fold binop f tree)
  (cond
    [(branch? tree) 
     (binop 
       (blt-fold binop f (branch-left tree)) 
       (blt-fold binop f (branch-right tree)))]
    [(leaf? tree) (f (leaf-datum tree))]))


; Question 3.
(define (blt-foldl a0 binop tree)
  'TO-DO)
