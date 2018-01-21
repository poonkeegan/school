#lang racket

; Question 1.
(define (pn-calc lst)
    (let ([x (pn-calc-hlp lst)])
      (if (equal? (car x) `syntax-error) 
        `syntax-error
        x)))

(define (pn-calc-hlp lst)
  (cond
    [(null? lst) `(syntax-error)]
    [else (let ([x (first lst)])
            (cond
                [(ormap (lambda (y) (equal? x y)) `(+ - * /))
                 (binop x (rest lst))]
                [(equal? `neg x) (negop x (rest lst))]
                [(number? x) (cons x (rest lst))]
                [else `(syntax-error)]))]))
(define (binop x lst)
  (let ([a (pn-calc-hlp lst)])
    (cond
      [(and (not (null? a)) (number? (car a))) (let ([b (pn-calc-hlp (rest a))])
                     (cond
                       [(and (not (null? b)) (number? (car b))) (cons (eval (list x (car a) (car b))) (rest b))]
                       [else `(syntax-error)]))]
      [else `(syntax-error)])))

(define (negop x lst)
  (let ([a (pn-calc-hlp lst)])
    (cond
      [(number? (car a)) (cons (- (car a)) (rest a))]
      [else `(syntax-error)]
      )))
   

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
  (cond
    [(branch? tree)
     (let ([x (blt-foldl a0 binop (branch-left tree))])
       (blt-foldl x binop (branch-right tree)))]
    [(leaf? tree) (- a0 (leaf-datum tree))]))
