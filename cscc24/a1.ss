#lang racket

; Question 1.
(define (pn-calc lst)
    (let ([x (pn-calc-hlp lst)])
      (if (equal? (car x) `syntax-error) 
        `syntax-error
        x)))

(define (pn-calc-hlp lst)
    (match lst
        [(list head tail ...)
            (match head
                [(or `+ `- `* `/) (binop head tail)]
                [`neg (negop tail)]
                [(? number?) lst]
                [_ `(syntax-error)])]
        [_ `(syntax-error)]))

(define (binop op lst)
    (match lst
        [(list head ...)
            (let ([x (pn-calc-hlp head)])
                (let ([y (pn-calc-hlp (cdr x))])
                    (match (list (car x) (car y))
                        [(list (? number?) (? number?))
                            (cons (eval (list op (car x) (car y))) (rest y))]
                        [_ `(syntax-error)])))]
        [_ `(syntax-error)]))

(define (negop lst)
  (let ([a (pn-calc-hlp lst)])
    (match a
      [(? number?) (cons (- (car a)) (rest a))]
      [_ `(syntax-error)])))
   

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
    (match tree
        [(leaf data) (f data)]
        [(branch left right)
         (binop (blt-fold binop f left) 
                (blt-fold binop f right))]))

; Question 3.
(define (blt-foldl a0 binop tree)
    (match tree
        [(leaf data) (- a0 data)]
        [(branch left right) 
         (blt-foldl (blt-foldl a0 binop left) binop right)]))
