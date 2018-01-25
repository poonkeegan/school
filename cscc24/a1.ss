#lang racket

; Question 1.
(define (pn-calc lst)
    (define (pn-calc-hlp lst)
        (match lst
            [(list head tail ...)
                (let* ([x (pn-calc-hlp tail)]
                       [y (pn-calc-hlp (cdr x))]
                       [felmt (car x)]
                       [selmt (car y)])
                      (match head
                          [(or `+ `- `* `/) 
                              (match (list felmt selmt)
                                  [(list (? number?) (? number?))
                                      (cons (eval 
                                                (list head felmt selmt))
                                                (rest y))]
                                  [_ `(syntax-error)])]
                          [`neg 
                              (match felmt
                                  [(? number?) (cons (- felmt) (rest x))]
                                  [_ `(syntax-error)])]
                          [(? number?) lst]
                          [_ `(syntax-error)]))]
                [_ `(syntax-error)]))
    (let ([x (pn-calc-hlp lst)])
      (if (equal? (car x) `syntax-error) 
        `syntax-error
        x)))


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


;(pn-calc `(+ 10 4))
;(pn-calc `(+ * 3 2 - 7 5))
;(pn-calc `(* * 6 - 7 5 + 4 1))
;(pn-calc `(neg 5))
;(pn-calc `(* neg 3 neg 5))
;(pn-calc `(3 neg 5))
;(pn-calc `(neg 3 neg 5))
