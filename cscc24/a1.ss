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
                      (match (list head felmt selmt)
                          ; Case one, binary operation
                          [(list (or `+ `- `* `/) 
                                 (? number?)
                                 (? number?))
                           (cons
                               ((match head [`+ +]
                                            [`- -]
                                            [`* *]
                                            [`/ /]) felmt selmt)
                               (rest y))]
                          ; Case two, unary operation
                          [(list `neg (? number?) _ )
                           (cons (- felmt) (rest x))]
                          ; Case three, only a number
                          [(list (? number?) _ _) lst]
                          ; Everything else
                          [_ `(syntax-error)]))]
                [_ `(syntax-error)]))
    (let ([x (pn-calc-hlp lst)])
      (if (null? (cdr x) ) 
        (car x)
        `syntax-error)))


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
        [(leaf data) (binop a0 data)]
        [(branch left right) 
         (blt-foldl (blt-foldl a0 binop left) binop right)]))

; (pn-calc `(+ 10 4))
; (pn-calc `(+ * 3 2 - 7 5))
; (pn-calc `(* * 6 - 7 5 + 4 1))
; (pn-calc `(neg 5))
; (pn-calc `(* neg 3 neg 5))
; (pn-calc `(3 neg 5))
; (pn-calc `(neg 3 neg 5))
; (displayln 'test )
; (pn-calc-hlp '(*)) ;too less number (first operand)
; (pn-calc-hlp '(* * 2)) ;too less number second case (second operand)
; (pn-calc-hlp '(* 1 2 3)) ;too many numbers
; (pn-calc-hlp (list 2))
; (pn-calc-hlp (list '* 1 2))
; (pn-calc-hlp (list '* '* 1 2 3))
; (pn-calc-hlp (list '+ '* 1 2 100))
; (pn-calc-hlp (list 'neg 2))
; (pn-calc-hlp '(* neg 3 neg 5))
; (pn-calc-hlp '(+ 10 4))
; (pn-calc-hlp '(+ * 3 2 - 7 5 ))
; (pn-calc-hlp '(* * 6 - 7 5 + 4 1))
; (pn-calc-hlp '(* neg 3 neg 5 ))
; (pn-calc-hlp '(neg neg 5))
; (pn-calc-hlp '(neg 5 neg))
; (pn-calc-hlp '(neg 5 5))
; (displayln 'sss )
; (pn-calc '(*)) ;too less number (first operand)
; (pn-calc '(* * 2)) ;too less number second case (second operand)
; (pn-calc '(* 1 2 3)) ;too many numbers
; (pn-calc (list 2))
; (pn-calc (list '* 1 2))
; (pn-calc (list '* '* 1 2 3))
; (pn-calc (list '+ '* 1 2 100))
; (pn-calc (list 'neg 2))
; (pn-calc '(* neg 3 neg 5))
; (pn-calc '(+ 10 4))
; (pn-calc '(+ * 3 2 - 7 5 ))
; (pn-calc '(* * 6 - 7 5 + 4 1))
; (pn-calc '(* neg 3 neg 5 ))
; (pn-calc '(neg neg 5))
; (pn-calc '(neg 5 neg))
; (pn-calc '(neg 5 5))