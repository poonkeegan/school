
#lang racket

; Question 1.
(define (pn-calc lst)
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
  (let ([a (pn-calc lst)])
    (cond
      [(and (not (null? a)) (number? (car a))) (let ([b (pn-calc (rest a))])
                     (cond
                       [(and (not (null? b)) (number? (car b))) (cons (eval (list x (car a) (car b))) (rest b))]
                       [else `(syntax-error)]))]
      [else `(syntax-error)])))

(define (negop x lst)
  (let ([a (pn-calc lst)])
    (cond
      [(number? (car a)) (cons (- (car a)) (rest a))]
      [else `(syntax-error)]
      )))
   

