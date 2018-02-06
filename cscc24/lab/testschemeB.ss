#lang racket

(require rackunit "schemeB.ss")

(module+ my-filter
  (check-equal? (my-filter (lambda (x) #f) '())
                '())
  (check-equal? (my-filter number? '(5 a 7 "b" 6 8))
                '(5 7 6 8))
)

(module+ eval-poly
  (check-eqv? (eval-poly '(2 8 7) 0)
              2)
  (check-eqv? (eval-poly '(-1 5 -10 10 -5 1) 3)
              32)
)


(module+ poly-func
  (check-eqv? ((poly-func '(5 6 2 1)) 10)
              1265)
)
