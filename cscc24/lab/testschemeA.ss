#lang racket

(require rackunit "schemeA.ss")

(module+ test-deep-rec
  (check-equal? (deep-reverse '()) '())
  (check-equal? (deep-reverse '(1 (3 (5 6) 2) 4))
                '(4 (2 (6 5) 3) 1))
  (check-equal? (deep-reverse '(a l b e r t))
                '(t r e b l a))
  (check-equal? (deep-reverse '((monochrom "bernard" #f) (("albert" 42) #t) ()))
                '(() (#t (42 "albert")) (#f "bernard" monochrom)))) 

; (struct secret (???))

(module+ test-sorted-merge
  (check-equal? (sorted-merge <= '(2 3 5) '(1 4))
                '(1 2 3 4 5))
  (check-equal? (sorted-merge string>=? '("tea" "donut") '("waffle" "jello" "cake"))
                '("waffle" "tea" "jello" "donut" "cake"))
  (check-equal? (sorted-merge >= '() '()) '())
  (check-equal? (sorted-merge >= '() '(3 3 4)) '(3 3 4))
  (check-equal? (sorted-merge string<=? '("albert" "bon" "cop") '())
                '("albert" "bon" "cop"))
)

(module+ test-bst-height
  (check-eqv? (bt-height (node 5 'nil (node 6 (node 7 'nil 'nil) 'nil)))
              3)
  (check-eqv? (bt-height 'nil) 0)
  (check-eqv? (bt-height (node 'x 'nil 'nil))
              1)
  (check-eqv? (bt-height (node 'x
                               (node 'y 'nil 'nil)
                               (node 'z
                                     (node 'a
                                           'nil
                                           (node 'c 'nil 'nil))
                                     (node 'b 'nil 'nil))))
              4))

(module+ test-bst?-numbers
  (check-false (bst? <= (node 5 'nil (node 6 (node 7 'nil 'nil) 'nil))))
  (check-true (bst? <= (node 5 'nil (node 7 (node 6 'nil 'nil) 'nil))))
  (check-false (bst? >= (node 5 'nil (node 7 (node 6 'nil 'nil) 'nil))))
  (check-true (bst? <= 'nil))
  (check-true (bst? >= (node 7 'nil 'nil))) 
  (define t1 (node 4
                   (node 2
                         (node 1 'nil 'nil)
                         (node 3 'nil 'nil))
                   (node 5 'nil 'nil)))
  (define t2 (node 12
                   (node 11 'nil 'nil)
                   (node 14
                         (node 13 'nil 'nil)
                         (node 15 'nil 'nil))))
  (check-false (bst? <= (node 7 t2 t1)))
  (check-false (bst? >= (node 7 t2 t1)))
  (check-true (bst? <= (node 7 t1 t2))))

(module+ test-bst?-secret
  (struct secret (datum))

  (define (sec<= s1 s2)
    (string>=? (secret-datum s1) (secret-datum s2)))

  (define (sn d l r) (node (secret d) l r))

  (define t1 (sn "y" (sn "z" 'nil 'nil) (sn "x" 'nil 'nil)))
  (define t2 (sn "b" (sn "c" 'nil 'nil) (sn "a" 'nil 'nil)))

  (check-true (bst? sec<= (sn "mid" t1 t2)))
  (check-false (bst? sec<= (sn "mid" t2 t1))))
