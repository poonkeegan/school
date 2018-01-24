#lang racket

; CSCC24 Lab Week 3: Basic scheme exercises.
;
; There are 4 functions to write. Each is worth 1 mark for correctness.
; Then there is 1 more mark overall for good style and layout.
;
; Due Friday 6PM on MarkUs: https://markus.utsc.utoronto.ca/cscc24w18/

; This line exports the 4 functions and the node type.
(provide deep-reverse sorted-merge (struct-out node) bt-height bst?)


; Take a list with possible nesting. Return reversed nested list---reverse at
; every level!
;
; Example: (deep-reverse '(1 (3 (5 6) 2) 4))
; answer: '(4 (2 (6 5) 3) 1)
;
; It is OK to use the "reverse" function to help you.
; Also recall the "map" function from the first lecture.
(define (deep-reverse nlst)
  (match nlst
    [(? number?) nlst]
    [_ (reverse (map deep-reverse nlst))]))


; Take 3 parameters: a comparator and two lists.
; "comparator" means a function f such that (f x y) tells whether x is
; "less than or equal to" y.
; 
; Assume the precondition that each input list is "non-decreasing" according to
; the comparator. Perform the "merge" of "mergesort", again the desired ordering
; is according to the comparator.
;
; Example: (sorted-merge <= '(2 3 5) '(1 4))
; answer: '(1 2 3 4 5)
;
; Example: (sorted-merge string>=? '("tea" "donut") '("waffle" "jello" "cake"))
; answer: '("waffle" "tea" "jello" "donut" "cake")
(define (sorted-merge cmp lstA lstB)
  (match (list lstA lstB)
    [(or (list (? null?) _) (list _ (? null?))) (append lstA lstB)]
    [_ (if (cmp (car lstA) (car lstB))
           (cons (car lstA) (sorted-merge cmp (cdr lstA) lstB))
           (cons (car lstB) (sorted-merge cmp lstA (cdr lstB))))]))


; The next two functions work on binary trees. So here is the record type of a
; node.

(struct node (key left right) #:transparent)

; And the empty tree is represented by the symbol 'nil


; Compute the height of a binary tree. Use this convention: The height of 'nil
; is 0.  In general, the height is the number of non-empty nodes on the longest
; path from root to bottom.
;
; Example: (bt-height (node 5 'nil (node 6 (node 7 'nil 'nil) 'nil)))
; answer: 3
(define (bt-height tree)
  (cond 
    [(equal? `nil tree) 0]
    [else (+ 1 (max (bt-height (node-left tree)) 
                    (bt-height (node-right tree))))]))


; Take a comparator and a binary tree. Determine whether the tree is a binary
; search tree according to the comparator.
;
; Example: (bst? <= (node 5 'nil (node 6 (node 7 'nil 'nil) 'nil)))
; answer: #f
; Example: (bst? <= (node 5 'nil (node 7 (node 6 'nil 'nil) 'nil)))
; answer: #t
; Example: (bst? >= (node 5 'nil (node 7 (node 6 'nil 'nil) 'nil)))
; answer: #f
(define (bst? cmp tree)
    (bst-hlp cmp tree +inf.0 -inf.0))

(define (bst-hlp cmp tree mx mn)
    (match tree
        [`nil #t]
        [_  (and (and (cmp mn (node-key tree)) 
                      (cmp (node-key tree) mx))
                 (and (bst-hlp cmp (node-left tree) 
                               (node-key tree) mn)
                      (bst-hlp cmp (node-right tree) 
                               mx (node-key tree))))]))
