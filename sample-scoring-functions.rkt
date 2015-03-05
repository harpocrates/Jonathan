#lang racket

(provide simplify number-simplify
         expand)

;; Scoring function meant to minimize size and depth of expression
(define (simplify target)
  (if (list? target)
      (+ 1 (apply + (map simplify target)))
      1))
;; Scoring function meant to minimize size and depth of expression
(define (number-simplify target)
  (cond [(list? target) (+ 1 (apply + (map number-simplify target)))]
        [(number? target) 1]
        [else 2]))


;; Scoring function meant to maximize size and depth of expression
(define (expand target)
  (- (simplify target)))