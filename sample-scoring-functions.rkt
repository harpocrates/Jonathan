#lang racket

(provide simplify number-simplify
         expand
         number-differentiate)

;; Scoring function meant to minimize size and depth of expression
(define (simplify target)
  (if (list? target)
      (+ 1 (apply + (map simplify target)))
      1))

;; Scoring function meant to maximize size and depth of expression
(define (expand target)
  (- (simplify target)))

;; Scoring function meant to minimize size and depth of expression and encourage presence of
;; numbers over symbols.
(define (number-simplify target)
  (cond [(list? target) (+ 1 (apply + (map number-simplify target)))]
        [(number? target) 1]
        [else 2]))


;; Returns a scoring function that is greedy: it will make a function that
(define (number-differentiate target)
  (cond [(list? target) (* (if (equal? (first target) 'd/dx) 20 1) 
                           (apply + (map number-differentiate target)))]
        [(number? target) 1]
        [else 2]))