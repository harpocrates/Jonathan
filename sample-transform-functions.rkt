#lang racket

(provide numbers-evaluate differentiate)

;; General number evaluation.
(define (numbers-evaluate expression)
  (if (or
        (and (pair? expression)
             (= (length expression) 3)
             (member (first expression) '(+ *))
             (number? (second expression))
             (number? (third expression)))
        (and (pair? expression)
             (= (length expression) 2)
             (equal? (first expression) '-)
             (number? (second expression))))
      (list (eval expression (make-base-namespace)))
      '()))

;; Differentiation rules. Allow treatment of two special cases
;;  * `(d/dx a)` where `a` is a constant
;;  * `(d/dx (f a))` where `f` is a function (not otherwise matched)
(define (differentiate expression)
  (match expression
    [(list 'd/dx (list f a))
      (if (member f '(- 1/ log sin cos tan asin acos atan)) '() `((* (D (,f ,a)) (d/dx ,a))))]
    [(list 'd/dx a) (if (or (equal? a 'x) (list? a)) '() '(0))]
    [_ '()]))