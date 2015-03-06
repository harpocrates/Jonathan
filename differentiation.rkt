#lang racket

(provide differentiate)

;; Given an expression, we differentiate symbolically that expression with respect to the given
;; variable. If `dependent-variables` is provided, we ensure to not treat them as constants, but as
;; functions of the `variable`.
;;
;; The notation used is `(D n (y x1 x2 ... xn))` where `(y x1 x2 ... xn)` is the initial function
;; and `n` is which variable the derivative is taken. If there is only one variable, we can use
;; `(D (y x))` instead.
(define (differentiate expression variable [dependent-variables '()])
  (local [(define (differentiate expression)
            (match expression
            ; combined functions
              ; sum rule
              [(list '+ a b) `(+ ,(differentiate a) ,(differentiate b))]
              ; product rule
              [(list '* a b) `(+ (* ,(differentiate a) ,b) (* ,a ,(differentiate b)))]
              ; quotient rule
              [(list '/ a b) `(/ (- (* ,(differentiate a) ,b) (* ,a ,(differentiate b))) (^ ,b 2))]
            ; exponential and logarithmic
              [(list '^ a b) `(* (^ ,a ,b)
                                 (+ (* ,(differentiate a) (/ ,b ,a))
                                    (* ,(differentiate b) (log ,a))))]
              [(list 'log a) `(/ ,(differentiate a) ,a)]
              [(list 'log b a) (differentiate `(/ (log ,a) (log ,b)))]
            ; trigonometric
              [(list 'sin a) `(* (cos ,a)     ,(differentiate a))]
              [(list 'cos a) `(* (- (sin ,a)) ,(differentiate a))]
              [(list 'tan a) `(/ ,(differentiate a) (^ (cos ,a) 2))]
            ; inverse trigonometric
              [(list 'asin a) `(/ ,(differentiate a)     (^ (+ 1 - (^ ,a 2)) 0.5))]
              [(list 'acos a) `(/ (- ,(differentiate a)) (^ (+ 1 - (^ ,a 2)) 0.5))]
              [(list 'atan a) `(/ ,(differentiate a)     (+ 1 (^ ,a 2)))]
            ; single-variable function
              [(list 'D f)  `(D ,(differentiate f))]
              [(list f a)  `(* (D ,(list f a)) ,(differentiate a))] ; chain rule
            ; general functions
              [(list 'D n f)  `(D ,n ,(differentiate f))]           ; chain rule
              [(list f a ...)
                (let loop ([n 2]
                           [args (rest a)]
                           [result `(* (D 1 ,(cons f a)) ,(differentiate (first a)))])
                  (if (empty? args)
                      result
                      (loop
                        (+ 1 n)
                        (rest args)
                        `(+ ,result (* (D ,n ,(cons f a)) ,(differentiate (first args)))))))]
            ; atoms
              [a (cond [(equal? a variable) 1]
                       [(member a dependent-variables) `(D ,a)]
                       [else 0])]))]
    (differentiate expression)))