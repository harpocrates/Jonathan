#lang racket
(require test-engine/racket-tests
         "symbolic-manipulation-program.rkt"
         "sample-transform-rules.rkt"
         "sample-scoring-functions.rkt"
         "differentiation.rkt")

;; Boolean algebra - problems sampled from 'Discrete Mathemtics with Applications' [Epp] 7th ed.

; prefix
(define (simplify-boolean-expression expression)
  (manipulate boolean expression simplify 50))

(check-expect (simplify-boolean-expression '(v (^ (v a b) (^ (~ b) c)) c))        'c)
(check-expect (simplify-boolean-expression '(v (~ (v p (~ q))) (^ (~ p) (~ q))))  '(~ p))
(check-expect (simplify-boolean-expression '(v (^ p (~ q)) p))                    'p)
(check-expect (simplify-boolean-expression '(v (^ p (~ (v (~ p) q))) (^ p q)))    'p)

; postfix
(define (simplify-boolean-expression-infix expression)
  (manipulate boolean-infix expression simplify 50))

(check-expect (simplify-boolean-expression-infix '((~ (((~ p) ^ q) v ((~ p) ^ (~ q)))) v (p ^ q))) 'p)
(check-expect (simplify-boolean-expression-infix '(((~ a) v (~ b)) ^ b))                '(b ^ (~ a)))
(check-expect (simplify-boolean-expression-infix '((p ^ q) v ((~ p) v (p ^ (~ q)))))    #t)
(check-expect (simplify-boolean-expression-infix '((~ ((~ p) ^ q)) ^ (p v q)))          'p)

;; General number simplification.
(define (transform-function expression)
  (if (and (pair? expression)
           (= (length expression) 3)
           (member (first expression) '(+ * / -))
           (number? (second expression))
           (number? (third expression)))
      (list (eval expression (make-base-namespace)))
      '()))

(define (num-simplify expression)
  (manipulate
    numbers
    expression
    number-simplify
    100
    (list transform-function)))

; (num-simplify '(+ (* a (+ 0 a)) (+ a a)))

(differentiate '(+ (* 6 x) (^ x 8)) 'x)
(num-simplify (differentiate '(+ (* 6 x) (^ x 8)) 'x))

(differentiate '(^ e x) 'x)
(num-simplify (differentiate '(^ e x) 'x))

; (differentiate '(y (* 2 x) (^ e x) 9) 'x)
; (differentiate '(y (* 2 x)) 'x)
(differentiate (differentiate '(y (* 2 x)) 'x) 'x)
(num-simplify (differentiate (differentiate '(y (* 2 x)) 'x) 'x))
(test)