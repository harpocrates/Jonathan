#lang racket
(require test-engine/racket-tests
         "symbolic-manipulation-program.rkt"
         (prefix-in TR: "sample-transform-rules.rkt")
         (prefix-in SF: "sample-scoring-functions.rkt")
         (prefix-in TF: "sample-transform-functions.rkt")
         ; "differentiation.rkt"
         )

;; Boolean algebra - problems sampled from 'Discrete Mathemtics with Applications' [Epp] 7th ed.

; prefix
(define (simplify-boolean-expression expression)
  (manipulate TR:boolean expression SF:simplify 50))

(check-expect (simplify-boolean-expression '(v (^ (v a b) (^ (~ b) c)) c))        'c)
(check-expect (simplify-boolean-expression '(v (~ (v p (~ q))) (^ (~ p) (~ q))))  '(~ p))
(check-expect (simplify-boolean-expression '(v (^ p (~ q)) p))                    'p)
(check-expect (simplify-boolean-expression '(v (^ p (~ (v (~ p) q))) (^ p q)))    'p)

; postfix
(define (simplify-boolean-expression-infix expression)
  (manipulate TR:boolean-infix expression SF:simplify 50))

(check-expect (simplify-boolean-expression-infix '((~ (((~ p) ^ q) v ((~ p) ^ (~ q)))) v (p ^ q))) 'p)
(check-expect (simplify-boolean-expression-infix '(((~ a) v (~ b)) ^ b))                '(b ^ (~ a)))
(check-expect (simplify-boolean-expression-infix '((p ^ q) v ((~ p) v (p ^ (~ q)))))    #t)
(check-expect (simplify-boolean-expression-infix '((~ ((~ p) ^ q)) ^ (p v q)))          'p)

;; General number manipulation.

; simplify expressions
(define (simplify-algebraic-expression expression)
  (manipulate TR:numbers expression SF:number-simplify 150 (list TF:numbers-evaluate)))

(define simplify simplify-algebraic-expression)
(check-expect (simplify '(+ (* 6 a) (+ a (+ (* (log 1) x) (* 5 a)))))                    '(* a 12))
(check-expect (simplify '(+ (* (^ (+ a (- b)) 5) (1/ (^ (+ a (- b)) 4))) (+ b (* 0 a)))) 'a)
(check-expect (simplify '(+ (+ (* 0 x) (* 6 1)) (* (^ x 8) (* 8 (1/ x)))))               '(+ 6 (* 8 (^ x 7))))

; differentiate, then simplify expressions
(define (differentiate-algebraic-expression expression)
  (simplify-algebraic-expression
    (manipulate
      (append TR:differentiate TR:numbers-identities)
      `(d/dx ,expression)
      SF:number-differentiate
      200
      (list TF:differentiate))))

(define d/dx differentiate-algebraic-expression)
(check-expect (d/dx '(^ e (^ x 2)))                   '(* (^ e (^ x 2)) (* 2 x)))
(check-expect (d/dx '(^ x 7))                         '(* 7 (^ x 6)))
(check-expect (d/dx '(+ (* 4 (^ e x)) (- (^ x 1/2)))) '(+ (* 4 (^ e x)) (* -1/2 (^ x -1/2))))
(check-expect (d/dx '(^ x x))                         '(* (^ x x) (+ 1 (log x))))
(check-expect (d/dx '(* x (sin (log (* 5 x)))))       '(+ (sin (log (* 5 x))) (cos (log (* 5 x)))))
(check-expect (d/dx '(log (log (log x))))             '(1/ (* (* x (log x)) (log (log x)))))
(check-expect (d/dx '(* (y x) (sin (^ (y x) 4))))
              '(+ (* (sin (^ (y x) 4)) (D (y x))) (* (^ (y x) 4) (* (cos (^ (y x) 4)) (* 4 (D (y x)))))))

(test)