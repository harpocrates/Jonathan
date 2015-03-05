#lang racket
(require test-engine/racket-tests "symbolic-manipulation-program.rkt")

;; Boolean algebra - problems sampled from 'Di screte Mathemtics with Applications' [Epp] 7th ed.
(define (bool-simplify expression)
  (manipulate boolean-algebra expression simplify 50))

(check-expect (bool-simplify '(v (^ (v a b) (^ (~ b) c)) c))                   'c)
(check-expect (bool-simplify '(v (~ (v p (~ q))) (^ (~ p) (~ q))))             '(~ p))
(check-expect (bool-simplify '(v (^ p (~ q)) p))                               'p)
(check-expect (bool-simplify '(^ (v (~ a) (~ b)) b))                           '(^ b (~ a)))
(check-expect (bool-simplify '(v (~ (v (^ (~ p) q) (^ (~ p) (~ q)))) (^ p q))) 'p)
(check-expect (bool-simplify '(v (^ p q) (v (~ p) (^ p (~ q)))))               #t)
(check-expect (bool-simplify '(^ (~ (^ (~ p) q)) (v p q)))                     'p)
(check-expect (bool-simplify '(v (^ p (~ (v (~ p) q))) (^ p q)))               'p)

(test)