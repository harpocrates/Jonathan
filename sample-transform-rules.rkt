#lang racket

(provide group-algebra
         boolean boolean-infix boolean-derived-operations
         numbers)

;; Group algebra rules. Since only one operation is defined, we can omit the `*`.
(define group-algebra 
  '(
  (a b)     (a b)         (b a)        ; commutativity
  (a b c)   ((a b) c)     (a (b c))    ; associativity
  (a b c)   (a (b c))     ((a b) c)    ; associativity
  (a)       (1 a)         a            ; identity
  (a)       ((~ a) a)     1            ; inverse element
  (a b)     (~ (a b))     ((~b) (~ a)) ;
  (a b)     ((~b) (~ a))  (~ (a b))    ;
  (a)       (~ (~ a))     a            ; double negative
  )
)

;; Boolean algebra rules. Since the manipulation is purely symbolic, the expressions do not have to
;; be valid racket forms - allowing us to define the infix version of these rules.
(define boolean
  '(
  ; associativity
  (a b c) (^ a (^ b c))        (^ (^ a b) c)
  (a b c) (^ (^ a b) c)        (^ a (^ b c))
  (a b c) (v a (v b c))        (v (v a b) c)
  (a b c) (v (v a b) c)        (v a (v b c))
  ; commutativity
  (a b)   (^ a b)              (^ b a)
  (a b)   (v a b)              (v b a)
  ; distributivity
  (a b c) (^ a (v b c))        (v (^ a b) (^ a c))
  (a b c) (^ (v a b) (v a c))  (v a (^ b c))
  (a b c) (v a (^ b c))        (^ (v a b) (v a c))
  (a b c) (v (^ a b) (^ a c))  (^ a (v b c))
  ; identity
  (a)     (v a #f)             a
  (a)     (^ a #t)             a
  ; annihilator
  (a)     (v a #t)             #t
  (a)     (^ a #f)             #f
  ; idempotence
  (a)     (v a a)              a
  (a)     (^ a a)              a
  ; absorption
  (a b)   (^ a (v a b))        a
  (a b)   (v a (^ a b))        a
  ; complement
  (a)     (v a (~ a))          #t
  (a)     (^ a (~ a))          #f
  ; double negative
  (a)     (~ (~ a))            a
  ; deMorgan's
  (a b)   (v (~ a) (~ b))      (~ (^ a b))
  (a b)   (~ (v a b))          (^ (~ a) (~ b))
  (a b)   (^ (~ a) (~ b))      (~ (v a b))
  (a b)   (~ (^ a b))          (v (~ a) (~ b))
  ; negations of #f and #t
  ()      (~ #f)               #t
  ()      (~ #t)               #f
  )
)
(define boolean-infix 
  '(
  ; associativity
  (a b c) (a ^ (b ^ c))        ((a ^ b) ^ c)
  (a b c) ((a ^ b) ^ c)        (a ^ (b ^ c))
  (a b c) (a v (b v c))        ((a v b) v c)
  (a b c) ((a v b) v c)        (a v (b v c))
  ; commutativity
  (a b)   (a ^ b)              (b ^ a)
  (a b)   (a v b)              (b v a)
  ; distributivity
  (a b c) (a ^ (b v c))        ((a ^ b) v (a ^ c))
  (a b c) ((a v b) ^ (a v c))  (a v (b ^ c))
  (a b c) (a v (b ^ c))        ((a v b) ^ (a v c))
  (a b c) ((a ^ b) v (a ^ c))  (a ^ (b v c))
  ; identity
  (a)     (a v #f)             a
  (a)     (a ^ #t)             a
  ; annihilator
  (a)     (a v #t)             #t
  (a)     (a ^ #f)             #f
  ; idempotence
  (a)     (a v a)              a
  (a)     (a ^ a)              a
  ; absorption
  (a b)   (a ^ (a v b))        a
  (a b)   (a v (a ^ b))        a
  ; complement
  (a)     (a v (~ a))          #t
  (a)     (a ^ (~ a))          #f
  ; double negative
  (a)     (~ (~ a))            a
  ; deMorgan's
  (a b)   ((~ a) v (~ b))      (~ (a ^ b))
  (a b)   (~ (a v b))          ((~ a) ^ (~ b))
  (a b)   (^ (~ a) (~ b))      (~ (a v b))
  (a b)   (~ (a ^ b))          ((~ a) v (~ b))
  ; negations of #f and #t
  ()      (~ #f)               #t
  ()      (~ #t)               #f
  )
)
(define boolean-derived-operations 
  '(
  (a b)   (-> a b)        (v (~ a) b)              ; conditional
  (a b)   (<-> a b)       (^ (v a b) (~ (^ a b)))  ; biconditional
  (a b)   (+ a b)         (v (v a b) c)            ; exclusive or
  )
)

;; General numeric algebra rules. Ultimately to be used in conjunction with symbolic
;; differentiation. (To include simplifications concerning logarithms and exponents.)
(define numbers 
  '(
  ;; Multiplication and Addition
  ; associativity
  (a b c) (* a (* b c))        (* (* a b) c)
  (a b c) (* (* a b) c)        (* a (* b c))
  (a b c) (+ a (+ b c))        (+ (+ a b) c)
  (a b c) (+ (+ a b) c)        (+ a (+ b c))
  ; commutativity
  (a b)   (* a b)              (* b a)
  (a b)   (+ a b)              (+ b a)
  ; distributivity
  (a b c) (* a (+ b c))        (+ (* a b) (* a c))
  (a b c) (* (+ a b) (+ a c))  (+ a (* b c))
  ; (heuristics)
  (a)       (+ a a)            (* 2 a)
  (a b)     (+ a (* b a))      (* a (+ b 1))
  ; identity
  (a)       (+ a 0)            a
  (a)       (* a 1)            a
  (a)       (* a 0)            0

  ;; Division
  (a b c)   (* a (/ b c))      (* b (/ a c))
  (a b c)   (* a (/ b c))      (/ (* a b) c)
  (a b c)   (/ (* a b) c)      (* a (/ b c))
  (a b c)   (/ a (/ b c))      (/ (* a c) b)
  (a b c)   (/ (* a c) b)      (/ a (/ b c))
  ; identity
  (a)       (/ 0 a)            0
  (a)       (/ a 1)            a
  (a)       (/ a a)            1

  ;; Logarithm and Exponentiation
  ; exponents
  (a b c)   (* (^ a b) (^ a c)) (^ a (+ b c))
  (a b c)   (^ (^ a b) c)       (^ a (* b c))
  (a b c)   (* (^ a c) (^ b c)) (^ (* a b) c)
  ; logarithms
  (a b)     (+ (log a) (log b)) (log (* a b))
  (a b)     (- (log a) (log b)) (log (/ a b))
  (a b)     (log a b)           (/ (log a) (log b))
  (a b)     (/ (log a) (log b)) (log a b)
  (a b)     (log (^ a b))       (* b (log a))
  (a b)     (* b (log a))       (log (^ a b))
  ; (heuristics)
  (a b)     (* a (^ a b))       (^ a (+ 1 b))
  (a b)     (/ (^ a b) a)       (^ a (- b 1))
  ; identity
  ()        (log e)            1
  ()        (log 1)            0
  )
)