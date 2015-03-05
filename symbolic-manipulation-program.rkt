#lang racket
(require data/heap
         racket/set)

(provide next-expressions manipulate)

;; Given a template, we check if target fits this template.
;; `variables` is a list of symbols in the `template` expression that are variables (hence do not
;; need to be matched exactly, just substituted). `dict` is a dictionary of values of variables
;; that have already been set.
(define (match-template variables dict template target)

  (cond [(and (not (list? template)) (member template variables))
         ; if the `template` is a variable, we check to see if it is previously defined
         ;   * if it is, we check that `target` matches this definition (returning #f otherwise)
         ;   * if it isn't, we add this definition and return the modified dictionary
          (if (dict-has-key? dict template)
              (if (equal? (dict-ref dict template) target) dict #f)
              (begin (dict-set! dict template target) dict))]

        [(and (not (list? template)) (not (list? target)) (equal? template target))
        ; if both the `template` and `target` are atoms (and the `template` is not a variable), we
        ; need the `template` and `target` to be the same
          dict]

        [(and (pair? template) (pair? target) (equal? (length template) (length target)))
        ; if both the `template` and `target` are lists and are the same length, we compare
        ; element-wise, passing the dictionary by reference through the recursive calls.
          (local [(define result 0)]
            (for ([i template]
                  [j target])
              (when
                (not (boolean? result))
                (set! result (match-template variables dict i j))))
            (if (boolean? result) #f dict))]
        [else #f]))

;; Given a template, we expand it out.
;; `variables` is a list of symbols in the `template` expression that are variables (hence need to
;; be substituted). `dict` is a dictionary of the values of the variables.
(define (splice-onto-template variables dict template)
  (cond [(not (list? template))
          ; if the `template` is an atom it is either
          ;  * a variable (we splice in its correct definition)
          ;  * or a symbol (we do nothing)
          (if (member template variables)
            (dict-ref dict template)
            template)]

        [(pair? template)
        ; if both `template` is a list, we sub in element-wise, passing the dictionary by reference
        ; through the recursive calls.
          (map (lambda (x) (splice-onto-template variables dict x)) template)]))

;; Given a set of transform rules (`transform-templates`) and a target expression (`target`),
;; generate the possible derived expressions. The optional argument `toplevel` provides the
;; option of matching the transform rules only at the top level (think about how equivalence vs.
;; inference rules are applied).
;;
;; `transform-functions` provides a way of extending the matching engine. These functions must
;; consume an expression and return a list of possible mutated expressions.
(define (next-expressions transform-templates target [toplevel #f] [transform-functions '()])
  (local [(define (next-expressions-toplevel transform-templates target)
          (append
            ; performs toplevel replacements based on `transform-functions`
            (apply append (map (lambda (f) (f target)) transform-functions))
            ; performs toplevel replacements based on `transform-templates`.
            ; Ex: Given only a commutativity rule (a b) :: (a b) -> (b a)
            ;     (a (b c)) produces ((b c) a) but not (a (c b)).
            (let loop ([transform-templates transform-templates])
              (if (empty? transform-templates)
                  '()
                  (local [(define variables (first transform-templates))
                          (define template  (second transform-templates))
                          (define remaining-templates
                            (rest (rest (rest transform-templates))))
                          (define try
                            (match-template variables (make-hash) template target))]
                    (if (boolean? try)
                      (loop remaining-templates)
                      (cons
                        (splice-onto-template
                              variables
                              try
                              (third transform-templates))
                        (loop remaining-templates))))))))
            (define (next-expressions-non-toplevel transform-templates target)
          ; performs all non-toplevel replacements based on transform-templates
          ; Ex: Given only a commutativity rule (a b) :: (a b) -> (b a)
          ;     ((a (b c)) d) produces (((b c) a) d) and ((a (c b)) d) but not (d (a (b c))).
            (if (empty? target)
                '()
                (append
                  (map
                    (lambda (x) (cons x  (rest target)))
                    (next-expressions transform-templates (first target) #f transform-functions))
                  (map
                    (lambda (x) (cons (first target) x))
                    (next-expressions-non-toplevel transform-templates (rest target))))))]
    (if (or (not (list? target)) toplevel)
        ; If the target is an atom (or we want only toplevel replacements), we cannot recursively
        ; descend any further. Otherwise, we must consider non-toplevel replacements too.
        (next-expressions-toplevel transform-templates target)
        (append
          (next-expressions-toplevel     transform-templates target)
          (next-expressions-non-toplevel transform-templates target)))))


;; Scoring function meant to minimize size and depth of expression
(define (simplify target)
  (if (list? target)
      (+ 1 (apply + (map simplify target)))
      1))

;; Applies the transformation rules (`transform-rules`) to a `source` expression.
;;   * `scoring`:              returns a number given an expression. `manipulate` minimizes this;
;;   * `cutoff`:               number representing the maximum number of iterations;
;;   * `transform-functions`:  list of functions that complement transform-rules. Each function
;;                             in the list consumes an expression and outputs a list of mutants.
;;   * `toplevel`:             flag allowing transform-rules to be applied only at the top level.
(define (manipulate transform-rules source scoring cutoff [transform-functions '()] [toplevel #f] [print #f])
  (local [(define visited  (set source)) ; keeps track of expressions already processed (set)
          (define to-visit               ; track of expressions to be processed (priority queue)
            (make-heap (lambda (x y) (<= (second x) (second y)))))
          (define best source)                   ; keeps track of best expression seen
          (define best-score (scoring source))   ; keeps track of the best expression's score
          (define o (current-output-port))]

    (heap-add! to-visit (list source (scoring source))) ; add the source point
    (when print (fprintf o "Starting...\nSource:   [~a]   ~a\n" best-score best))

    ; loop until `cutoff` is reached or there are no more expressions waiting to be processed.
    ;   * pop off the lowest score expression in the priority queue
    ;   * if it has lower score than the current best, update `best` and `best-score`
    ;   * get `next-expressions` from popped element (filtering out those already visited)
    ;   * add these expressions to both the `visited` and `to-visit` structures
    (let loop ([n cutoff])
      (if (or (equal? 0 (heap-count to-visit)) (equal? n 0))

          ; loop exit:
          (begin
            (when print (fprintf o (if (= n 0) "Cutoff reached.\n" "Cases exhausted.\n")))
            best)

          (local [(define top         ; get minimum element from `to-visit`
                    (begin0
                      (first (heap-min to-visit))
                      (heap-remove-min! to-visit)))
                  (define next-expr   ; get new next expressions accessible from top
                    (filter
                      (lambda (e) (not (set-member? visited e)))
                      (next-expressions transform-rules top toplevel transform-functions)))]

            ; update best scores
            (when (< (scoring top) best-score)
              (set!-values (best best-score) (values top (scoring top)))
              (when print (fprintf o "New best: [~a]   ~a\n" best-score best)))

            ; add new expressions to `visited` and `to-visit`
            (for-each
              (lambda (e)
                (set! visited (set-add visited e))
                (heap-add! to-visit (list e (scoring e))))
              next-expr)

            ; loop
            (loop (- n 1)))))))