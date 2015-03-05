# Jonathan
Jonathan is a symbolic manipulation program. Here is a rough idea of how he works.

  * Jonathan begins by accepting the following input:
      1. A list of transform rules. For example, a valid transform rule for associativity of `and` is
       
       ```racket
       (a b c)   (and a (and b c))   (and (and a b) c)
       ```
      since we can replace occurences of `(and a (and b c))` with `(and (and a b) c)` (where the variables
      to bind are `a`, `b`, and `c`).
      
      2. A source expression. This is the expression that Jonathan will manipulate.
      
      3. A scoring function. This is a function that Jonathan will attempt to minimize. If our objective is
      to reduce the given source expression to its shortest form, one such function could be defined as
      
      ```racket
      (define (simplify target)
        (if (list? target)
            (+ 1 (apply + (map simplify target)))
            1))
      ```
      
      which essentially penalizes every extra symbol, as well as extra parentheses.
  * Using these inputs, Jonathan checks available mutations of promising expressions, prioritizing expressions
    with already low scores. This is done by way of a priority queue. Expressions that are visited are added to
    a hash set, so as to avoid revisiting them multiple times.

### Example
Using the preset `boolean-algebra` transform rules defined and the `simplify` scoring function above, we can
simplify boolean algebra expressions.

```racket
(define (bool-simplify expression)
  (manipulate boolean-algebra expression simplify 50))
  
(bool-simplify '(v (^ p (~ (v (~ p) q))) (^ p q)))               ; ===> 'p
(bool-simplify '(v (~ (v (^ (~ p) q) (^ (~ p) (~ q)))) (^ p q))) ; ===> 'p
(bool-simplify '(v (^ p q) (v (~ p) (^ p (~ q)))))               ; ===> #t
```

