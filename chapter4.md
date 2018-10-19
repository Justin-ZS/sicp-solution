### SICP Solution Chapter 4
完成度:

### 4.1
```scheme
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-of-values-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((left  (eval (first-operand exps) env)))
        (cons left (list-of-values (rest-operands exps) env))
      )))

(define (list-of-values-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env) right)
      )))
```

### 4.2
```scheme
; a
; Louis's plan
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))
; application? before assignment?
; call (define x 3)
; (define (application? exp) (pair? exp))
; so the application? clause will be execute
; (apply (eval (operator exp) env)
;                (list-of-values (operands exp) env))
; equals
; (apply (eval define env) ...)
; (eval define env) will throw an exception

; b
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
```

### 4.3
self-evaluating和variable不是复杂表达式，不需要改变

```scheme
(define eval-type car)
(put 'quote (lambda (exp env) (text-of-quotation exp)))
(put 'set! eval-assignment)
(put 'define eval-definition)
(put 'if eval-if)
(put 'lambda (lambda (exp env)
                (make-procedure (lambda-parameters exp)
                                (lambda-body exp)
                                env)))
(put 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'cond (lambda (exp env) (eval (cond->if exp) env)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get (eval-type exp))
          ((get (eval-type exp)) exp env))
        ((application? exp)
          (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))
```

### 4.4
```scheme
(define and-clauses cdr)
(define or-clauses cdr)

(define fst-clause car)
(define rest-clauses cdr)
(define empty-clauses? null?)

(put 'and (lambda (exp env) (eval-and (and-clauses exp) env '#t)))
(define (eval-and clauses env val)
  (if (empty-clauses? clauses)
      val
      (let ((cur (eval (fst-clause clauses) env)))
        (if (true? cur) (eval-and (rest-clauses clauses) env cur) '#f)
      )))

(put 'or (lambda (exp env) (eval-or (or-clauses exp) env)))
(define (eval-or clauses env)
  (if (empty-clauses? clauses)
      '#f
      (let ((cur (eval (fst-clause clauses) env)))
        (if (true? cur) cur (eval-or (rest-clauses clauses) env))
      )))

; derived expressions

(put 'and (lambda (exp env) (eval (and->if (and-clauses exp) '#t) env)))
(define (and->if clauses val)
  (if (empty-clauses? clauses)
    val
    (make-if (fst-clause clauses)
             (and->if (rest-clauses clauses) (fst-clause clauses))
             '#f
    )))

(put 'or (lambda (exp env) (eval (or->if (or-clauses exp)) env)))
(define (or->if clauses)
  (if (empty-clauses? clauses)
      '#f
      (make-if (fst-clause clauses)
               (fst-clause clauses)
               (or->if (rest-clauses clauses))
      )))
```


### 4.5
```scheme
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

; new added
(define (cond-arrow? list) (tagged-list? list '=>))
(define cond-arrow-recipient cadr)

(define (parse-clause clause)
  (let ((value (cond-predicate first))
        (actions (cond-actions clause)))
    (if (cond-arrow? actions)
        (list (cond-arrow-recipient actions) value) ; or ((cond-arrow-recipient actions) value)
        (sequence->exp actions)
    )))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (parse-clause first)                   ; changed!!!
                     (expand-clauses rest))
        ))))
; Why not use ((cond-arrow-recipient actions) value) to replace? 
; ((cond-arrow-recipient actions) value) will be executed during evaluating make-if syntax
; As a result, <recipient> is invoked before checking <test> in (<test> => <recipient>)
```

### 4.6
```scheme
(define let-parameters cadr)
(define let-body cddr)
(define (parameters-argus parameters) (map car parameters))
(define (parameters-values parameters) (map cadr parameters))

(define (let-combination exp)
  (let ((parameters (let-parameters exp)))
    (cons (make-lambda (parameters-argus parameters) (let-body exp))
      (parameters-values parameters))))
; test
(let-combination '(let ((a 1) (b 2)) (+ a b)))
; => ((lambda (a b) (+ a b)) 1 2)
((lambda (a b) (+ a b)) 1 2)
; => 3

(define (let? exp) (tagged-list? exp 'let))
(define (eval exp env)
  (cond ; ...
        ((let? exp) (eval (let-combination exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))
```

### 4.7
```scheme
(define first-parameter car)
(define rest-parameters cdr)
(define empty-parameters? null?)

(define (make-let parameters body) (list 'let parameters body))

(define (let*->nested-lets exp)
  (define (nest-let parameters body)
    (make-let
      (list (first-parameter parameters))
      (if (empty-parameters? (rest-parameters parameters))
          (sequence->exp body) ; body may be a sequence expression
          (nest-let (rest-parameters parameters) body)
      )))
  (nest-let (let-parameters exp) (let-body exp)))

; action
(eval (let*->nested-lets exp) env)
; derived expressions is enough

; test
(let*->nested-lets '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)))
; => (let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))

(let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))
; => 39 
```

### 4.8
```scheme
(let fib-iter ((a 1)
               (b 0)
               (count n))
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1)))
)
; equals
(let ((a 1)
      (b 0)
      (count n))
    (define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
    (fib-iter a b count))

; solution
(define (named-let? exp) (not (list? (let-parameters exp))))
(define named-let-name cadr)
(define named-let-parameters caddr)
(define named-let-body cdddr)
(define (make-define name body) (list 'define name body))

(define (let-combination exp)
  (if (named-let? exp)
      (let ((name       (named-let-name exp))
            (parameters (named-let-parameters exp))
            (body       (named-let-body exp)))
        (make-let parameters
                  (sequence->exp
                    (list (make-define name (make-lambda (parameters-argus parameters) body))
                          (cons name (parameters-argus parameters))))))
      (let ((parameters (let-parameters exp)))
        (cons (make-lambda (parameters-argus parameters) (let-body exp))
          (parameters-values parameters)))
  )
)

; test
(let-combination '(let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
; => (let ((a 1) (b 0) (count n)) (begin (define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter a b count)))
(define (fib n)
  (let ((a 1) (b 0) (count n)) (begin (define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter a b count))) ; copy previous output
)
(fib 10)
; => 55
```

### 4.9
```scheme
(define n 0)
(define sum 0)
; (while (< n 5) (set! sum (+ n sum)) (set! n (+ n 1)))

(define while-predicate cadr)
(define while-body cddr)
(define (while->recurse exp)
  (let ((predicate (while-predicate exp))
        (body      (while-body exp)))
       (make-if predicate
                (make-begin (cons (sequence->exp body) (list exp)))
                '#f
       )))

; can't add test since the loop is iterated in eval procedure
(while->recurse '(while (< n 5) (set! sum (+ n sum)) (set! n (+ n 1))))
; =>
;  (if (< n 5)
;      (begin
;        (begin (set! sum (+ n sum))
;               (set! n (+ n 1)))
;        (while (< n 5) (set! sum (+ n sum)) (set! n (+ n 1))))
;      #f)

; Another solution!!!

; while expression:(while <predicate> <body>) equals
;((lambda ()
;   (define while-iter
;     (lambda ()
;       (if <predicate>
;           (begin
;             (begin <body>)
;             (while-iter))
;           'false))
;   )
;   (while-iter)))

(define (while->combination exp)
  (let ((predicate (while-predicate exp))
        (body      (while-body exp))
        (while-iter 'while-iter))
       (list (make-lambda '()
          (list (make-define while-iter
                (make-lambda '()
                  (list (make-if predicate
                                 (make-begin (cons (sequence->exp body) (list (list while-iter))))
                                 'false))))
                (list while-iter)))
        )
  ))

; test
(while->combination '(while (< n 5) (set! sum (+ n sum)) (set! n (+ n 1))))
; => ((lambda () (define while-iter (lambda () (if (< n 5) (begin (begin (set! sum (+ n sum)) (set! n (+ n 1))) (while-iter)) false))) (while-iter)))
((lambda () (define while-iter (lambda () (if (< n 5) (begin (begin (set! sum (+ n sum)) (set! n (+ n 1))) (while-iter)) false))) (while-iter)))
; => #f
sum
; => 10
n
; => 5
```

### 4.10
```scheme
(define (definition? exp)
  (tagged-list? exp 'def))
; Change the definition? function will make below expression work
(def n 0)
```

### 4.11
```scheme
; Represent a frame as a list of bindings
(define (make-frame variables values)
  (map cons variables values))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (let ((new-frame-cdr (cons (cons var val) (cdr frame))))
      (set-cdr! frame new-frame-cdr)
  ))

; keep lookup-variable-value unchanged

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan items)
      (cond ((null? items)
             (env-loop (enclosing-environment env)))
            ((eq? var (car (car items)))
             (set-cdr! (car items) val))
            (else (scan (cdr items)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan items)
      (cond ((null? items)
             (add-binding-to-frame! var val frame))
            ((eq? var (car (car items)))
             (set-cdr! (car items) val))
            (else (scan (cdr items)))))
    (scan frame)))
```

### 4.12
我好像做复杂了，参考JS的[迭代器](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols
)多抽象了一层.  
其实直接把三个函数体内重复的部分抽象出一层就可以了
```scheme
(define (make-iterable empty? next get-value obj)
  (define (invalid-next) (error "No next iterable"))
  (define (invalid-value) (error "No value"))
  (if (empty? obj)
      (list invalid-next invalid-value (lambda () (empty? obj)))
      (let ((next-obj (next obj))
            (value    (get-value obj)))
        (define (next-iterable) (make-iterable empty? next get-value next-obj))
        (define (current-value) value)
        (define (done?) (empty? obj))
        (list next-iterable current-value done?)
      )
  )
)
(define iterable-next car)
(define iterable-value cadr)
(define iterable-done caddr)

(define (not-found? val) (eq? 'NOT-FOUND val))
(define (find-iterable pred? iterable)
  (define (iter iterable)
    (let ((done? (iterable-done iterable))
          (get-value (iterable-value iterable))
          (next (iterable-next iterable)))
      (cond ((done?) 'NOT-FOUND)
            ((pred? (get-value)) (get-value))
            (else (find-iterable pred? (next)))
      )
    ))
  (iter iterable))

; convert data to iterable
(define (make-env-iterable env)
  (define (empty? env) (eq? env the-empty-environment))
  (make-iterable empty? enclosing-environment first-frame env))

(define (make-frame-iterable frame)
  (define format cons)
  (define formated-frame (format (frame-variables frame) (frame-values frame)))
  (define (next formated) (format (cdr (car formated)) (cdr (cdr formated))))
  (define (empty? formated) (null? (car formated)))
  (define (get-value formated) formated)
  (make-iterable empty? next get-value formated-frame))
(define (frame-iterable-item-var item) (car (car item)))
(define (frame-iterable-item-val item) (car (cdr item)))

; rewrite the three procedures
(define (search-frame? var fn frame)
  (define frame-iterable (make-frame-iterable frame))
  (define (same? val) (eq? var (frame-iterable-item-var val)))
  (let ((found (find-iterable same? frame-iterable)))
    (cond ((not-found? found) '#f)
          (else (fn found) '#t)))
)

(define (lookup-variable-value var env)
  (define result 0)
  (define (set-result! v) (set! result v))
  (define env-iterable (make-env-iterable env))
  (define (pred? frame) (search-frame? var set-result! frame))
  (let ((found (find-iterable pred? env-iterable)))
    (if (not-found? found)
        (error "Unbound variable" var)
        (frame-iterable-item-val result)))
)
(define (set-variable-value! var val env)
  (define (set-val! frame) (set-car! (cdr frame) val))
  (define env-iterable (make-env-iterable env))
  (define (pred? frame) (search-frame? var set-val! frame))
  (let ((found (find-iterable pred? env-iterable)))
    (if (not-found? found)
        (error "Unbound variable -- SET!" var)))
)

(define (define-variable! var val env)
  (define (set-val! frame) (set-car! (cdr frame) val))
  (let ((frame (first-frame env)))
    (define result (search-frame? var set-val! frame))
    (if (not result)
        (add-binding-to-frame! var val frame))
  ))

; test
(define frame1 (make-frame '(a b) '(1 2)))
(define frame2 (make-frame '(a b c) '(4 5 6)))
(define env (list frame1 frame2))
; (((a b) 1 2) ((a b c) 4 5 6))

(lookup-variable-value 'c env)
; => 6
(lookup-variable-value 'b env)
; => 2
(set-variable-value! 'a 2 env)
env
; => (((a b) 2 2) ((a b c) 4 5 6))
(set-variable-value! 'c 7 env)
env
; => (((a b) 2 2) ((a b c) 4 5 7))
(define-variable! 'b 3 env)
env
; => (((a b) 2 3) ((a b c) 4 5 7))
(define-variable! 'c 4 env)
env
; => (((c a b) 4 2 3) ((a b c) 4 5 7))
```

### 4.13
```scheme
(define (make-unbound! var env) 
  (let ((frame (first-frame env)))
    (define (scan vars vals is-fst)
      (if (null? vars)
          '#f
          (let ((next-vars (cdr vars))
                (next-vals (cdr vals)))
            (cond ((and (eq? var (car vars)) is-fst)
                    (set-car! frame (cdr vars))
                    (set-cdr! frame (cdr vals))
                    '#t)
                  ((eq? var (car vars)) '#t)
                  ((scan next-vars next-vals '#f)
                    (set-cdr! vars (cdr next-vars))
                    (set-cdr! vals (cdr next-vals))
                    '#f)
                  (else '#f)
            ))))
    (scan (frame-variables frame)
          (frame-values frame)
          '#t))
)

; test
env
; => (((a b) 2 3) ((a b c) 4 5 7))
(make-unbound! 'a env)
env
; => (((b) 3) ((a b c) 4 5 7))
(make-unbound! 'c env)
env
; => (((b) 3) ((a b c) 4 5 7))

; this implementation just unbounds the variable in first frame
```

### 4.14
```scheme
; Consider calling (map + '(1 2 3) '(1 2 3))
; it work fine in native environment
(map + '(1 2 3) '(1 2 3))
; => (2 4 6)
; but if we call it in M-Eval environment
; all primitive procedures in M-Eval are tagged with 'primitive symbol
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) ; this line
       primitive-procedures))
; so the + will be parsed to something like ('primitive <native produce>).
; ('primitive <native + produce>) is not applicable !!!

; custom defined function will work fine.
```

### 4.15
这是著名的[停机问题](https://zh.wikipedia.org/wiki/%E5%81%9C%E6%9C%BA%E9%97%AE%E9%A2%98)
```scheme
; it is impossible to write a procedure halts? that correctly determines whether p halts on a for any procedure p and object a?
; the key point is `any procedure p and object a`

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))
; if we call (try try)
(try try)    ; step 1  
; equals
(if (halts? try try) ; step 2
    (run-forever)
    'halted)
; in step 2, halts? will determines (try try)
; if (halts? try try) return true, which means internal (try try) will return a value. (not run-forever)
; as a result, the outer (try try) will run forever
; it becomes a paradox
```

### 4.16
```scheme
; a
(define unassigned '*unassigned*)
(define (unassigned? val) (eq? val unassigned))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (unassigned? (car vals))
                 (error "This is a not-yet-assigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; b
(define (scan-out-defines body)
  (let ((defs           (filter definition? body))
        (set-unassigned (lambda (var) (list (definition-variable var) 'unassigned)))
        (replace        (lambda (exp) (if (definition? exp) (cons 'set! (cdr exp)) exp))))

    (define parameters (map set-unassigned defs))
    (define new-body (sequence->exp (map replace body)))

    (make-let parameters new-body)
  ))

; test
(lambda () (define a 1) (define b 2) (+ a b))
; body: '((define a 1) (define b 2) (+ a b))
(scan-out-defines '((define a 1) (define b 2) (+ a b)))
; => (let ((a unassigned) (b unassigned)) (begin (set! a 1) (set! b 2) (+ a b)))
; try it!
((lambda ()
  (let ((a unassigned) (b unassigned)) (begin (set! a 1) (set! b 2) (+ a b)))
))
; => 3

; c
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

; `make-procedure` would be called at eval. (define function)
; `procedure-body` would be called at apply. (call function)
; A function can only be defined once, but may be called many times.
```

### 4.17
Q: *Why is there an extra frame in the transformed program?*   
A: Transformed program add a `let` expression, it will create a new frame.  

Q: *Why this difference in environment structure can never make a difference in the behavior of a correct program?*  
A:  The enclosing environment of **transformed program's environment** is the environment of **sequential program**

Q: *Design a way to make the interpreter implement the ``simultaneous'' scope rule for internal definitions without constructing the extra frame.*  
A: Consider the [variables-hoisting](https://developer.mozilla.org/en-US/docs/Glossary/Hoisting) behavior in JavaScript

### 4.18
```scheme
; Q: Will this procedure work if internal definitions are scanned out as shown in this exercise?

; example in the text
(lambda <vars>
  (define u <e1>)
  (define v <e2>)
  <e3>)
; an alternative strategy
(lambda <vars>
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (let ((a <e1>)
          (b <e2>))
      (set! u a)
      (set! v b))
    <e3>))
; if v is called at <e1> and u is called at <e2>
; It doesn't work since u and v are still unassigned when <e1> and <e2> are evaluated
; In this case, <e2>: (stream-map f y) u: y

; Q: What if they are scanned out as shown in the text?
; It works.
; (delay <exp>) is syntactic sugar for (lambda () <exp>)
; as a result, evaluating (delay dy) does't led to evaluate `dy`
; and before defining `dy`, `y` has been defined.
```

### 4.19
```scheme
; I support Alyssa's viewpoint.
; Alyssa's viewpoint is similar to the behavior of `let` or `const` in JavaScript

; Q: Can you devise a way to implement internal definitions so that they behave as Eva prefers?
; No, it is too hard to implement for me.
```

### 4.20
```scheme
;;;; a

(letrec ((fact
          (lambda (n)
            (if (= n 1)
                1
                (* n (fact (- n 1)))))))
  (fact 10))
; equals
(let ((fact '*unassigned*))
  (set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))
  (fact 10)
)
; => 3628800

(define (letrec->let exp)
  (let ((parameters (let-parameters exp))
        (body       (let-body exp)))
    (define unassigned-vars
      (map (lambda (var) (list var ''*unassigned*))
           (parameters-argus parameters)))
    (define set-vars
      (map (lambda (var val) (list 'set! var val))
           (parameters-argus parameters)
           (parameters-values parameters)))
    (make-let unassigned-vars
      (make-begin (cons (sequence->exp set-vars) body)))
  ))

(define (letrec? exp) (tagged-list? exp 'letrec))

; action in eval cond:
; ((letrec? exp) (eval (letrec->let exp) env))

; test
(letrec ((fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))) (fact 10))
; => (let ((fact (quote *unassigned*))) (begin (set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))) (fact 10)))
(let ((fact (quote *unassigned*))) (begin (set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))) (fact 10)))
; => 3628800

;;;;  b
Consider the 'letrec' procedure before.
If we use 'let' to instead of inner 'define', recurse function can't work well.
```

### 4.21
```scheme
;;;; a
; normal fib
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))
  ))
(fib 10)
; new one
((lambda (n)
  ((lambda (fn) (fn fn n))
   (lambda (fn n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fn fn (- n 1)) (fn fn (- n 2))))
    )))
 ) 10)

;;;; b

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
```

### Separating Analysis from Execution  
```scheme
; Analyze flow

(eval '(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n))) env)

; d = '(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n)))
; so it equals
((analyze-definition d) env)

; in analyze-definition
; (analyze (definition-value exp)) equals
(analyze '(lambda (n) (if ...)))

; equals
(analyze-lambda '(lambda (n) ...))
; the returned function will be executed and bound to 'factorial' variable

; in analyze-lambda
; (analyze-sequence (lambda-body exp)) equals
(analyze-if '(if (= n 1)
                 1
                 (* (factorial (- n 1)) n)))
; the returned function will become the body of 'factorial'

; in analyze-if
; (analyze (if-alternative exp))
(analyze-application '(* (factorial (- n 1)) n))
; the returned function will be stored in 'aproc'

; in analyze-application
; (map analyze (operands exp)) contains
(analyze-application '(factorial (- n 1)))

; in analyze-application
; (analyze (operator exp)) equals
(analyze-variable ‘factorial)

; equals
(lambda (env) (lookup-variable-value exp env))
```
Then, the value of `factorial` is an *analyzed* function.
```scheme
; Evaluation flow

(eval '(factorial 2) env)

; equals
((analyze-application '(factorial 2)) env)

; equals
(execute-application <analyzed factorial> '(2))

; equals
((procedure-body <analyzed factorial>)
  (extend-environment (procedure-parameters <analyzed factorial>)
                      '(2)
                      (procedure-environment <analyzed factorial>)))

; analyzed-body = (procedure-body <analyzed factorial>)
; env2 = the extended environment
; so it equals
(analyzed-body env2)

; What is the analyzed-body?
(analyze-if '(if (= n 1)
                 1
                 (* (factorial (- n 1)) n)))

; so (analyzed-body env2) equals
(if (true? (pproc env)) ; (= n 0)
    (cproc env)   ; 1
    (aproc env)) ; (* (factorial (- n 1)) n)

; n is 2, so it equals
(aproc env2)

; What is the aproc?
(analyze-application '(* (factorial (- n 1)) n))

; so it equals
((analyze-application '(* (factorial (- n 1)) n)) env2)

; begin from ((analyze-application '(factorial 2)) env)
; end with ((analyze-application '(* (factorial (- n 1)) n)) env2)

; Everything is clear, it become a recursion
; No analyze during the evaluation stage!
```


### 4.22
```scheme
(define (analyze-let exp) (analyze (let-combination exp)))
```

### 4.23
```scheme
; In Alyssa's version, the procs are traversed at evaluation time.
; In the original version, the procs are traversed at analyze time.
```

### 4.25
```scheme
; applicative-order
(factorial 5)
(* 5 (factorial 4))
(* 5 (* 4 (factorial 3)))
(* 5 (* 4 (* 3 (factorial 2))))
(* 5 (* 4 (* 3 (* 2 (factorial 1)))))
(* 5 (* 4 (* 3 (* 2 (* 1 (factorial 0)))))) ; !!!
; Can't stop the recursion
...
; Error: maximum recursion depth exceeded

; normal-order
(factorial 5)
(* 5 (factorial (- 5 1)))
(* 5 (* (- 5 1) (factorial (- (- 5 1) 1))))
... 
(* 5 (* (- 5 1) (* (- (- 5 1) 1) (* (- (- (- 5 1) 1) 1) (factorial (- (- (- (- 5 1) 1) 1) 1))))))
(* 5 (* (- 5 1) (* (- (- 5 1) 1) (* (- (- (- 5 1) 1) 1) 1))))
(* 5 (* (- 5 1) (* (- (- 5 1) 1) 2)))
(* 5 (* (- 5 1) (* 3 2)))
...
; Everything is fine!
```

### 4.26
```scheme
(define unless-cond car)
(define unless-usual cadr)
(define unless-except caddr)
(define (unless->if exp)
  (make-if (unless-cond exp)
           (unless-except exp)
           (unless-usual exp)
  ))

; if 'unless' is a procedure rather than a syntax, it can be used as an expression.
```

### 4.27
```scheme
; There is only one 'delay-it' in eval-apply loop, that is in applying compound-procedure

(id 10) ; => call `id`, count + 1
(id (id 10)) ; => the inner (id 10) will become a thunk, call outer `id`, count + 1
; so the value of 'w' is a thunk of (id 10)

(define w (id (id 10)))
;;; L-Eval input:
count
;;; L-Eval value:
1 ; comes from calling outer 'id'.
;;; L-Eval input:
w ; it force the thunk of (id 10), count + 1, return 10
;;; L-Eval value:
10
;;; L-Eval input:
count
;;; L-Eval value:
2
```

### 4.28
```scheme
; If `Eval` doesn't force the operator, the operator may be a thunk and passed into `apply`.
; so `apply` will throw an error since a thunk is neither a primitive-procedure nor compound-procedure.

; example
(define (compose f g) (lambda (x) (f (g n))))
; if 'f' or 'g' is a thunk, an error will occur when call the composed procedure
```


### 4.29
```scheme
; consider Fibonacci number
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))
  ))
; in this procedure, (fib n) will create a thunk of n
; it will speed up calculating the next number

; no memoize
(define (square x)
  (* x x))
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
2

; with memoize
(define (square x)
  (* x x))
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
1 ; difference
```

### 4.30
```scheme
;;;; a 

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

; <lazy-lambda>: (delay-it (lambda (x) (newline) (display x) env)
; <lazy-list>: (delay-it (list 57 321 88) env)
; equals
(if (null? <lazy-list>) ; forced due to 'null?'
    'done
    (begin (<lazy-lambda> (car <lazy-list>))
           (for-each <lazy-lambda> (cdr <lazy-list>))))

; list is not null
; ((begin? exp) (eval-sequence (begin-actions exp) env))
(eval-sequence ((<lazy-lambda> (car <lazy-list>)) (for-each <lazy-lambda> (cdr <lazy-list>)) env)
; in original `eval-sequence`, (eval (first-exp exps) env)
(eval (<lazy-lambda> (car <lazy-list>)) env)

; force <lazy-lambda> due to application operator
; <lazy-car>: (delay-it (car <lazy-list>) env)
(display <lazy-car>) ; forced by primitive procedue: display

; <lazy-list> will be forced too because `force-it` is a recurse procedure
(actual-value (car <lazy-list>) env)

(car (list 57 321 88))
; => 55

; Q: Why the `for-each` example works?
; A: It use `display` to show number. This primitive procedure will force the given thunk.


;;;; b

; with original eval-sequence
(p1 1)
; => (1 2)
(p2 1)
; => 1
; the inner thunk won't be evaluated without 'force'.

; with Cy's eval-sequence
(p1 1)
; => (1 2)
(p2 1)
; => (1 2)


;;;; c
; 'force' will return the original input if it isn't a thunk 

;;;; d
; Cy's solution is better since it can avoid some strange error like the example in part b.
```

### 4.31
```scheme
(define (lazy? x) (eq x 'lazy))
(define (lazy-memo? x) (eq x 'lazy-memo))

; need more augument to toggle lazy/memorized-lazy
(define (delay-it exp lazy env)
  (cond ((lazy? lazy) (list 'thunk exp env))
        ((lazy-memo? lazy) (list 'memo-thunk exp env))
        (else exp)
  ))
; still delay expression in `list-of-delayed-args`
(define (list-of-delayed-args exps lazys env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) (car lazys) env)
            (list-of-delayed-args (rest-operands exps)
                                  (cdr lazys)
                                  env))))

; extend 'force-it' to handle lazy/memorized-lazy
(define (memo-thunk? obj)
  (tagged-list? obj 'memo-thunk))
(define (force-it obj)
  (cond ((memo-thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((thunk? obj) (actual-value (thunk-exp obj) (thunk-env obj)))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

; get clean parameters without lazy flag
(define (procedure-parameters p)
  ((lambda (x) (if (symbol? x) x (car x))) (cadr p)))

; get lazy flags
(define (procedure-lazys p)
  ((lambda (x) (if (symbol? x) 'normal (cadr x))) (cadr p)))

; in apply
; change (list-of-delayed-args arguments env) to
(list-of-delayed-args arguments (procedure-lazys procedure) env)
```

### 4.33
```scheme
(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (if (pair? text)
        (eval (convert-list text) env)
        text
    )
  ))

; first version
(define (convert-list xs)
  (if (null? xs)
      '()
      (cons (underlaying-car xs) (convert-list (underlaying-cdr xs)))
  ))

; I suddenly realize that the cons, car and cdr in `eval` are primitive procedure.

; the second version
(define (convert-list xs)
  (if (null? xs)
      '()
      (list 'cons (car xs) (convert-list (cdr xs)))
  ))

; test
(convert-list '(1 2 3))
; => (cons 1 (cons 2 (cons 3 ())))
(cons 1 (cons 2 (cons 3 ())))
; => (1 2 3)
```

### 4.35
```scheme
(define (an-integer-between low high)
  (require (>= high low ))
  (amb low (an-integer-between (+ low 1) high)))
```

### 4.36
```scheme
; replace `an-integer-between` by `an-integer-starting-from`
(define (a-pythagorean-triple-between low)
  (let ((i (an-integer-starting-from low)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-starting-from j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; use 'an-integer-starting-from' generate infinite branches.
; if the low is 1, it will test (1,1,1), (1,1,2), (1,1,3), ...
; As a result, (1,2,1) would never be tested.

(define (a-pythagorean-triple-between low)
  (let ((k (an-integer-starting-from low)))
    (let ((j (an-integer-between low k)))
      (let ((i (an-integer-between low j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
```

### 4.37
```scheme
; Q: Is he correct?
; A: yes, Ben's procedure reduce the times of testing j and obviously skip the searching for k.
```

### 4.38
```scheme
; omit the requirement that Smith and Fletcher do not live on adjacent floors

((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))
```

### 4.39
```scheme
; Q: Does the order of the restrictions in the multiple-dwelling procedure affect the answer?
; A: No

; Q: Does it affect the time to find an answer? 
; A: Yes

(require (> miller cooper)) ; halve the possible cases
(require (not (= baker 5)))
(require (not (= cooper 1)))
(require (not (= fletcher 5)))
(require (not (= fletcher 1)))
(require (not (= (abs (- smith fletcher)) 1)))
(require (not (= (abs (- fletcher cooper)) 1)))
(require (distinct? (list baker cooper fletcher miller smith))) ; slowest test

; move the `distinct?` down to last can speed up finding result since some case would be rejected early.
```
