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
这是著名的[停机问题](https://zh.wikipedia.org/wiki/%E5%81%9C%E6%9C%BA%E9%97%AE%E9%A2%98)
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
; it becomes a paradox
```