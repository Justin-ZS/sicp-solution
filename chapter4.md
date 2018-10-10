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
(define let-body caddr)
(define (parameters-argus parameters) (map car parameters))
(define (parameters-values) (map cadr parameters))

(define (let-combination exp)
  (let ((parameters (let-parameters exp)))
    (cons (make-lambda (parameters-argus parameters)
                  (let-body exp))
      (parameters-values parameters))))

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
          body
          (nest-let (rest-parameters parameters) body)
      )))
  (nest-let (let-parameters exp) (let-body exp)))

; action
(eval (let*->nested-lets exp) env)
; derived expressions is enough

(let*->nested-lets '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)))
; => (let ((x 3))
;      (let ((y (+ x 2)))
;        (let ((z (+ x y 5)))
;          ((* x z)))))
 
```


