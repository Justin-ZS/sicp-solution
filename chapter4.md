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
                (list-of-values (operands exp) env)))
; equals
; (apply (eval define env) ...)
; (eval define env) will throw an exception

; b
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
```

### 4.3
self-evaluating和variable不是复杂表达式，不需要改变

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