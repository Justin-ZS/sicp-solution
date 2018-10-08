### SICP Solution Chapter 1
完成度: 31/46
### 1.3
```scheme
(define (square x) (* x x))

(define (sum-of-max-two x y z)
(cond ((and (< x y) (< x z)) (+ (square y) (square z)))
      ((< y z) (+ (square x) (square z)))
      (else (+ (square x) (square y))))
```
### 1.5
只要对`(p)`求值，程序就会死于无穷的递归。  
**normal-order evaluation**: 正常执行退出，输出0，因为求值被推迟到实际需要时，而此次调用不需要对`(P)`求值。  
**applicative-order evaluation**： 先求值在调用，直接卡死。

### 1.6
卡死， 因为*Scheme*使用的是**applicative-order evaluation**,按照新的定义方式，`new-if`成为一个普通的`combination`。
```scheme
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
```
在这一步死于无穷的`sqrt-iter`递归。

### 1.7
```scheme
;how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess.
(define (good-enough? guess x)
  (< (abs (- guess (improve guess x)))
     (abs (* guess 0.001))))
```

### 1.8
```scheme
(define (iter guess x)
  (if (good-enough? guess x)
      guess
      (iter (improve guess x) x)))


(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (< (abs (- guess (improve guess x)))
     (abs (* guess 0.001))))

(define (cube-root x)
  (iter 1.0 x))

```

### 1.9
```scheme
; recursive
(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))
; iterative
(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))
```

### 1.10
`(define (f n) (A 0 n))` computes `2n`  
`(define (g n) (A 1 n))` computes `2^n`  
`(define (h n) (A 2 n))` computes `2^2^2...(the number of 2 is n)`


### 1.11
```scheme
; recursive
(define (fn n)
  (cond ((< n 3) n)
        (else (+ 
          (fn (- n 1))
          (* 2 (fn (- n 2)))
          (* 3 (fn (- n 3))))
        )))

; iterative
(define (fn4 x y z count) 
  (define val (+ z (* 2 y) (* 3 x)))
  (if (= count 0)
      val
      (fn4 y z val (- count 1))))
(define (fn n)
  (cond ((< n 3) n)
        (else (fn4 0 1 2 (- n 3)))
```

### 1.12
```scheme
; 除每行最左侧与最右侧的数字以外，每个数字等于它的左上方与右上方两个数字之和（也就是说，第n行第k个数字等于第n-1行的第k-1个数字与第k个数字的和）
(define (pascal-triangle r c)
  (cond ((or (= c 0) (= r c)) 1)
        (else (+ (pascal-triangle (- r 1) (- c 1)) (pascal-triangle (- r 1) c)))
  ))
```

### 1.15
**a.** 5 times. `3^4 = 81 < 12.15 * 10 < 3^5 = 243`  
**b.** steps: ceil(log<sub>3</sub>10a) space: Θ(log<sub>3</sub>a）

### 1.16
```scheme
; invariant quantity
(define (fast-expt b n )
 (inv-expt-iter b n 1))
(define (inv-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (inv-expt-iter (* b b) (/ n 2) a))
        (else (inv-expt-iter b (- n 1) (* a b)))
  ))
```

### 1.17
```scheme
(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mul (* a 2) (/ b 2)))
        (else (+ a (fast-mul a (- b 1))))
  ))
```

### 1.18
```scheme
; invariant quantity
(define (fast-mul a b)
  (define (inv-mul-iter a b val)
    (cond ((= b 0) val)                                   ;end
          ((> (/ b 10) a) (inv-mul-iter b a val))         ;swap
          ((even? b) (inv-mul-iter (* a 2) (/ b 2) val))  ;double/halve
          (else (inv-mul-iter a (- b 1) (+ val a)))       ;add
    ))
  (inv-mul-iter a b 0))
```

### 1.19
[快速幂->矩阵的快速幕](https://www.jianshu.com/p/1c3f88f63dec)
```scheme
; Transform Matrix   |1,1| |a|
;                    |1,0| |b|
; Transform Function:  a = bq + aq + ap = a(p + q) + bq
;                      b = bp + aq      = aq + bq
; Corresponed Matrix(CM of pq):  |(p + q), q|
;                                |q      , p|
; (CM of pq)^2 =  (CM of p'q') 
; so, p' = p^2 + q^2 q' = 2pq + q^2

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))              ; compute p'
                   (+ (* 2 p q) (* q q))            ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
```


### 1.20
```scheme
; r = remainder
; normal-order
; (gcd 200 40)
; (gcd 40 (r 206 40))

; (if (= (r 206 40) 0)    ; 1
;     40
;    (gcd (r 206 40) (r 40 (r 206 40))))

;(if (= (r 40 (r 206 40)) 0)                     ; 1 + 2 = 3
;    (r 206 40)
;    (gcd (r 40 (r 206 40)) 
;         (r (r 206 40) (r 40 (r 206 40)))))

; (if (= (r (r 206 40) (r 40 (r 206 40))) 0)     ; 3 + 4 = 7
;    (r 40 (r 206 40))
;    (gcd (r (r 206 40) (r 40 (r 206 40)))
;         (r (r 40 (r 206 40))
;            (r (r 206 40) 
;               (r 40 (r 206 40))))))

;(if (= (r (r 40 (r 206 40))
;          (r (r 206 40) 
;             (r 40 (r 206 40)))) 0)             ; 7 + 7 = 14
;    (r (r 206 40) (r 40 (r 206 40)))
;    (gcd (r (r 206 40) (r 40 (r 206 40)))
;         (r (r 40 (r 206 40))
;            (r (r 206 40) 
;               (r 40 (r 206 40))))))

; (r (r 206 40) (r 40 (r 206 40)))               ; 14 + 4 = 18

; applicative-order
; (gcd 200 40)
; (gcd 40 (r 200 40))      ; 1
; (gcd 6 (r 40 6))         ; 2
; (gcd 4 (r 6 4))          ; 3
; (gcd 2 (r 4 2))          ; 4
; (gcd 2 0)
; 2
```

### 1.21
```scheme
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

;(smallest-divisor 199)   -> 199
;(smallest-divisor 1999)  -> 1999
;(smallest-divisor 19999) -> 7
```

### 1.22
```scheme
(define (search-for-primes start-value end-value)
  (define cur (if (even? start-value) (+ start-value 1) start-value))
  (if (prime? cur) (timed-prime-test cur))
  (if (<= cur end-value) (search-for-primes (+ 2 cur) end-value))
)
```

### 1.23
```scheme
(define (next x) (if (= x 2) 3 (+ x 2)))
```

### 1.34
在第二次迭代里，2作为参数变成里算子，导致error。  
`;The object 2 is not applicable.`

### 1.35
```scheme
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0)
; 1.6180327868852458
```
### 1.36
```scheme
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next ))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x)))
               2)
; 4.555540912917957

;with damping next -> (/ (+ next guess) 2)
(fixed-point (lambda (x) (/ (log 1000) (log x)))
               2)
; 4.555534487262465
```
应用`average damping`后会更快的得到结果

### 1.37
```scheme
(define (cont-frac next-n next-d count)
  (define (cont-frac-iter val i)
    (cond ((= i 0) val)
          (else (cont-frac-iter
                  (/ (next-n i) (+ val (next-d i)))
                  (- i 1))
          )
    ))
  (define (cont-frac-rec i)
    (cond ((= i (+ count 1)) 0)
          (else (/ (next-n i) (+ (next-d i) (cont-frac-rec (+ i 1))))
          )
    ))
  (cont-frac-rec 1))

(define (golden-cont-frac k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

(define (golden-ratio-deci tolerance)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (search-rec k v)
    (if (close-enough? v (golden-cont-frac (+ k 1)))
        k
        (search-rec (+ k 1) (golden-cont-frac (+ k 1)))
    ))
  (search-rec 1 0))

(golden-ratio-deci 0.0001)
; 10
```

### 1.38
```scheme
(define (cont-frac next-n next-d count)
  (define (cont-frac-iter val i)
    (cond ((= i 0) val)
          (else (cont-frac-iter
                  (/ (next-n i) (+ val (next-d i)))
                  (- i 1))
          )
    ))
  (cont-frac-iter 0 count))

(define (approx-e k)
  (define (next-d i)
    (cond ((< i 3) i)
          ((= (remainder (- i 2) 3) 0) (* 2 (+ 1 (/ (- i 2) 3))))
          (else 1)
    ))
  (+ (cont-frac (lambda (i) 1.0)
                next-d
                k) 2))
```

### 1.39
```scheme
(define (cont-frac next-n next-d count)
  (define (cont-frac-iter val i)
    (cond ((= i 0) val)
          (else (cont-frac-iter
                  (/ (next-n i) (+ val (next-d i)))
                  (- i 1))
          )
    ))
  (cont-frac-iter 0 count))

(define (tan-cf x k)
  (cont-frac (lambda (i) (
    if (= i 1)
       x
       (- (square x))
  ))
             (lambda (i) (- (* 2 i) 1))
             k))
```

### 1.40
```scheme
(define (cubic a b c)
  (define (cube x) (* x x x))
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c
              )))
```

### 1.41
```scheme
(define (double fn) (lambda (x) (fn (fn x))))
(define (inc x) (+ x 1))
(((double (double double)) inc) 5) ; 21
```

### 1.42
```scheme
(define (compose f g) (lambda (x) (f (g x))))
```

### 1.43
```scheme
(define (repeated f n) (
  if (= n 1) f (compose f (repeated f (- n 1)))
))
```

### 1.44
```scheme
(define dx 0.00001)
(define (smooth f) (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define (n-fold-smooth n) (repeated smooth n))
```

### 1.45
```scheme
(define (average-damp f) (lambda (x) (/ (+ x (f x)) 2)))
(define (n-times-average-damp n) (repeated average-damp n))

(define (fast-expt b n )
 (inv-expt-iter b n 1))
(define (inv-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (inv-expt-iter (* b b) (/ n 2) a))
        (else (inv-expt-iter b (- n 1) (* a b)))
  ))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next ))))
  (try first-guess))

(define (n-roots-manul x n k)
  (define (f y) (/ x (fast-expt y (- n 1.0))))
  (fixed-point ((n-times-average-damp k) f) 1.0))
```
| 开n次根 | 进行k次平均阻尼 | 结果 |
| --- | --- | --- |
| 2 | 1 | Converge |
| 3 | 1 | Converge |
| 4 | 1 | Unconverge |
| 4 | 2 | Converge |
| 5 | 2 | Converge |
| 6 | 2 | Converge |
| 7 | 2 | Converge |
| 8 | 2 | Unconverge |
| 8 | 3 | Converge |
| n | floor(log<sub>2</sub>n)| Converge |

```scheme
(define (n-roots x n)
  (n-roots-manul x n (floor (/ (log n) (log 2)))))
; (n-roots 1024 10) 
; Value: 2.0000011830103324
```

### 1.46
```scheme
(define (iter-improve improve enough?)
  (lambda (x)
    (define (calc x) (
      if (enough? x (improve x))
         x
         (calc (improve x)))
    )
    (calc x)))

(define enough? (lambda (v1 v2) (< (abs (- v1 v2)) 0.00001)))
(define (sqrt x)
  (define (f y) (/ x y))
  (define (aver-damp x) (/ (+ x (f x)) 2))
  ((iter-improve aver-damp enough?) 1.0)
)

(define (fixed-point f guess) (
  ((iter-improve f enough?) guess))
```

## 1.1 The Elements of Programming
Three mechanisms
* **primitive expressions:**  
  represent the simplest entities the language is concerned
* **means of combination:**  
  by which compound elements are built from simpler ones
* **means of abstraction:**  
  by which compound elements can be named and manipulated as units


### 1.1.1  Expressions
You type an *expression*, and the interpreter responds by displaying the result of its *evaluating* that expression.  

Like`(+ 2.7 10)`,formed by delimiting a list of expressions within parentheses in order to denote procedure application, are called *combinations*.

### 1.1.3  Evaluating Combinations
To evaluate a combination, do the following:
1. Evaluate the subexpressions of the combination.

2. Apply the procedure that is the value of the leftmost subexpression (the operator) to the arguments that are the values of the other subexpressions (the operands).

Some stipulations  
* the values of numerals are the numbers that they name,
* the values of built-in operators are the machine instruction sequences that carry out the corresponding operations, and
* the values of other names are the objects associated with those names in the environment.

So, in Lisp:
* Numbers and arithmetic operations are **primitive data and procedures**.
* Nesting of combinations provides a **means of combining operations**.
* Definitions that associate names with values provide a limited **means of abstraction**.
