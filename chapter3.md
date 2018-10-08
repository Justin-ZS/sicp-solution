### SICP Solution Chapter 3
完成度： 
### 3.1
```scheme
(define (make-accumulator init)
  (let ((sum init)) (lambda (val)
        (set! sum (+ sum val))
        sum
  )))
```

### 3.2
```scheme
(define (make-monitored fn)
  (let ((count 0))
       (lambda (arg) (
         cond ((eq? 'how-many-calls? arg) count)
              (else                       (set! count (+ count 1)) (fn arg))
       ))
  ))
```

### 3.3
```scheme
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p password)) (error "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)
```

### 3.4
```scheme
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((count 0))
    (define (dispatch p m)
      (cond ((and (not (eq? p password)) (= count 6)) (error "Call the cops"))
            ((not (eq? p password)) (set! count (+ count 1)) (error "Incorrect password"))
            ((eq? m 'withdraw) (set! count 0) withdraw)
            ((eq? m 'deposit)  (set! count 0) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                        m))))
  dispatch))
```

### 3.5
```scheme
(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((x-length (abs (- x1 x2)))
        (y-length (abs (- y1 y2))))
    (define test
      (let ((x (random-in-range (min x1 x2) (max x1 x2)))
            (y (random-in-range (min y1 y2) (max y1 y2))))
        (P x y)
      ))
    (* (monte-carlo trials test) (* x-length y-length))
  ))

(define (estimate-pi trials)
  (estimate-integral (lambda (x y) (<= (+ (square (- x 1)) (square (- y 1))) 1)) -1 1 -1 1 trials))
```

### 3.6
```scheme
(define rand
  (let ((x random-init))
    (define generate (lambda () (set! x (rand-update x)) x))
    (define reset (lambda (val) (set! x val)))
    (lambda (action)
      (cond ((eq? action 'generate) generate)
            ((eq? action 'reset) reset)
            (else  (error "MUST specied a valid action rather than" action))
      ))))
```

### 3.7
```scheme
(define (make-joint account password new-password)
  (lambda (p m)
    (if (eq? p new-password)
        (account password m)
        (error "Incorrect password")
    )))
```

### 3.8
```scheme
(define (fn)
  (let ((prev 0))
    (lambda (x) (let ((temp prev)) (set! prev x) temp))
  ))
(define f (fn))
```

### 3.9
Just like Figure 3.5
```scheme
; recursive
; Global: factorial
; (factorial 6) (factorial 5) (factorial 4) (factorial 3) (factorial 2) (factorial 1) 

; iterative
; Global: factorial fact-iter
; (factorial 6) (fact-iter 1 1 6) (fact-iter 1 2 6) (fact-iter 2 3 6) (fact-iter 6 4 6) (fact-iter 24 5 6) (fact-iter 120 6 6) (fact-iter 720 7 6)
```

### 3.10
```scheme
; since (let ((<var> <exp>)) <body>) is equal to ((lambda (<var>) <body>) <exp>)
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
; same to
(define (make-withdraw initial-amount)
  ((lambda (balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))) initial-amount))
; same to
(define make-withdraw 
  (lambda (initial-amount)
    ((lambda (balance)
      (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Insufficient funds"))) initial-amount)))
```
There three lambda function `[(L i),(L b),(L a)]`  
When `(define make-withdraw (L i))`, `(L i)` is evaluated to create a function object `(F i)` that link to **global env**.  
When `(make-withdraw 100)`, `(F i)` is called and create a frame which contains `initial-amount` bind to **100** as **[new env 1]** and point to **global env**.  
Exec the body of `(F i)` in **[new env 1]**, `(L b)` is evaluated and called with `initial-amount` as argument in **[new env 1]**.  
Evaluated `(L b)` will create a function object `(F b)` that link to **[new env 1]**.  
Call `(F b)` will create a new frame which contains `blance` bind to `initial-amount` as **[new env 2]** and point to **[new env 1]**.
Exec the body of `(F b)` in **[new env 2]**, `(L a)` is evaluated and the value is returned as result.  
Evaluate `(L a)` will create a function object `(F a)` that link to **[new env 2]**.  
so `(define W1 (make-withdraw 100))` will bind `W1` to `(F a)` in **global env**  
所以说就是多了let语句创建的一层。

### 3.11
Nothing is shared

### 3.12
```scheme
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
; (a b c d)
(cdr x)
; (b)
(define w (append! x y))
w
; (a b c d)
(cdr x)
; (b c d)
```

### 3.13
```scheme
; throw an error due to infinite loop. (last-pair)
```

### 3.14
```scheme
(define v (list 'a 'b 'c 'd))
v
; (a b c d)
(define w (mystery v))
w
; (d c b a)
v
(a)
```

### 3.16
```scheme
; 3
'(a b c)
; 4
(define x '(x))
(cons (cons 'b x) x)
; 7
(define y (cons x x))
(cons y y)
; infinie
(make-cycle '(a b c))
```

### 3.17
```scheme
(define (count-pairs xs)
  (let ((tracked '()))
    (define (count-pairs-cache rest)
      (cond ((not (pair? rest)) 0)
            ((memq rest tracked) 0)
            (else (set! tracked (cons rest tracked))
                  (+ (count-pairs-cache (car rest))
                     (count-pairs-cache (cdr rest))
                     1
                  ))
      ))
    (count-pairs-cache xs)
  ))
```

### 3.18
```scheme
(define (is-list-cycle? xs)
  (let ((tracked '()))
    (define (iter ls)
      (cond ((null? ls) #f)
            ((memq ls tracked) #t)
            (else (set! tracked (cons ls tracked))
                  (iter (cdr ls)))
      ))
    (iter xs)
))
```

### 3.19
若有环,快慢指针**同起点**一定相遇.
```scheme
(define (is-list-cycle? xs)
  (define (floyd-iter slow fast flag)
    (cond ((null? fast)               #f)
          ((null? (cdr fast))         #f)
          ((and (eq? slow fast) flag) #t)
          (else                       (floyd-iter (cdr slow) (cddr fast) #t))
    ))
  (floyd-iter xs xs #f))
```

### 3.21
```scheme
; the queue list is (front-ptr rear-ptr)
(define (print-queue q)
  (print (front-ptr q)))
```

### 3.22
```scheme
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (insert-queue! i)
      (cond ((empty-queue?) (set! front-ptr i) (set! rear-ptr i))
            (else           (set-cdr! rear-ptr i) (set! rear-ptr i))
      ))
    (define (delete-queue!)
      (cond ((empty-queue?) (error "DELETE! called with an empty queue"))
            (else           (set! front-ptr (cdr front-ptr)))
      ))
    (define (front-queue!)
      (cond ((empty-queue?) (error "FRONT! called with an empty queue"))
            (else           (car front-ptr))
      ))
    (define (dispatch m) 
      (cond ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'front-queue!)  front-queue!)
            ((eq? m 'empty-queue?)  empty-queue?)
            (else                   (error "Undefined operation -- QUEUE" m))
      ))
    dispatch))

(define (insert-queue! q i)
  ((q 'insert-queue!) (cons i '()))
  q)
(define (delete-queue! q)
  ((q 'delete-queue!))
  q)
(define (empty-queue? q)
  ((q 'empty-queue?))
  q)
(define (front-queue? q)
  ((q 'front-queue?))
  q)
```
### 3.24
```scheme
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
```

### 3.25
```scheme
; one dim
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
; two dim
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))
; generalize
(define (make-table)
  (list '*table*))
(define (lookup keys table)
  (if (null? keys) 
      (cdr table)
      (let ((subtable (assoc (car keys) (cdr table))))
        (if subtable
            (lookup (cdr keys) subtable)
            false))))
(define (insert! keys value table)
  (define (last xs) (if (null? (cdr xs)) xs (last (cdr xs))))
  (if (null? keys) 
      (set-cdr! table value)
      (let ((subtable (assoc (car keys) (cdr table))))
        (if subtable
            (insert! (cdr keys) value subtable)
            (let ((new-subtable (list (car keys))))
              (set-cdr! (last table) (list new-subtable))
              (insert! (cdr keys) value new-subtable)
            )
        )))
  'ok
)

(define t (make-table))
t
; (*table*)
(insert! '(1 2 3) 'a t)
; ok
t
; (*table* (1 (2 (3 . a))))
(lookup '(1 2 3) t)
; a
```

### 3.26
```scheme
; 一步到位，多维的二叉树表
(define (make-table)
  (define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (make-tree entry left right)
    (list entry left right))
  (define (assoc key records get-key)
    (cond ((null? records) false)
          ((= key (get-key (entry records))) (car records))
          ((< key (get-key (entry records))) (assoc key (left-branch records) get-key))
          ((> key (get-key (entry records))) (assoc key (right-branch records) get-key))
    ))
  (define (adjoin-set x set get-key)
    (cond ((null? set) (make-tree x '() '()))
          ((= (get-key x) (get-key (entry set))) set)
          ((< (get-key x) (get-key (entry set)))
          (make-tree (entry set) 
                      (adjoin-set x (left-branch set) get-key)
                      (right-branch set)))
          ((> (get-key x) (get-key (entry set)))
          (make-tree (entry set)
                      (left-branch set)
                      (adjoin-set x (right-branch set) get-key)))))
  (define (make-subtable key) (list key))
  (define get-key car)
  (define get-val cdr)
  (define set)
  
  (let ((local-table (make-subtable '*table*)))
    (define (lookup keys table)
      (if (null? keys)
          (get-val table)
          (let ((subtable (assoc (car keys) (cdr table) get-key)))
            (if subtable
                (lookup (cdr keys) subtable)
                false))))
    (define (insert! keys value table)
      (if (null? keys)
          (set-cdr! table value)
          (let ((subtable (assoc (car keys) (cdr table) get-key)))
            (if subtable
                (insert! (cdr keys) value subtable)
                (let ((new-subtable (make-subtable (car keys))))
                  (define new-table (adjoin-set new-subtable (cdr table) get-key))
                  (set-cdr! table new-table)
                  (insert! (cdr keys) value new-subtable)
                )
            )))
      'ok
    )
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (keys) (lookup keys local-table)))
            ((eq? m 'insert-proc!) (lambda (keys value) (insert! keys value local-table)))
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (lookup keys table)
  ((table 'lookup-proc) keys))
(define (insert! keys value table)
  ((table 'insert-proc!) keys value))

(define (test-table)
  (let ((t (make-table)))
    (insert! '(1 2 3) 'a t)
    (display (lookup '(1 2 3) t))
    (newline)
    (display (lookup '(1 3 3) t))
    (newline)
    (insert! '(1 3 3) 'world t)
    (display (lookup '(1 2 3) t))
    (newline)
    (display (lookup '(1 3 3) t))
    (newline)
  ))
```

### 3.27
```scheme
; 加了cache以后，每个n只会算一次，所以(memo-fib n)只会算n次。
(define memo-fib (memoize fib))
; 不起作用，因为fib里面递归的是fib，而非有cache的memo-fib

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x) ; memo-fib 抓的即是这个函数，共用一个table，所有cache有效
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))
```

### 3.28
```scheme
(define (logical-or a b)
  (cond ((or (= a 1) (= b 1)) 1)
        (else 0)
  ))
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
```

### 3.29
```scheme
(define (or-gate a1 a2 output)
  (let ((b1 (make-wire)) (b2 (make-wire)) (c (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)
    'ok
  ))
; or-gate-delay: 2 * 'inverter-delay' + 'and-gate-delay'
```

### 3.30
T<sub>n-rcA</sub>  
= n * T<sub>FA</sub>  
= n * (2 * T<sub>HA</sub> + T<sub>or</sub>)  
= n * (2 * (max(T<sub>or</sub>, (T<sub>and</sub> + T<sub>inv</sub>)) + T<sub>and</sub>) + T<sub>or</sub>)

### 3.31
'after-delay' will add a callback to agenda, if we don't call appended procedure immediately, nothing will happen.

### 3.38
a:   
1. 45 (peter -> paul -> mary), (paul -> peter -> mary)
1. 35 (peter -> mary -> paul)
1. 50 (paul -> mary -> peter)
1. 40 (mary -> paul -> peter), (mary -> peter -> paul)  

b:  110, 80, 50,...

### 3.39
101, 121, 100
11不可能，因为P2被序列号，无法被插入。
110不可能，因为（* x x）被序列号，两次取的x值一定相等。

### 3.40
P1: `(lambda () (set! x (* x x)))`  
P2: `(lambda () (set! x (* x x x)))`  
Three stage: `Gn`(get n-th value of x) `S`(set)  
1. 1,000,000 [`P1 -> P2` (10<sup>2<sup>3</sup></sup>), `P2 -> P1` (10<sup>3<sup>2</sup></sup>)]
1. 100,000 [`P2(G1) -> P1 -> P2(G2) -> P2(G3)` (10 * 100 * 100)]
1. 10,000 [`P1(G1) -> P2 -> P1(G2)` (10 * 1000), `P2(G1) -> P2(G2) -> P1 -> P2(G3)` (10 * 10 * 100)]
1. 1,000 [`P2(G) -> P1 -> P2(S)` (10 * 10 * 10)]
1. 100 [`P1(G) -> P2 -> P1(S)` (10 * 10)]

### 3.41
No, blance are only changed by withdraw and deposit procedure.
The balance willn't be anomalous since both procedure have been serialized

### 3.42
Safe.  Threre is no difference between two version.


### 3.43
```scheme
; exchange: 10 <-> 20, 10 <-> 30  
; 10 +10  +20  20  40     
; 20 -10       10  10  
; 30      -20  30  10  
```
Q: The sum of the balances in the accounts will be preserved?  
A: Yes!

### 3.44
Louis is wrong.  
The essential difference is in the `amount/difference` between transfer and exchange.  
It's safe since the amount is passed as argument rather then calculated from two account.


### 3.45
Deadlock  
调用被序列化的`exchange`会消耗掉唯一的序列资源，而在`exchange`里面，再次调用被同样序列化的`withdraw／deposit`会再次请求已被占用的序列资源，所以被挂起直到`exchange`结束，这同样会导致`exchange`永远不会结束。  
原版为什么不会呢？因为原版`exchange`调用的`withdraw／deposit`并没有被序列化。


### 3.46
```scheme
; Procedure: P1, P2
; P1 check mutex -> P2 check mutex -> P1 set mutex and run -> P2 set mutex and run -> anomalous result
```

### 3.47
```scheme
; a
(define (make-semaphore n)
  (let ((resource n)
        (mutex (make-mutex))) ; used for semaphore

    (define (update fn) (set! resource (fn resource 1)))
    (define (inc) (update +))
    (define (dec) (update -))

    (define (acquire)
      (mutex 'acquire)
      (if (> n 0)
          (begin (dec) (mutex 'release))
          (begin (mutex 'release) (acquire))
      ))

    (define (release)
      (mutex 'acquire)
      (inc)
      (mutex 'release))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))
      ))

    the-semaphore
  ))
; b
(define (make-semaphore n)
  (let ((resource n)
        (cell (list false)))

    (define (update fn) (set! resource (fn resource 1)))
    (define (inc) (update +))
    (define (dec) (update -))

    (define (acquire)
      (test-and-set! cell)
      (if (> n 0)
          (begin (dec) (clear! cell))
          (begin (clear! cell) (acquire))))
    (define (release)
      (test-and-set! cell)
      (inc)
      (clear! cell))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))
      ))

    the-semaphore
  ))
```

### 3.48
```scheme
; In previous dealock example: (exchange a1 a2) and (exchange a2 a1)
; If they check the smaller-numbered account first, they both will acquire a1 or a2 firstly
```

### 3.50
```scheme
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
```

### 3.51
```scheme
(display x)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
; 0 1 2 3 4 5 // error
; 1 2 3 4 5 // stream-map will consume the first element of stream
(stream-ref x 7)
; 6 7  // 1 ~ 5 has been consumeed in (stream-ref x 5)
```

### 3.52
```scheme
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; seq: (1, #stream), (1, 3, #stream), (1, 3, 6, #stream), (1, 3, 6, 10, #stream)...
(define y (stream-filter even? seq))
; The first even number is 6.
; y: (6, #stream) sum: 6  
                                                                 
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))      
                         seq))         
; The first finded number is 10
: z: (10, #stream) sum: 10
(stream-ref y 7)
; the 8th number in y, sum: 136                  
                                                                 
(display-stream z)
; all items in z, total sum: 210
```
Q: Implemented `(delay <exp>)` simply as `(lambda () <exp>)`, what would differ?  
A: 增加很多重复计算，并且每次计算都会增加sum的值，结果完全不同。  
每个`stream-cons`都有一个`delay`
* `stream-enumerate-interval`： 增加计算量
* `stream-map`：增加计算量，重复增加sum值
* `stream-filter`: 增加计算量  

有副作用(side-effect)的函数在stream(lazy evaluate)中会带来很多非常难debug的问题。如果要用惰性求值，所有的函数一定,必须,只能是**纯函数**！(mathematic function).这样做不做cache只是性能问题，而不可能改变结果。


### 3.53
2<sup>n</sup> sequence
```scheme
; (1, 2, 4, 8, ...)
```

### 3.54
```scheme
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))
```

### 3.55
```scheme
(define (partial-sums s)
  (add-streams (cons-stream 0 (partial-sums s)) s))
```

### 3.56
```scheme
(define S (cons-stream 1
  (merge (scale-stream S 2)
    (merge (scale-stream S 3)
      (scale-stream S 5)))
  ))
```

### 3.57
```scheme
; n: 0, (0, 1, 1, #stream)        +: 1
; n: 1, (0, 1, 1, 2, #stream)     +: 2
; n: 2, (0, 1, 1, 2, 3, #stream)  +: 3
; n: n, ...                       +: n + 1

; without memorize
; n: 0, (0, 1, 1, #stream)              +: 1 + 0 + 0 = 1
; n: 1, (0, 1, 1, 2, #stream)           +: 1 + 1 + 0 = 2
; n: 2, (0, 1, 1, 2, 3, #stream)        +: 1 + 2 + 1 = 4
; n: 3, (0, 1, 1, 2, 3, 5, #stream)     +: 1 + 4 + 2 = 7
; n: 4, (0, 1, 1, 2, 3, 5, 8, #stream)  +: 1 + 7 + 4 = 12
; n: n, ...                             +: fib(n + 3) - 1
; where f(0): 0, f(1): 1, f(2): 1, ...
```

### 3.58
```scheme
(expand 1 7 10)
; (quotient (* 1 10) 7) --> 1
; (quotient (* 3 10) 7) --> 4
; (quotient (* 2 10) 7) --> 2
; ...
(expand 3 8 10)
; (quotient (* 3 10) 8) --> 3
; (quotient (* 6 10) 8) --> 7
; (quotient (* 4 10) 8) --> 5
; (quotient (* 0 10) 8) --> 0
; (quotient (* 0 10) 8) --> 0
; ...
```

### 3.59
```scheme
; a
(define (integrate-series s)
  (stream-map / s integers))
; b
(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))

(define (neg-stream s)
  (stream-map (lambda (x) (- 0 x)) s))
(define neg-sin-series
  (cons-stream 0 (neg-stream (integrate-series cosine-series))))
(define sine-series
  (neg-stream neg-sin-series))
```

index | 0 | 1 | 2 | 3
--- | --- | --- | --- | ---
e<sup>x</sup> | 1 | 1 | 1/2 | 1/(3*2)
cos(x) | 1 | 0 | -1/2 | 0
-sin(x) | 0 | -1 | 0 | 1/(3*2) |
sin(x) | 0 | 1 | 0 | -1/(3*2) |

然后正负是可以交换的，所以简化为：
```scheme
(define (neg-stream s)
  (stream-map (lambda (x) (- 0 x)) s))

(define cosine-series
  (cons-stream 1 (neg-stream (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
; the derivative of sine is cosine and the derivative of cosine is the negative of sine
(define (deriv-stream s)
  (neg-stream (integrate-series s)))
```

### 3.60
```scheme
(define (mul-series s1 s2)
  (let ((fst-s1 (stream-car s1)) (fst-s2 (stream-car s2))
        (rest-s1 (stream-cdr s1)) (rest-s2 (stream-cdr s2)))
    (cons-stream (* fst-s1 fst-s2) 
      (add-streams
        (add-streams
          (scale-stream rest-s1 fst-s2)
          (scale-stream rest-s2 fst-s1))
        (cons-stream 0 (mul-series rest-s1 rest-s2))
        ))))
```

### 3.61
```scheme
(define (invert-unit-series s)
  (cons-stream 1 
    (mul-series 
      (scale-stream (stream-cdr s) -1)
      (invert-unit-series s))
  ))
```

### 3.62
```scheme
(define (div-series nums dens)
  (if (= (stream-car dens) 0)
      (error "The denominator series MUST begins with a nonzero constant term")
      (mul-series nums (invert-unit-series dens))
  ))

(define tangent-series (div-series sine-series cosine-series))
```

### 3.63
回忆一下cache的处理，当`stream-cdr`访问同一个stream两次时，第二次返回的是cache起来的结果。  
这里的核心是**同一个**stream，什么时候会认为是同一个呢？  
先看之前的版本：
```scheme
 (define (sqrt-stream x)
   (define guesses
     (cons-stream 1.0
                  (stream-map (lambda (guess) (sqrt-improve guess x))
                               guesses)))
   guesses)
; create a stream
(define s (sqrt-stream 1))
; s = (1, #stream) = (1, (stream-map fn s)))
; #stream = (stream-map fn s)
; 这里有两个`cons-stream`: 一个在`sqrt-stream`，一个在`stream-map`里.
; 前者只负责把第一项cons进去，剩下的项都被后者塞进去。
; 因此忽略第一项，当我们访问第二项会怎么样？
(stream-car (stream-cdr s))
; `stream-cdr`会强迫对`#stream`求值(evaluate), 即调用`stream-map`
; `stream-map`会返回(stream-cons (fn 1) (stream-map fn (stream-cdr s)))
; 看到了么，核心在最后的(stream-cdr s)，这和我们主动的调用一摸一样,这是第二次。

(stream-car (stream-cdr (stream-cdr s)))
; 访问第三项来触发第二次`stream-cdr`

; 下一个问题是为什么会认为是同一个stream?
; 因为他们都是guesses，自然是同一个
; 所以Louis版的问题就在于这两个stream并不是同一个。
; (sqrt-stream 1) != (sqrt-stream 1)
; cache无效,就会产生很多额外的计算

; 如果本身`delay`没有做cache，这俩就没区别了。
```

### 3.64
```scheme
(define (stream-limit stream tolerance)
  (let ((fst (stream-car stream))
        (snd (stream-car (stream-cdr stream))))
    (if (< (abs (- fst snd)) tolerance)
        snd
        (stream-limit (stream-cdr stream))
    )))
```

### 3.65
```scheme
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))
```

### 3.66
```scheme
; (1,100) in (1+, 1+)
; (1, 1), (2+, 2+), (1, 2+)
; 大约 200个 (1 + 99 + 98 = 198)
; (99, 100)
; (2, 100): 在(2+, 2+)中大约前面有200个，类似(1, 100)
; 在总体中，交叉来看大约 200 * 2 = 400个
; 所以 (99, 100) 前大概有200 * 2^98个， 具体为(99 * 2^99)
```

### 3.67
```scheme
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                (stream-map (lambda (x) (list (stream-car t) x)) (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))
  ))
```

### 3.68
```scheme
; he remove the `cons-stream`, so the recurse wouldn't be delayed
; (pairs s t) -> (pairs (stream-cdr s) (stream-cdr t)) -> ...
; cause a infinite loop
```

### 3.69
```scheme
(define (triples s t u)
  (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (tu) (cons (stream-car s) tu)) (pairs t (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))
    )
  ))
(define pythagorean-triples
  (stream-filter
    (lambda (xs) (= (+ (square (car xs)) (square (cadr xs))) (square (caddr xs))))
    (triples integers integers integers)
  ))
```

### 3.70
```scheme
(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1-car (stream-car s1))
                (s2-car (stream-car s2)))
              (if (< (weight s1-car) (weight s2-car))
                  (cons-stream s1-car (merge-weighted weight (stream-cdr s1) s2))
                  (cons-stream s2-car (merge-weighted weight s1 (stream-cdr s2)))
              )))
  ))

(define (weighted-pairs weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))

; a
(define (weight-a xs)
  (+ (car xs) (cadr xs)))
(define pairs-a (weighted-pairs weight-a integers integers))

;b
(define (weight-b xs)
  (+ (* 2 (car xs)) (* 3 (cadr xs)) (* 5 (car xs) (cadr xs))))
(define (divisible-235? x)
  (cond ((= (remainder x 2) 0) #t)
        ((= (remainder x 3) 0) #t)
        ((= (remainder x 5) 0) #t)
        (else #f)
  ))
(define (filter-b? xs)
  (not (or
    (divisible-235? (car xs))
    (divisible-235? (cadr xs)))
  ))
(define pairs-b
  (stream-filter filter-b?
    (weighted-pairs weight-b integers integers)))
```

### 3.71
```scheme
(define (sum-of-cube xs) 
  (let ((fst (car xs))
        (snd (cadr xs)))
    (+ (* fst fst fst) (* snd snd snd))
  ))

(define ramanujan-numbers
  (let ((stream (weighted-pairs sum-of-cube integers integers)))
    (define (filter-ramanujan s1 s2)
      (if (= (sum-of-cube (stream-car s1)) (sum-of-cube (stream-car s2)))
          (cons-stream (stream-car s1) (filter-ramanujan (stream-cdr s1) (stream-cdr s2)))
          (filter-ramanujan (stream-cdr s1) (stream-cdr s2))
      ))
    (stream-map sum-of-cube (filter-ramanujan stream (stream-cdr stream)))
  ))

; 
```

### 3.72
```scheme
(define (filter-three s1 s2, s3)
  (if (= (sum-of-cube (stream-car s1)) (sum-of-cube (stream-car s2)) (sum-of-cube (stream-car s3)))
      (cons-stream (stream-car s1) (filter-ramanujan (stream-cdr s1) (stream-cdr s2) (stream-cdr s3)))
      (filter-ramanujan (stream-cdr s1) (stream-cdr s2) (stream-cdr s3))
  ))
(define three-ways-numbers
  (define stream (weighted-pairs sum-of-cube integers integers))
  (stream-map sum-of-cube (filter-three stream (stream-cdr stream) (stream-cdr (stream-cdr stream)))))
```