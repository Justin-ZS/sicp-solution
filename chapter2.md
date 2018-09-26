### SICP Solution Chapter 2
完成度： 75/97
### 2.1
```scheme
(define (make-rat n d)
  (cond (and (< n 0) (< d 0)) (cons (- n) (- d))
        (or (< n 0) (< d 0))  (cons (- (abs n)) (abs d))
        (else                 (cons n d)
  )
; other solution
(define (make-rat n d)
    (if (< d 0)
        (cons (- n) (- d))
        (cons n d)))
```

### 2.2
```scheme
(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment seg)
  (define (average x y) (/ (+ x y) 2.0))
  (let ((start (start-segment seg))
        (end   (end-segment seg)))
       (make-point (average (x-point start) (x-point end))
                   (average (y-point start)   (y-point end)))
  ))

```

### 2.3
```scheme
(define (rect-perimeter rect)
  (let ((width  (rect-width rect))
        (height (rect-height rect)))
       (* (+ width height) 2)))

(define (rect-area rect)
  (* (rect-width rect) (rect-height rect)))

(define (segment-length seg)
  (let ((x-length (abs (- (x-point (start-segment seg)) (x-point (end-segment seg)))))
        (y-length (abs (- (y-point (start-segment seg)) (y-point (end-segment seg))))))
       (sqrt (+ (square x-length) (square y-length)))))

; rectangle: (cons left-top-point (cons width-seg height-seg))

(define (make-rect lt rt lb)
  (cons lt (cons (make-segment lt lb)
                 (make-segment lt rt))))

(define (rect-width rect) (segment-length (car (cdr rect))))
(define (rect-height rect) (segment-length (cdr (cdr rect))))

; rectangle: (cons (cons width-seg width-seg2) (cons height-seg height-seg2))

(define (make-rect w1 w2 h1 h2)
  (cons (cons w1 w2) (cons h1 h2)))

(define (rect-width rect) (segment-length (car (car rect))))
(define (rect-height rect) (segment-length (cdr (cdr rect))))
```

### 2.4
```scheme
; (car (cons x y))
; (car (lambda (m) (m x y)))
; ((lambda (m) (m x y)) (lambda (p q) p)))
; ((lambda (p q) p) x y)
; x

(define (cdr z)
  (z (lambda (p q) q)))
```

### 2.5
```scheme
(define (cons x y) (* (expt 2 x) (expt 3 y)))

(define (divider-iter divisor val k)
  (if (= (remainder val divisor) 0)
      (divider-iter divisor (/ val divisor) (+ k 1))
      k))

(define (car z) (divider-iter 2 z 0))
(define (cdr z) (divider-iter 3 z 0))

```
### 2.6
```scheme
(define zero (lambda (f) (lambda (x) x)))
; (add-1 zero)
; ((lambda (f) (lambda (x) (f ((n f) x)))) (lambda (f) (lambda (x) x)))
; ((lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
; ((lambda (f) (lambda (x) (f x))))

(define one ((lambda (f) (lambda (x) (f x)))))
(define two ((lambda (f) (lambda (x) (f (f x))))))

(define (add x y) ((lambda (f) (lambda (x) ((x f) ((y f) x)))))

```

### 2.7
```shceme
(define lower-bound car)
(define upper-bound cdr)
```

### 2.8
```scheme
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y)) (- (upper-bound x) (lower-bound y))))

```

### 2.9
```scheme
(define (width-interval x) (/ (+ (lower-bound x) (upper-bound x)) 2))
```

### 2.10
```scheme
(define (div-interval x y)
  (if (<= (* (upper-bound y) (lower-bound y)) 0)
      (error "The divisor interval spans 0")
      (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))
```

### 2.12
```scheme
(define (make-center-percent c p)
  (define width (* (/ p 100) c))
  (make-interval (- c width) (+ c width)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent x)
  (* 100 (/ (width x) (center x))))
```

### 2.17
```scheme
(define (last-pair xs)
  (if (null? (cdr xs))
      (car xs)
      (last-pair (cdr xs))
  ))
```

### 2.18
```scheme
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse xs)
  (if (null? xs)
      '()
      (append (reverse (cdr xs)) (list (car xs)))
  ))
```

### 2.19
```scheme
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coin-values)) 0)
        (else
         (+ (cc amount
                (cdr coin-values))
            (cc (- amount
                   (car coin-values))
                coin-values)))))
```
Q: Does the order of the list coin-values affect the answer produced by cc?
A: No. see the result of `(cc 100 (reverse us-coins))`

### 2.20
```scheme
(define (same-parity fst . xs)
  (define same? (if (even? fst) even? odd?))
  (define (filter p xs)
    (cond ((null? xs)     '())
          ((p (car xs)) (cons (car xs) (filter p (cdr xs))))
          (else         (filter p (cdr xs)))
    ))
  (cons fst (filter same? xs)))

```

### 2.21
```scheme
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))
```

### 2.22
```scheme
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things)) ; Error!
                    answer))))            ; (square-list (list 1 2)) -> (cons 4 (cons 1 '())) 
  (iter items '()))
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer                    ; Error!
                    (square (car things)))))) ; (square-list (list 1 2)) -> (cons (cons '() 1) 4) 
  (iter items '()))
```

### 2.23
```scheme
(define (for-each f xs)
  (cond ((not (null? xs)) (f (car xs)) (for-each f (cdr xs))))
)
```

### 2.24
(1 (2 (3 4)))

### 2.25
```scheme
(define l1 (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr l1)))))

(define l2 (list (list 7)))
(car (car l2))

(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))
```

### 2.26
```scheme
(append x y)
; (1 2 3 4 5 6)

(cons x y)
; ((1 2 3) 4 5 6)

(list x y)
; ((1 2 3) (4 5 6))
```

### 2.27
```scheme
(define (deep-reverse xs)
  (cond ((null? xs)       '())
        ((not (pair? xs)) (list xs))
        ((pair? (car xs)) (append (deep-reverse (cdr xs)) (list (deep-reverse (car xs)))))
        (else             (append (deep-reverse (cdr xs)) (deep-reverse (car xs))))
  ))
```

### 2.28
```scheme
(define (fringe xs)
  (cond ((null? xs) '())
        ((not (pair? xs)) (list xs))
        (else (append (fringe (car xs)) (fringe (cdr xs))))
  ))
```

### 2.29
```scheme
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a
(define (compose f g) (lambda (x) (f (g x))))
(define left-branch car)
(define right-branch (compose car cdr))

(define branch-length car)
(define branch-structure (compose car cdr))

; b
(define left-structure (compose branch-structure left-branch))
(define right-structure (compose branch-structure right-branch))


(define mobile? pair?)
(define (structure-weight x) (if (mobile? x) (total-weight x) x))
(define (total-weight m)
  (let ((left   (left-structure m))
        (right  (right-structure m)))
       (+ (structure-weight left) (structure-weight right))
  ))

; c
; the length of the left rod multiplied by the weight
(define (branch-torque b)
  (* (branch-length b) (structure-weight (branch-structure b))))

(define (balanced? m)
  (let ((left   (left-branch m))
        (right  (right-branch m)))
       (define (sub-blanced? x) (if (mobile? x) (balanced? x) #t) )       
       (and (= (branch-torque left) (branch-torque right))
            (sub-blanced? (branch-structure left))
            (sub-blanced? (branch-structure right))
       )))
; d
(define right-branch cdr)
(define branch-structure cdr)
```

### 2.30
```scheme
(define (square-tree t)
  (cond ((null? t)       '())
        ((not (pair? t)) (square t))
        (else            (cons (square-tree (car t)) (square-tree (cdr t))))
  ))

(define (square-tree t)
  (map (lambda (i) (if (pair? i) (square-tree i) (square i))) t))
```

### 2.31
```scheme
(define (tree-map f t)
  (map (lambda (i) (if (pair? i) (tree-map f i) (f i))) t))

(define (square-tree tree) (tree-map square tree))
```

### 2.32
```scheme
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
; 显然
```

### 2.33
```scheme
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
```

### 2.34
```scheme
; Horner's rule

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
```

### 2.35
```scheme
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))
```

### 2.36
```scheme
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
```

### 2.37
```scheme
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

```

### 2.38
从左往右或是从右往左，本质上是改变运算的顺序，运算数(operands)的顺序并没有改变。
改变运算的顺序而不影响结果，即op满足结合律(Associative)

### 2.39
```scheme
(define (reverse sequence)
  (fold-right (lambda (cur acc) (append acc (list cur))) '() sequence))
(define (reverse sequence)
  (fold-left (lambda (acc cur) (cons cur acc)) '() sequence))
```

### 2.40
```scheme
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 2 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
```

### 2.41
```scheme
(define (unique-triples n)
  (flatmap (lambda (k)
             (flatmap (lambda (j)
                    (map (lambda (i)
                           (list i j k))
                         (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 2 (- k 1))))
           (enumerate-interval 3 n)))

(define (eq-sum-triples n) (filter (lambda (ts) (= (accumulate + 0 ts) n)) (unique-triples n)))
```

### 2.42
```scheme
(define (adjoin-position row col prev)
  (cons (list row col) prev))

(define empty-board '())

(define (check? q1 q2)
  (let ((x1 (car q1))
        (y1 (cadr q1))
        (x2 (car q2))
        (y2 (cadr q2)))
       (cond ((= x1 x2)                     #t) ; same row
             ((= y1 y2)                     #t) ; same column
             ((or (= (- x1 y1) (- x2 y2)) 
                  (= (+ x1 y1) (+ x2 y2)))  #t) ; same diagonal   
             (else                          #f))
  ))

(define (find-k k positions)
  (car (filter (lambda (p)
                 (= (cadr p) k))
               positions)
  ))

(define (safe? k positions)
  (let ((k-pos  (find-k k positions)))
       (= (length
            (filter (lambda (p)
                      (check? p k-pos))
                    positions)
          ) 1)
  ))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
```

### 2.43
此程序的开销都在`queen-cols`上，包含`board-size`次迭代。
调换顺序后`queen-cols`由每个迭代跑1次，增加到每个迭代跑`board-size`次。  
原先是:  
  * `board-size + 1`次  

现在是:  
  * 1 + `board-size` + `board-size^2` + `board-size^3` + ... + `board-size^board-size`  

所以解决**eight-queens puzzle**问题的时间大约是T<sup>8</sup>.(board-size = 8)

### 2.44
```scheme
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
```

### 2.45
```scheme
(define (split fn1 fn2)
  (define (paint-split p n)
    (if (= n 0)
        p
        (let ((smaller (paint-split p (- n 1))))
             (fn1 painter (fn2 smaller smaller)))))
  paint-split)

(define right-split (split beside below))
(define up-split (split below beside))
```

### 2.53
```scheme
(list 'a 'b 'c)                         ; (a b c)
(list (list 'george))                   ; ((george))
(cdr '((x1 x2) (y1 y2)))                ; ((y1 y2))
(cadr '((x1 x2) (y1 y2)))               ; (y1 y2)
(pair? (car '(a short list)))           ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks))     ; (red shoes blue socks)
```

### 2.54
```scheme
(define (equal? xs ys)
  (if (and (eq? xs '()) 
           (eq? ys '()))
      #t
      (let ((x (car xs))
            (y (car ys)))
           (and (if (and (pair? x) (pair? y)) ; recursively
                      (equal? x y)
                      (eq? x y))
                (equal? (cdr xs) (cdr ys)))
      )
  ))
```

### 2.55
```scheme
;  ' is just shorthand for quote
; ''abracadabra equals (quote (quote abracadabra)))

(car (quote (quote abracadabra))) ; quote
```

### 2.56
```scheme
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (make-exponentiation base exp)
  (cond ((= exp 0)  1)
        ((= exp 1)  base)
        (else       (list '** base exp))))
(define (base s) (cadr s))
(define (exponent s) (caddr s))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
          (make-product (make-product (exponent exp)
                                      (make-exponentiation (base exp) (- (exponent exp) 1)))
                        (deriv (base exp) var))
        )
        (else
         (error "unknown expression type -- DERIV" exp))))
```

### 2.57
```scheme
(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand s)
  (if (= (length s) 3)
      (caddr s)
      (cons '* (cddr s))))

; (+ (* x (+ (* y (+ 1 0)) (* 0 (+ x 3)))) (* 1 (* y (+ x 3))))

; The question of whether two numerically equal numbers (as tested by =) are also eq? is highly implementation-dependent.
; (eq? 1 1) #t
(define (make-sum a1 a2)
  (cond ((eq? a1 0) a2)
        ((eq? a2 0) a1)
        (else     (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((eq? m1 1) m2)
        ((eq? m2 1) m1)
        ((or (eq? m1 0) (eq? m2 0)) 0)
        (else     (list '* m1 m2))))

; (+ (* x y) (* y (+ x 3)))
```

#### 2.58
```scheme
; a
(define (make-sum a1 a2) (list a1 '+ a2))
(define (make-product m1 m2) (list m1 '* m2))

(define (addend s) (car s))
(define (multiplier p) (car p))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

; (deriv '(x + (3 * (x + (y + 2)))) 'x)
; (1 + ((3 * (1 + (0 + 0))) + (0 * (x + (y + 2)))))
; 4

; b
; 难，唯一的思路是将标准语法转换成全括号的前缀语法，最后将计算结果返转回标准语法。
; -. 考虑乘法的优先级，给乘法前后加上括号
; -. 考虑连续的+和*运算。
```

### 2.59
```scheme
; Θ(n^2)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else         (union-set (cdr set1) (adjoin-set (car set1) set2)))
  ))
```

### 2.60
```scheme
; element-of-set?不变

; 性能优化，Θ(n) -> Θ(1)
(define adjoin-set cons)

; intersection-set不变

; union-set不变,性能优化 Θ(n^2) -> Θ(n)
```

### 2.61
```scheme
; Θ(n) [≈ Θ(n/2)]
(define (adjoin-set x set)
  (cond ((null? set)     (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else            (cons (car set) (adjoin-set x (cdr set))))
  ))
```

### 2.62
```scheme
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else         (let ((x1 (car set1)) (x2 (car set2)))
                           (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                                 ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                                 ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
                           )))
  ))
```

### 2.63
```scheme
; a
; 一样

; b
; 如果不看append函数，两种算法复杂度相同。
; 实际上append的复杂度为Θ(n)
; 所以前者性能更差，Θ(n^2) > Θ(n)
```

### 2.64
```scheme
; a
; tree '(1 2 3 4 5 6 7 8 9))
; (5 (2 (1 () ()) (3 () (4 () ()))) (7 (6 () ()) (8 () (9 () ()))))
;
;            5
;           / \
;          /   \
;         2     7
;        / \   / \
;       1   3 6   8
;            \     \
;             4     9

; b
; 每次迭代出一个节点，所以是Θ(n)
```

### 2.65
[Do I misunderstand the meaning of exercise 2.65 of SICP?
](https://stackoverflow.com/questions/17522420/do-i-misunderstand-the-meaning-of-exercise-2-65-of-sicp)
```scheme
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else         (let ((x1 (car set1)) (x2 (car set2)))
                           (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                                 ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                                 ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
                           )))
  ))

(define (union-set-tree set1 set2)
  (list->tree (union-set (tree->list set1)
                         (tree->list set2))))

(define (intersection-set-tree set1 set2)
  (list->tree (intersection-set (tree->list set1)
                                (tree->list set2))))
```

### 2.66
```scheme
(define (lookup given-key set-of-records)
  (define record (entry set-of-records))
  (cond ((null? set-of-records) #f)
        ((= given-key (key record)) record)
        ((< given-key (key record)) (lookup given-key (left-branch set-of-records)))
        ((> given-key (key record)) (lookup given-key (right-branch set-of-records)))
  ))
```

### 2.67
```scheme
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(decode sample-message sample-tree)
; (a d a b b c a)
```

### 2.68
```scheme
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-in-list? x xs)
  (cond ((null? xs)       #f) 
        ((eq? x (car xs)) #t)
        (else             (element-in-list? x (cdr xs)))
  ))

(define (encode-symbol c tree)
  (define (iter-encode c tree bits)
    (cond ((eq? c (symbol-leaf tree)) bits)
          ((leaf? tree) (error "The symbol is not in the tree." (symbol-leaf tree) c))
          ((element-in-list? c (symbols (left-branch tree))) (iter-encode c (left-branch tree) (append bits '(0))))
          (else (iter-encode c (right-branch tree) (append bits '(1))))
    ))
  (iter-encode c tree '())
)

(equal? (encode (decode sample-message sample-tree)
                sample-tree)
        sample-message)
; #t
```

### 2.69
```scheme
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (let ((fst  (car leaf-set))
            (snd  (cadr leaf-set))
            (rest (cddr leaf-set)))
           (successive-merge (adjoin-set (make-code-tree fst snd) rest))
      )
  ))
```

### 2.70
```scheme
(define (encode-70 xs) (encode xs (generate-huffman-tree songs)))
(define (encode-70-length xs) (length (encode-70 xs)))

; length of huffman
; 14 + 12 + 14 + 12 + 23 + 9 = 84

; length of fixed
; 36 * 3 = 108
```

### 2.71
```scheme
; Special case
; The most frequent symbol?
; 1

; The least frequent symbol?
; n - 1
```

### 2.72
```scheme 
; n个symbol: Θ(n)
; append: Θ(n) ~ Θ(1)
; element-in-list?: Θ(n) ~ Θ(1)
; 注意append和element-in-list?不是嵌套关系，而是顺序执行关系(复杂度相加而不是相乘)
; 最差情况是: Θ(n^2). 最好情况是：Θ(n)
```

### 2.73
```scheme
; a
; 将各种运算泛化（into the data-directed dispatch），number? 和 same-variable? 的模式和+，*不同，所以不能也一并泛化。

; b
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-sum-package)
  ;; internal procedures
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (addend operands) (car operands))
  (define (augend operands) (cadr operands))
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  ;; interface to the rest of the system
  (put 'deriv '(+) deriv-sum)
  'done)

(define (install-product-package)
  ;; internal procedures
  (define (make-product a1 a2) (list '* a1 a2))
  (define (multiplier operands) (car operands))
  (define (multiplicand operands) (cadr operands))
  (define (deriv-product operands var)
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))))

  ;; interface to the rest of the system
  (put 'deriv '(*) deriv-product)
  'done)

```

### 2.75
```scheme
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle)     a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)
```

### 2.76
对于增加新的类型（new type），这三种抽象都需要增加新类型的api实现。
此外：
* explicit dispatch: 所有暴露的api都要修改，考虑命名冲突
* data-directed style: 实现的同时顺便注册api
* message-passing-style: 无

message-passing-style win！

对于增加新的api，这三种抽象都需要增加所有类型的新api实现。
此外：
* explicit dispatch: 暴露新的api，考虑命名冲突
* data-directed style: 所有类型都要注册新api
* message-passing-style: 所有已存在的对象（部分调用的函数）需要重新生成才能应用新的api

data-directed style win！

### 2.77
每次成功的调用(`apply-generic` invoked)都会剥除掉匹配到的tag，并把内容传递给匹配到的函数。  
修改后的调用实际上跑了两遍`apply-generic`,  
第一次把内容传给了外层的`magnitude`,  
第二次把内容传给了包内的`magnitude`

### 2.78
```scheme
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)
  ))

(define (type-tag datum)
  (cond ((pair? datum)   (car datum))
        ((number? datum) `scheme-number)
        (else            (error "Bad tagged datum -- TYPE-TAG" datum))
  ))
(define (contents datum)
  (cond ((pair? datum)   (cdr datum))
        ((number? datum) datum)
        (else            (error "Bad tagged datum -- CONTENTS" datum))
  ))
```

### 2.79
```scheme
; install-scheme-number-package
(put 'equ? '(scheme-number scheme-number) =)

; install-rational-package
(put 'equ? '(rational rational)
  (lambda (x y) (and (= (numer x) (numer y))
                     (=  (denom x) (denom y)))
  ))

; install-complex-package
(put 'equ? '(complex complex)
  (lambda (x y) (and (= (real-part x) (real-part y))
                     (=  (imag-part x) (imag-part y)))
  ))
; others
(put 'equ? '(rational scheme-number)
  (lambda (x y) #f)
(put 'equ? '(complex scheme-number)
  (lambda (x y) #f)
(put 'equ? '(complex rational)
  (lambda (x y) #f)
; api
(define (equ? x y)
  (apply-generic 'equ? x y))
```

### 2.80
```scheme
; install-scheme-number-package
(put '=zero? 'scheme-number 
  (lambda (x) (= 0 x)))

; install-rational-package
(put '=zero? 'srational 
  (lambda (x) (= 0 (numer x))))

; install-complex-package
(put '=zero? 'srational 
  (lambda (x) (and (= 0 (real-part x))
                   (= 0 (imag-part x)))))
```

### 2.81
```scheme
; a
; will cause infinite loop since `apply-generic` call it self after coercion type
; b
; no. There nothing more need to correct.
; c
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                              (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                              (apply-generic op a1 (t2->t1 a2)))
                            (else
                              (error "No method for these types"
                                     (list op type-tags)))))))
              (error "No method for these types"
                    (list op type-tags)))))))
```

### 2.82
```scheme
; if the first argument is lower than other arguments in 'tower',
; 'lower' all other arguments down may fail
```

### 2.83
```scheme
; define
(define (integer->rational x)
  (make-rational x 1))
(define (rational->real x)
  (make-real-from-rational (numer x) (denom x)))
(define (real->complex x)
  (make-from-real-imag x 0))
; put
(put 'raise 'integer integer->rational)
(put 'raise 'rational rational->real)
(put 'raise 'real real->complex)
; generic
(define (raise x)
  (apply-generic 'raise x))
```

### 2.84
```scheme
(define (can-raise? in out)
  (if (equal? (type-tag in) (type-tag out))
      #t
      (if (get 'raise (type-tag in))
          (can-raise? (raise x) out)
          #f
      )
  ))
(define (raise-to in out)
  (if (equal? (type-tag in) (type-tag out))
      in
      (raise-to (raise in) out)
  ))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (if (can-raise? a1 a2)
                    (apply-generic op (raise-to a1 a2) a2)
                    (apply-generic op a1 (raise-to a2 a1))))
              (error "No method for these types"
                     (list op type-tags)))))))
```

### 2.85
```scheme
(define (complex->real x)
  (make-real (real-part (contents x))))
(define (real->rational x)
  (let ((rational (inexact->exact (contents x))))
       (make-rational (numerator rational)
                      (denominator rational))))
(define (rational->integer x)
  (make-scheme-number (round (/ (numer (contents x)) (denom (contents x))))))

(put 'project 'complex complex->real)
(put 'project 'real real->rational)
(put 'project 'rational rational->integer)

(define (project x)
  (apply-generic 'project x))

(define (drop x)
  (define (can-drop? i) (equ? (raise (project i)) i))
  (if (can-drop? x)
      (drop (project x))
      x
  ))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (drop (car args)))
                    (a2 (drop (cadr args))))
                (if (can-raise? a1 a2)
                    (apply-generic op (raise-to a1 a2) a2)
                    (apply-generic op a1 (raise-to a2 a1))))
              (error "No method for these types"
                     (list op type-tags)))))))
```

### 2.87
```scheme
(put '=zero? 'polynomial (lambda (L) (
  = (length (filter (lambda (t) (not (=zero? (coeff t))))
                    (term-list L)))
    0)))
; filter 默认term-list为list
(put '=zero? 'polynomial (lambda (L) (
  (define (=zero-terms? xs) (
    if (empty-termlist? xs)
        #t
       (and (=zero? (coeff (first-term xs)))
            (=zero-terms? (rest-terms xs)))
    ))
  (=zero-terms? (term-list L))
)))
```

### 2.88
```scheme
(define (sub-poly L1 L2) (add-poly L1 (negate L2)))

(put 'negate 'scheme-number (lambda (x) (- x)))
(put 'negate 'rational (lambda (x) (make-rational (- (numer x)) (denom x))))
(put 'negate 'complex (lambda (x) (make-from-real-imag (- (real-part x)) (- (imag-part x)))))
(put 'negate 'polynomial (lambda (x) (make-poly (variable x) (negate-terms (term-list x)))))

(define (negate-terms ts)
  (let ((fst (first-term ts)))
       (if (empty-termlist? ts)
         ts
         (adjoin-term (make-term (order fst) (negate (coeff fst)))
                      (rest-terms ts)
         ))
  ))

(define (negate x)
  (apply-generic 'negate x))
```

### 2.89
```scheme
(define (adjoin-term term term-list)
  (let ((fst (first-term term-list))
        (rest (rest-terms term-list)))
       (cond ((=zero? (coeff term)) term-list)
             ((= (order term) (order fst)) (cons (+ (coeff term) (coeff fst)) rest))
             ((> (order term) (order fst)) (adjoin-term term (cons 0 term-list)))
             (else                         (cons (coeff fst) (adjoin-term term rest)))
  )))
(define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))
```

