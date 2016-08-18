(define (square x) (* x x))
(define (sum-of-square x y) (+ (square x) (square y)))

(define (sqrt-item guess x)
        (if (good-enough? guess x)
            guess
            (sqrt-item (improve guess x) x)))

(define (good-enough? guess x)
        (< (abs (- (square guess) x)) 0.01))
(define (improve guess x)
        (average guess (/ x guess)))
(define (average x y)
        (/ (+ x y) 2))
(define sqrt
    (lambda (x)
      (sqrt-item 1.0 x)))
;(sqrt 10)
;1.2.2树形递归
;菲波那切数列 递归计算过程
(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1)) (fib (- n 2))))))
;迭代计算过程
(define fib1
    (lambda (n)
      (fib-iter 1 0 n)))
(define fib-iter
    (lambda (a b count)
        (cond ((= count 0) b)
              (else (fib-iter (+ a b) a (- count 1))))))
;(fib 15)
;换零钱方式
;现金为a
(define (count-chang amount)
    (cc amount 5))

(define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((< amount 0) 0)
          ((= kinds-of-coins 0) 0)
          (else (+ (cc amount (- kinds-of-coins 1))
                   (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 5) 50)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 1) 1)))
;最大公约数 GCD
(define (gcd n m)
  (cond ((= m 0) n)
        (else (gcd m (remainder n m)))))


;确定是不是素数
(define (prime? n)
  (= n (smallest-divisor n)))
(define smallest-divisor
    (lambda (n)
      (find-divisor n 2)))
(define find-divisor
    (lambda (n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1))))))

(define divides?
    (lambda (n m)
      (= (remainder m n) 0)))

;1.3.1
;从a到b各整数合
(define (sum-integers a b)
  (cond ((> a b) 0)
        (else (+ a (sum-integers (add1 a) b)))))
(define add1
    (lambda (n)
      (+ n 1)))
(define sub1
    (lambda (n)
      (- n 1)))
;从a到b各整数立方之和
(define (sum-cube a b)
  (cond ((> a b) 0)
        (else (+ (cube a) (sum-cube (add1 a) b)))))
(define cube
    (lambda (n)
      (* n n n)))

(define (pi-sum a b)
  (cond ((> a b) 0)
        (else (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b)))))

(define sum
    (lambda (term next a b)
        (cond ((> a b) 0)
              (else (+ (term a) (sum term next (next a) b))))))
;let表达式 (let ((<var1> <exp1>) (<var2> <exp2>)) <body>)
;((lambda (<var1> <var2>) <body>) <exp1> <exp2>)

;通过区间折半寻找方程的根
(define search
    (lambda (f neg-point pos-point)
      (let ((mid-point (average neg-point pos-point)))
           (if (close-enough? neg-point pos-point)
               mid-point
               (let ((test-point (f mid-point)))
                    (cond ((positive? test-point) (search f neg-point mid-point))
                          ((negative? test-point) (search f mid-point pos-point))
                          (else mid-point)))))))
(define close-enough?
    (lambda (n m) (< (abs (- n m)) 0.001)))

(define half-method
    (lambda (f a b)
      (let ((a-value (f a))
            (b-value (f b)))
           (cond ((and (positive? a-value) (negative? b-value))
                  (search f b a))
                 ((and (negative? a-value) (positive? b-value))
                  (search f a b))
                 (else (error "Values are not of oppos]ite sign" a b))))))

;找出函数的不动点
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
         (if (close-enough? next guess)
             next
             (try next))))
  (try first-guess))

;牛顿法,导数表示
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.0000001)
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))
(define (sqrt1 x)
  (newton-method (lambda (a) (- (square a) x)) 1.0))
(sqrt1 9)
