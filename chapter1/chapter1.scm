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


(count-chang 100)
