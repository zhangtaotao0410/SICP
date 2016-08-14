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
(sqrt 10)
