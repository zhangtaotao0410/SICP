(define (sqrt-item guess x)
        (if (good-enough? guess x)
            guess
            (sqrt-item (improve guess x) x)))

(define (good-enough? guess x)
        (< (abs (- (improve guess x) guess)) (* guess 0.001)))
(define (improve guess x)
        (average guess (/ x guess)))
(define (average x y)
        (/ (+ x y) 2))
(define (sqrt x)
        (sqrt-item 1.0 x))
(sqrt 100000000000000000000.0)
