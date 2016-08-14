(load "chapter1.scm")

(define cube-root-item
    (lambda (guess x)
        (cond ((good-enough? guess x) guess)
              (else (cube-root-item (improve guess x) x)))))
(define good-enough?
    (lambda (guess x)
        (< (abs (- (improve guess x) guess)) (* guess 0.001))))
(define improve
    (lambda (guess x)
        (/ (+ (/ x (square guess)) (* 2 guess)) 3)))
(define cube-root
    (lambda (x)
        (cube-root-item 1.0 x)))

(cube-root 27)
