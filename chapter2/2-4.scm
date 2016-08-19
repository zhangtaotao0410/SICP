(define (cons* x y)
  (lambda (m) (m x y)))

(define (car* z)
  (z (lambda (a b) a)))

(car* (cons* x y))
((cons* x y) (lambda (a b) a))
((lambda (m) (m x y)) (lambda (a b) a))
((lambda (a b) a) x y)
x
(define (cdr* z)
  (z (lambda (a b) b)))
