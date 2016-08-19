(define make-point
    (lambda (x y)
      (cons x y)))

(define x-point
    (lambda (point)
      (car point)))

(define y-point
    (lambda (point)
      (cdr point)))

(define make-segment
    (lambda (p1 p2)
      (cons p1 p2)))

(define start-segment
    (lambda (segment)
      (car segment)))

(define end-segment
    (lambda (segment)
      (cdr segment)))
(define average
    (lambda (n m)
      (/ (+ n m) 2)))
(define mid-segment
    (lambda (segment)
      (make-point (average (car (start-segment segment))
                           (car (end-segment segment)))
                  (average (cdr (start-segment segment))
                           (cdr (end-segment segment))))))
(define print-p
    (lambda (point)
      (newline)
      (display "(")
      (display (x-point point))
      (display ",")
      (display (y-point point))
      (display ")")))

(define a (make-point 0 0))
(define b (make-point 2 2))
(define seg (make-segment a b))
(print-p (mid-segment seg))
