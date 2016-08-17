(define fast-*
    (lambda (a b)
      (fast-item 0 a b)))

(define fast-item
    (lambda (n a b)
      (cond ((= b 0) n)
            ((even? b) (fast-item n (double a) (halve b)))
            (else (fast-item (+ n a) a (- b 1))))))
(define halve
    (lambda (n)
      (/ n 2)))

(define double
    (lambda (n)
      (* n 2)))
(fast-* 4 5)
