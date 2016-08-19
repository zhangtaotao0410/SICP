(define make-rat
    (lambda (n m)
      (cond ((< m 0) (cons (- n) (- m)))
            (else (cons n m)))))

(define a (make-rat 7 -3))
a
