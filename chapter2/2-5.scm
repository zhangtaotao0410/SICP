(define expt
    (lambda (a b)
      (cond ((= b 0) 1)
            (else (* a (expt a (- b 1)))))))

(define (cons* x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car* z)
  (cond ((= 0 (remainder z 2)) (+ 1 (car* (/ z 2))))
        (else 0)))
(define (cdr* z)
  (cond ((= 0 (remainder z 3)) (+ 1 (cdr* (/ z 3))))
        (else 0)))
(define a (cons* 4 5))
(cdr* a)
