(define (same-parity a . w)
  (cond ((null? w) (quote ()))
        (else (filter (if (even? a)
                          even?
                          odd?)
                          (cons a w)))))

(define (filter predicate l)
    (cond ((null? l) (quote ()))
          ((predicate (car l)) (cons (car l) (filter predicate (cdr l))))
          (else (filter predicate (cdr l)))))
(same-parity 1 2 3 4 5 6 7 10)
