(define last-pair
    (lambda (l)
      (cond ((null? l) (error "list is empty"))
            ((null? (cdr l)) l)
            (else (last-pair (cdr l))))))

(define l (list 1 2 3 4))
(last-pair l)
