(define (fringe tree)
    (cond ((null? tree) (quote ()))
          ((atom? (car tree)) (cons (car tree) (fringe (cdr tree))))
          (else (append (fringe (car tree)) (fringe (cdr tree))))))

(fringe (list (list 1 2) (list 3 4) (list 5 (list 6 7))))
