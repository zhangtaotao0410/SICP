(define sum-odd-square
    (lambda (tree)
      (cond ((null? tree) 0)
            ((atom? (car tree)) (if (odd? (car tree))
                                    (+ (square (car tree)) (sum-odd-square (cdr tree)))
                                    (sum-odd-square (cdr tree))))

            (else (+ (sum-odd-square (car tree))
                     (sum-odd-square (cdr tree)))))))

;映射过程
(define map
    (lambda (pro l)
      (cond ((null? l) (quote ()))
            (else (cons (pro (car l)) (map pro (cdr l)))))))
;过滤过程
(define filter
    (lambda (predicate l)
        (cond ((null? l) (quote ()))
              (else (cond ((predicate (car l)) (cons (car l) (filter predicate (cdr l))))
                          (else (filter predicate (cdr l))))))))

(filter odd? (list 1 2 3 4 5))
