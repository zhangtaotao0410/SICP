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

(sum-odd-square (list 1 2 (list 1 2 (list 1 2) 1) 3))
