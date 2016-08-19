(define reverse
    (lambda (l)
      (reverse-help l (quote ()))))

(define reverse-help
    (lambda (l result)
      (cond ((null? l) result)
            (else (reverse-help (cdr l) (cons (car l) result))))))

(define l (list 1 2 3 4))
(reverse l)
