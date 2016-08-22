(define subsets
    (lambda (lst)
      (if (null? lst)
          (list (quote ()))
          (let ((res (subsets (cdr lst))))
               (append res (map (lambda (x) (cons (car lst) x)) res))))))
(subsets (list 1 2 3))
