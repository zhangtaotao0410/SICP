
(define atom?
    (lambda (x)
      (and (not (null? x)) (not (pair? x)))))
(define (deep-reverse lst)
  (define (deep-reverse-help x)
    (if (atom? x)
        x
        (deep-reverse x)))
  (define (deep-reverse-item list result)
    (if (null? list)
        result
        (deep-reverse-item (cdr list)
                           (cons (deep-reverse-help (car list)) result))))
  (deep-reverse-item lst (quote ())))
(deep-reverse (list 1 2 3 (list 1 2 3) (list 1 2 3 (list 1 2 3) 4 (list 1 2 (list 1 2 (list 1 2))))))
