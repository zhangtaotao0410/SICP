(define (for-each list pro)
  (if (not (null? list))
      (begin
        (pro (car list))
        (for-each (cdr list) pro))))

(define (for-each lst p)
    (cond ((not (null? lst))
            (p (car lst))
            (for-each p (cdr lst)))))

(for-each (list 1 2 3 4) (lambda (x) (newline) (display x)))
