(define (for-each list pro)
  (if (not (null? list))
      (begin
        (pro (car list))
        (for-each (cdr list) pro))))

(for-each (list 1 2 3 4) (lambda (x) (newline) (display x)))
