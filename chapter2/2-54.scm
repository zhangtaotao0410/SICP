(define equal?
    (lambda (a b)
      (cond ((and (atom? a) (atom? b)) (eq? a b))
            ((or (atom? a) (atom? b)) #f)
            ((and (null? a) (null? b)) #t)
            (else (and (equal? (car a) (car b))
                       (equal? (cdr a) (cdr b)))))))

(define atom?
    (lambda (x)
      (and (not (null? x)) (not (pair? x)))))

(equal? (list 'a 'b) (list 'a 'b))
