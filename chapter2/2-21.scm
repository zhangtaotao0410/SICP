;(define (square-list list)
;  (if (null? list)
;      (quote ())
;      (cons (square (car list)) (square-list (cdr list)))))
;
;(define (square-list list)
;  (map square list))
(define (reverse list)
  (define (reverse-item list result)
    (if (null? list)
        result
        (reverse-item (cdr list) (cons (car list) result))))
  (reverse-item list (quote ())))

(define (square-list l)
  (define (item list result)
    (if (null? list)
        (reverse result)
        (item (cdr list)
              (cons (square (car list)) result))))
  (item l (quote ())))

(square-list (list 1 2 3 4 5))
