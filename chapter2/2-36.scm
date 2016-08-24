;(define (accumulate-n op initial tree)
;  (cond ((null? (car tree)) (quote ()))
;        (else (cons (accumulate op
;                                initial
;                                (car-n tree))
;                    (accumulate-n op initial (cdr-n tree))))))

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (car-n seqs))
              (accumulate-n op init (cdr-n seqs)))))

(define (accumulate op initial lst)
  (cond ((null? lst) initial)
        (else (op (car lst) (accumulate op initial (cdr lst))))))
(define map
    (lambda (pro list)
      (cond ((null? list) (quote ()))
            (else (cons (pro (car list)) (map pro (cdr list)))))))
(define (car-n seqs)
  (map car seqs))

(define (cdr-n seqs)
  (map cdr seqs))

(define s (list (list 1 2 3)
                        (list 4 5 6)
                        (list 7 8 9)
                        (list 10 11 12)))

(accumulate-n + 0 s)
