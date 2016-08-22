(define atom?
    (lambda (x)
      (and (not (null? x)) (not (pair? x)))))

(define (cons* x y)
  (define dispatch
      (lambda (m)
        (cond ((= 0 m) x)
              (else y))))
  dispatch)
(define (car* x) (x 0))
(define (cdr* x) (x 1))

(define a (cons* 3 4))
;(define length
;    (lambda (l)
;      (cond ((null? l) 0)
;            (else (+ 1 (length (cdr l)))))))
(define (length l)
  (define (length-help a l)
      (cond ((null? l) a)
            (else (length-help (+ 1 a) (cdr l)))))
  (length-help 0 l))

(define append
    (lambda (l1 l2)
      (cond ((null? l1) l2)
            (else (cons (car l1) (append (cdr l1) l2))))))

(define l1 (list 1 2 3 4))
(define l2 (list 5 6 7 8))
;---映射
(define map
    (lambda (pro list)
      (cond ((null? list) (quote ()))
            (else (cons (pro (car list)) (map pro (cdr list)))))))
(define count-leaves
    (lambda (x)
      (cond ((null? x) 0)
            ((not (pair? x)) 1)
            (else (+ (count-leaves (car x))
                     (count-leaves (cdr x)))))))
(count-leaves (list 1 2 3 (list 1 2 3 (list 1 2 (list 1)))))
